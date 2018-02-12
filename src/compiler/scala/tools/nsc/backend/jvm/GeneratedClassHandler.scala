package scala.tools.nsc
package backend.jvm

import java.util.concurrent.ThreadPoolExecutor.CallerRunsPolicy
import java.util.concurrent._

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future, Promise}
import scala.reflect.internal.util.{NoPosition, Position, SourceFile}
import scala.tools.nsc.backend.jvm.PostProcessorFrontendAccess.BackendReporting
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.profile.ThreadPoolFactory
import scala.util.control.NonFatal

/**
 * Interface to handle post-processing (see [[PostProcessor]]) and classfile writing of generated
 * classes, potentially in parallel.
 */
private[jvm] sealed trait GeneratedClassHandler {
  val postProcessor: PostProcessor

  /**
    * Pass the result of code generation for a compilation unit to this handler for post-processing
    */
  def process(unit: GeneratedCompilationUnit)

  /**
   * If running in parallel, block until all generated classes are handled
   */
  def complete(): Unit

  /**
    * Invoked at the end of the jvm phase
    */
  def close(): Unit
}

private[jvm] object GeneratedClassHandler {
  def apply(global: Global): GeneratedClassHandler = {
    import global._
    import genBCode.postProcessor

    val cfWriter = ClassfileWriter(global)

    val unitInfoLookup = settings.outputDirs.getSingleOutput match {
      case Some(dir) => new SingleUnitInfo(postProcessor.bTypes.frontendAccess, dir)
      case None => new LookupUnitInfo(postProcessor.bTypes.frontendAccess)
    }
    val handler = settings.YaddBackendThreads.value match {
      case 1 =>
        new SyncWritingClassHandler(unitInfoLookup, postProcessor, cfWriter)

      case maxThreads =>
        if (global.statistics.enabled)
          global.reporter.warning(global.NoPosition, "jvm statistics are not reliable with multi-threaded jvm class writing")
        val additionalThreads = maxThreads -1
        // the queue size is taken to be large enough to ensure that the a 'CallerRun' will not take longer to
        // run that it takes to exhaust the queue for the backend workers
        // when the queue is full, the main thread will no some background work
        // so this provides back-pressure
        val queueSize = if (settings.YmaxQueue.isSetByUser) settings.YmaxQueue.value else maxThreads * 2
        val threadPoolFactory = ThreadPoolFactory(global, currentRun.jvmPhase)
        val javaExecutor = threadPoolFactory.newBoundedQueueFixedThreadPool(additionalThreads, queueSize, new CallerRunsPolicy, "non-ast")
        val execInfo = ExecutorServiceInfo(additionalThreads, javaExecutor, javaExecutor.getQueue)
        new AsyncWritingClassHandler(unitInfoLookup, postProcessor, cfWriter, execInfo)
    }

    if (settings.optInlinerEnabled || settings.optClosureInvocations)
      new GlobalOptimisingGeneratedClassHandler(postProcessor, handler)
    else handler
  }

  private class GlobalOptimisingGeneratedClassHandler(
      val postProcessor: PostProcessor,
      underlying: WritingClassHandler)
    extends GeneratedClassHandler {

    private val generatedUnits = ListBuffer.empty[GeneratedCompilationUnit]

    def process(unit: GeneratedCompilationUnit): Unit = generatedUnits += unit

    def complete(): Unit = {
      val allGeneratedUnits = generatedUnits.result()
      generatedUnits.clear()
      postProcessor.runGlobalOptimizations(allGeneratedUnits)
      allGeneratedUnits.foreach(underlying.process)
      underlying.complete()
    }

    def close(): Unit = underlying.close()

    override def toString: String = s"GloballyOptimising[$underlying]"
  }

  sealed abstract class WritingClassHandler(val javaExecutor: Executor) extends GeneratedClassHandler {
    val unitInfoLookup: UnitInfoLookup
    val cfWriter: ClassfileWriter

    def tryStealing: Option[Runnable]

    private val processingUnits = ListBuffer.empty[UnitResult]

    def process(unit: GeneratedCompilationUnit): Unit = {
      val unitProcess = new UnitResult(unitInfoLookup, unit.classes, unit.sourceFile)
      postProcessUnit(unitProcess)
      processingUnits += unitProcess
    }

    protected implicit val executionContext: ExecutionContextExecutor = ExecutionContext.fromExecutor(javaExecutor)

    final def postProcessUnit(unitProcess: UnitResult): Unit = {
      unitProcess.task = Future {
        unitProcess.withBufferedReporter {
          // we 'take' classes to reduce the memory pressure
          // as soon as the class is consumed and written, we release its data
          unitProcess.takeClasses foreach {
            postProcessor.sendToDisk(unitProcess, _, cfWriter)
          }
        }
      }
    }

    protected def getAndClearProcessingUnits(): List[UnitResult] = {
      val result = processingUnits.result()
      processingUnits.clear()
      result
    }

    override def complete(): Unit = {
      val directBackendReporting = postProcessor.bTypes.frontendAccess.directBackendReporting

      def stealWhileWaiting(unitResult: UnitResult, fut: Future[Unit]): Unit = {
        while (!fut.isCompleted)
          tryStealing match {
            case Some(r) => r.run()
            case None => Await.ready(fut, Duration.Inf)
        }
        //we know that they are complete by we need to check for exception
        //but first get any reports
        unitResult.relayReports(directBackendReporting)
        fut.value.get.get // throw the exception if the future completed with a failure
      }


      /** We could consume the results when yey are ready, via use of a [[java.util.concurrent.CompletionService]]
        * or something similar, but that would lead to non deterministic reports from backend threads, as the
        * compilation unit could complete in a different order that when they were submitted, and thus the relayed
        * reports would be in a different order.
        * To avoid that non-determinism we read the result in order or submission, with a potential minimal performance
        * loss, do to the memory being retained longer for tasks that it might otherwise.
        * Most of the memory in the UnitResult is reclaimable anyway as the classes are deferenced after use
        */
      getAndClearProcessingUnits().foreach { unitResult =>
        try {
          stealWhileWaiting(unitResult, unitResult.task)
        } catch {
          case NonFatal(t) =>
            t.printStackTrace()
            postProcessor.bTypes.frontendAccess.backendReporting.error(NoPosition, s"unable to write ${unitResult.sourceFile} $t")
        }
      }
    }

    def close(): Unit = cfWriter.close()
  }

  private final class SyncWritingClassHandler(
      val unitInfoLookup: UnitInfoLookup,
      val postProcessor: PostProcessor,
      val cfWriter: ClassfileWriter)
    extends WritingClassHandler((r) => r.run()) {

    override def toString: String = s"SyncWriting [$cfWriter]"

    override def tryStealing: Option[Runnable] = None
  }

  private final case class ExecutorServiceInfo(maxThreads: Int, javaExecutor: ExecutorService, queue: BlockingQueue[Runnable])

  private final class AsyncWritingClassHandler(val unitInfoLookup: UnitInfoLookup,
                                               val postProcessor: PostProcessor,
                                               val cfWriter: ClassfileWriter,
                                               val executorServiceInfo: ExecutorServiceInfo)
    extends WritingClassHandler(executorServiceInfo.javaExecutor) {

    override def toString: String = s"AsyncWriting[additional threads:${executorServiceInfo.maxThreads} writer:$cfWriter]"

    override def close(): Unit = {
      super.close()
      executorServiceInfo.javaExecutor.shutdownNow()
    }

    override def tryStealing: Option[Runnable] = Option(executorServiceInfo.queue.poll())
  }

}
//we avoid the lock on frontendSync for the common case, when compiling to a single target
sealed trait UnitInfoLookup {
  def outputDir(source:AbstractFile) : AbstractFile
  val frontendAccess: PostProcessorFrontendAccess
}
final class SingleUnitInfo(val frontendAccess: PostProcessorFrontendAccess, constantOutputDir:AbstractFile) extends UnitInfoLookup {
  override def outputDir(source: AbstractFile) = constantOutputDir
}
final class LookupUnitInfo(val frontendAccess: PostProcessorFrontendAccess) extends UnitInfoLookup {
  lazy val outputDirectories = frontendAccess.compilerSettings.outputDirectories
  override def outputDir(source: AbstractFile) = outputDirectories.outputDirFor(source)
}
sealed trait SourceUnit {
  def withBufferedReporter[T](fn: => T): T

  val outputDir: AbstractFile
  val outputPath: java.nio.file.Path
  def sourceFile:AbstractFile
}

final class UnitResult(unitInfoLookup: UnitInfoLookup, _classes : List[GeneratedClass], val sourceFile: AbstractFile) extends SourceUnit with BackendReporting {
  lazy val outputDir = unitInfoLookup.outputDir(sourceFile)
  lazy val outputPath = outputDir.file.toPath

  private var classes: List[GeneratedClass] = _classes

  def copyClasses = classes

  def takeClasses(): List[GeneratedClass] = {
    val c = classes
    classes = Nil
    c
  }

  /** the main async task submitted onto the scheduler */
  var task: Future[Unit] = _

  def relayReports(backendReporting: BackendReporting): Unit = this.synchronized {
    if (bufferedReports nonEmpty) {
      for (report: Report <- bufferedReports.reverse) {
        report.relay(backendReporting)
      }
    }
    bufferedReports = Nil
  }

  // We optimise access to the buffered reports for the common case - that there are no warning/errors to report
  // We could use a listBuffer etc - but that would be extra allocation in the common case
  // Note - all access is externally synchronized, as this allow the reports to be generated in on thread and
  // consumed in another
  private var bufferedReports = List.empty[Report]

  override def withBufferedReporter[T](fn: => T) = unitInfoLookup.frontendAccess.withLocalReporter(this)(fn)

  override def inlinerWarning(pos: Position, message: String): Unit =
    this.synchronized(bufferedReports ::= new ReportInlinerWarning(pos, message))

  override def error(pos: Position, message: String): Unit =
    this.synchronized(bufferedReports ::= new ReportError(pos, message))

  override def warning(pos: Position, message: String): Unit =
    this.synchronized(bufferedReports ::= new ReportWarning(pos, message))

  override def inform(message: String): Unit =
    this.synchronized(bufferedReports ::= new ReportInform(message))

  override def log(message: String): Unit =
    this.synchronized(bufferedReports ::= new ReportLog(message))

  private sealed trait Report {
    def relay(backendReporting: BackendReporting): Unit
  }

  private class ReportInlinerWarning(pos: Position, message: String) extends Report {
    override def relay(reporting: BackendReporting): Unit =
      reporting.inlinerWarning(pos, message)
  }

  private class ReportError(pos: Position, message: String) extends Report {
    override def relay(reporting: BackendReporting): Unit =
      reporting.error(pos, message)
  }

  private class ReportWarning(pos: Position, message: String) extends Report {
    override def relay(reporting: BackendReporting): Unit =
      reporting.warning(pos, message)
  }

  private class ReportInform(message: String) extends Report {
    override def relay(reporting: BackendReporting): Unit =
      reporting.inform(message)
  }

  private class ReportLog(message: String) extends Report {
    override def relay(reporting: BackendReporting): Unit =
      reporting.log(message)
  }
}
