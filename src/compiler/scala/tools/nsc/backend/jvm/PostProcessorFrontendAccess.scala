package scala.tools.nsc
package backend.jvm

import scala.collection.generic.Clearable
import scala.reflect.internal.util.{JavaClearable, Position, Statistics}
import scala.reflect.io.AbstractFile
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import java.util.{Collection => JCollection, Map => JMap}

/**
 * Functionality needed in the post-processor whose implementation depends on the compiler
 * frontend. All methods are synchronized.
 */
sealed abstract class PostProcessorFrontendAccess {
  import PostProcessorFrontendAccess._

  def initialize(): Unit

  final val frontendLock: AnyRef = new Object()
  @inline final def frontendSynch[T](x: => T): T = frontendLock.synchronized(x)

  def compilerSettings: CompilerSettings

  def withThreadLocalReporter[T](reporter: BackendReporting)(fn: => T): T
  def backendReporting: BackendReporting
  def directBackendReporting: BackendReporting

  /**
   * Statistics are not thread-safe, they can only be used if `compilerSettings.backendThreads == 1`
   */
  def unsafeStatistics: Statistics with BackendStats

  def backendClassPath: BackendClassPath

  def getEntryPoints: List[String]

  def javaDefinedClasses: Set[InternalName]

  def recordPerRunCache[T <: Clearable](cache: T): T

  def recordPerRunJavaMapCache[T <: JMap[_,_]](cache: T): T

  def recordPerRunJavaCache[T <: JCollection[_]](cache: T): T
}

object PostProcessorFrontendAccess {
  sealed trait CompilerSettings {
    def debug: Boolean

    def target: String

    def outputDirectory(source: AbstractFile): AbstractFile

    def optAddToBytecodeRepository: Boolean
    def optBuildCallGraph: Boolean

    def optNone: Boolean
    def optLClasspath: Boolean
    def optLProject: Boolean

    def optUnreachableCode: Boolean
    def optNullnessTracking: Boolean
    def optBoxUnbox: Boolean
    def optCopyPropagation: Boolean
    def optRedundantCasts: Boolean
    def optSimplifyJumps: Boolean
    def optCompactLocals: Boolean
    def optClosureInvocations: Boolean

    def optInlinerEnabled: Boolean
    def optInlineFrom: List[String]
    def optInlineHeuristics: String

    def optWarningNoInlineMixed: Boolean
    def optWarningNoInlineMissingBytecode: Boolean
    def optWarningNoInlineMissingScalaInlineInfoAttr: Boolean
    def optWarningEmitAtInlineFailed: Boolean
    def optWarningEmitAnyInlineFailed: Boolean

    def optLogInline: Option[String]
    def optTrace: Option[String]
  }

  trait BackendReporting {
    def inlinerWarning(pos: Position, message: String): Unit
    def error(pos: Position, message: String): Unit
    def warning(pos: Position, message: String): Unit
    def inform(message: String): Unit
    def log(message: String): Unit
  }

  final class BufferingBackendReporting extends BackendReporting {
    // We optimise access to the buffered reports for the common case - that there are no warning/errors to report
    // We could use a listBuffer etc - but that would be extra allocation in the common case
    // Note - all access is externally synchronized, as this allow the reports to be generated in on thread and
    // consumed in another
    private var bufferedReports = List.empty[Report]

    def inlinerWarning(pos: Position, message: String): Unit =
      this.synchronized(bufferedReports ::= new ReportInlinerWarning(pos, message))

    def error(pos: Position, message: String): Unit =
      this.synchronized(bufferedReports ::= new ReportError(pos, message))

    def warning(pos: Position, message: String): Unit =
      this.synchronized(bufferedReports ::= new ReportWarning(pos, message))

    def inform(message: String): Unit =
      this.synchronized(bufferedReports ::= new ReportInform(message))

    def log(message: String): Unit =
      this.synchronized(bufferedReports ::= new ReportLog(message))

    def relayReports(toReporting: BackendReporting): Unit = this.synchronized {
      if (bufferedReports.nonEmpty) {
        bufferedReports.reverse.foreach(_.relay(toReporting))
        bufferedReports = Nil
      }
    }

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

  sealed trait BackendClassPath {
    def findClassFile(className: String): Option[AbstractFile]
  }

  class PostProcessorFrontendAccessImpl(val global: Global) extends PostProcessorFrontendAccess with PerRunInit {
    import global._
    import genBCode.bTypes.{LazyVar, perRunLazy}

    private[this] lazy val _compilerSettings: LazyVar[CompilerSettings] = perRunLazy(this)(buildCompilerSettings())

    def compilerSettings: CompilerSettings = _compilerSettings.get

    private def buildCompilerSettings(): CompilerSettings = new CompilerSettings {
      import global.{settings => s}

      val debug: Boolean = s.debug

      val target: String = s.target.value

      private val singleOutDir = s.outputDirs.getSingleOutput
      // the call to `outputDirFor` should be frontendSynch'd, but we assume that the setting is not mutated during the backend
      def outputDirectory(source: AbstractFile): AbstractFile = singleOutDir.getOrElse(s.outputDirs.outputDirFor(source))

      val optAddToBytecodeRepository: Boolean = s.optAddToBytecodeRepository
      val optBuildCallGraph: Boolean = s.optBuildCallGraph

      val optNone: Boolean = s.optNone
      val optLClasspath: Boolean = s.optLClasspath
      val optLProject: Boolean = s.optLProject

      val optUnreachableCode: Boolean = s.optUnreachableCode
      val optNullnessTracking: Boolean = s.optNullnessTracking
      val optBoxUnbox: Boolean = s.optBoxUnbox
      val optCopyPropagation: Boolean = s.optCopyPropagation
      val optRedundantCasts: Boolean = s.optRedundantCasts
      val optSimplifyJumps: Boolean = s.optSimplifyJumps
      val optCompactLocals: Boolean = s.optCompactLocals
      val optClosureInvocations: Boolean = s.optClosureInvocations

      val optInlinerEnabled: Boolean = s.optInlinerEnabled
      val optInlineFrom: List[String] = s.optInlineFrom.value
      val optInlineHeuristics: String = s.YoptInlineHeuristics.value

      val optWarningNoInlineMixed: Boolean = s.optWarningNoInlineMixed
      val optWarningNoInlineMissingBytecode: Boolean = s.optWarningNoInlineMissingBytecode
      val optWarningNoInlineMissingScalaInlineInfoAttr: Boolean = s.optWarningNoInlineMissingScalaInlineInfoAttr
      val optWarningEmitAtInlineFailed: Boolean = s.optWarningEmitAtInlineFailed
      val optWarningEmitAnyInlineFailed: Boolean = {
        val z = s // `s` is a def, but need a stable path, the argument type of `contains` is path-dependent
        z.optWarnings.contains(z.optWarningsChoices.anyInlineFailed)
      }

      val optLogInline: Option[String] = s.YoptLogInline.valueSetByUser
      val optTrace: Option[String] = s.YoptTrace.valueSetByUser
    }

    private lazy val localReporter = perRunLazy(this)(new ThreadLocal[BackendReporting])

    override def withThreadLocalReporter[T](reporter: BackendReporting)(fn: => T): T = {
      val threadLocal = localReporter.get
      val old = threadLocal.get()
      threadLocal.set(reporter)
      try fn finally
        if (old eq null) threadLocal.remove() else threadLocal.set(old)
    }

    override def backendReporting: BackendReporting = {
      val local = localReporter.get.get()
      if (local eq null) directBackendReporting else local
    }

    object directBackendReporting extends BackendReporting {
      def inlinerWarning(pos: Position, message: String): Unit = frontendSynch {
        currentRun.reporting.inlinerWarning(pos, message)
      }

      def error(pos: Position, message: String): Unit = frontendSynch {
        reporter.error(pos, message)
      }

      def warning(pos: Position, message: String): Unit = frontendSynch {
        global.warning(pos, message)
      }

      def inform(message: String): Unit = frontendSynch {
        global.inform(message)
      }

      def log(message: String): Unit = frontendSynch {
        global.log(message)
      }
    }
    def unsafeStatistics: Statistics with BackendStats = global.statistics

    private lazy val cp = perRunLazy(this)(frontendSynch(optimizerClassPath(classPath)))
    object backendClassPath extends BackendClassPath {
      def findClassFile(className: String): Option[AbstractFile] = cp.get.findClassFile(className)
    }

    def getEntryPoints: List[String] = frontendSynch(cleanup.getEntryPoints)

    def javaDefinedClasses: Set[InternalName] = frontendSynch {
      currentRun.symSource.keys.collect{
        case sym if sym.isJavaDefined => sym.javaBinaryNameString
      }(scala.collection.breakOut)
    }


    def recordPerRunCache[T <: Clearable](cache: T): T = frontendSynch(perRunCaches.recordCache(cache))

    def recordPerRunJavaMapCache[T <: JMap[_,_]](cache: T): T = {
      recordPerRunCache(JavaClearable.forMap(cache))
      cache
    }

    def recordPerRunJavaCache[T <: JCollection[_]](cache: T): T = {
      recordPerRunCache(JavaClearable.forCollection(cache))
      cache
    }
  }
}
