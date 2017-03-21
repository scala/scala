/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala
package tools.nsc
package backend
package jvm

import scala.collection.mutable
import scala.reflect.internal.util.Statistics
import scala.tools.asm
import scala.tools.asm.tree.ClassNode
import scala.tools.nsc.backend.jvm.opt.ByteCodeRepository
import java.util.concurrent.BlockingQueue

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.control.NonFatal
import java.util.{concurrent => juc}

import scala.tools.nsc.io.AbstractFile

/*
 *  Prepare in-memory representations of classfiles using the ASM Tree API, and serialize them to disk.
 *
 *  Three pipelines are at work, each taking work items from a queue dedicated to that pipeline:
 *
 *  (There's another pipeline so to speak, the one that populates queue-1 by traversing a CompilationUnit until ClassDefs are found,
 *   but the "interesting" pipelines are the ones described below)
 *
 *    (1) In the first queue, an item consists of a ClassDef along with its arrival position.
 *        This position is needed at the time classfiles are serialized to disk,
 *        so as to emit classfiles in the same order CleanUp handed them over.
 *        As a result, two runs of the compiler on the same files produce jars that are identical on a byte basis.
 *        See `ant test.stability`
 *
 *    (2) The second queue contains items where a ClassDef has been lowered into:
 *          (a) an optional mirror class,
 *          (b) a plain class, and
 *          (c) an optional bean class.
 *
 *    (3) The third queue contains items ready for serialization.
 *        It's a priority queue that follows the original arrival order,
 *        so as to emit identical jars on repeated compilation of the same sources.
 *
 *  Plain, mirror, and bean classes are built respectively by PlainClassBuilder, JMirrorBuilder, and JBeanInfoBuilder.
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *  @version 1.0
 *
 */
abstract class GenBCode extends BCodeSyncAndTry with BCodeParallel with HasReporter{
  import global.{reporter => _, _}

  import bTypes._
  import coreBTypes._

  val phaseName = "jvm"

  override def newPhase(prev: Phase) = new BCodePhase(prev)

  final class PlainClassBuilder(cunit: CompilationUnit) extends SyncAndTryBuilder(cunit)

  class BCodePhase(prev: Phase) extends StdPhase(prev) {

    override def name = phaseName
    override def description = "Generate bytecode from ASTs using the ASM library"
    override def erasedTypes = true

    private var bytecodeWriter  : BytecodeWriter   = null
    private var mirrorCodeGen   : JMirrorBuilder   = null
    private var beanInfoCodeGen : JBeanInfoBuilder = null

    private val allData = new ArrayBuffer[Workflow]
    abstract class ParallelWorker[I <: AnyRef, O](val id: Int, val queue: BlockingQueue[Workflow], val timer: Statistics.Timer) extends Runnable {

      import scala.concurrent.{Await, Future}
      import scala.concurrent.duration.Duration
      import scala.util.{Success, Failure}

      def getWork(workflow: Workflow): Future[I]

      def process(input: I): O

      def nextStageSuccess(workflow: Workflow, result: O): Unit

      def nextStageFailed(workflow: Workflow, ex: Throwable): Unit

      def waitReady : Unit

      var currentWork: Workflow = _

      //FIXME should be final
      def run(): Unit = {
        waitReady
        doRun
      }
      @tailrec final def doRun(): Unit = {
        currentWork = queue.poll()
        if (currentWork ne null) {
          withReporterOverride(currentWork) {
            val work = getWork(currentWork)
            Await.ready(work, Duration.Inf)
            work.value.get match {
              case Success(item) =>
                val start = timer.start()
                try {
                  nextStageSuccess(currentWork, process(item))
                } catch {
                  case t: Throwable =>
                    nextStageFailed(currentWork, t)
                }
                timer.stop(start)
              case Failure(f) => //TODO
                nextStageFailed(currentWork, f)
            }
          }
          doRun()
        }
      }
    }

    case class Item1(cd: ClassDef, cunit: CompilationUnit)

    private val optimise:juc.BlockingQueue[Workflow] = new juc.LinkedBlockingQueue[Workflow]
    private val q2:juc.BlockingQueue[Workflow] = new juc.LinkedBlockingQueue[Workflow]
    private val q3:juc.BlockingQueue[Workflow] = new juc.LinkedBlockingQueue[Workflow]


    class Workflow(val cunit:CompilationUnit,
                   val item1:List[Item1])  extends AsyncReporter {
      //we hold the promises as vars. Once read the values are nulled
      var optimize = Promise[List[Item2]]
      var item2 = Promise[List[Item2]]
      var item3 = Promise[List[Item3]]

      override def toString: String = s"Workflow optimizeComplete: ${optimize == null || optimize.isCompleted} item2Complete: ${item2 == null || item2.isCompleted} item3Complete: ${item3 == null || item3.isCompleted}"
    }
    case class Item2(mirror:         asm.tree.ClassNode,
                     plain:          asm.tree.ClassNode,
                     bean:           asm.tree.ClassNode,
                     sourceFilePath: String,
                     outFolder:      scala.tools.nsc.io.AbstractFile)
    /*
     *  An item of queue-3 (the last queue before serializing to disk) contains three of these
     *  (one for each of mirror, plain, and bean classes).
     *
     *  @param jclassName  internal name of the class
     *  @param jclassBytes bytecode emitted for the class SubItem3 represents
     */
    case class SubItem3(
                         jclassName:  String,
                         jclassBytes: Array[Byte]
                       )

    case class Item3(mirror:     SubItem3,
                     plain:      SubItem3,
                     bean:       SubItem3,
                     outFolder:  scala.tools.nsc.io.AbstractFile)


    /*
     *  Pipeline that takes ClassDefs from queue-1, lowers them into an intermediate form, placing them on queue-2
     */
    class Worker1(needsOutFolder: Boolean) {
      import scala.util.{Try, Success, Failure}

      private def outputFolder(workflow: Workflow) = {
        if (needsOutFolder && workflow.item1.nonEmpty) {
          try outputDirectory(workflow.cunit.source)
          catch {
            case t: Throwable =>
              reporter.error(workflow.cunit.body.pos, s"Couldn't create file for ${workflow.cunit.source}\n${t.getMessage}")
              null
          }
        } else null
      }

      //TODO should be a scalac param
      val checkCaseInsensitively = true

      val caseInsensitively = mutable.Map.empty[String, Symbol]

      def run() {
        for (workflow <- allData) withCurrentUnitNoLog(workflow.cunit){
          val item2Builder = List.newBuilder[Item2]
          val outFolder = outputFolder(workflow)
          for (item1 <- workflow.item1) {
            try {
              item2Builder += visit(item1,workflow.cunit, outFolder)
            } catch {
              case ex: Throwable =>
                ex.printStackTrace()
                //we can report directly to the reporter - this is not parallelised
                reporter.error(NoPosition, s"Error while emitting ${workflow.cunit.source}\n${ex.getMessage}")
            }
          }
          workflow.optimize.success(item2Builder.result())
        }
      }

      /*
       *  Checks for duplicate internal names case-insensitively,
       *  builds ASM ClassNodes for mirror, plain, and bean classes;
       *  returns an Item2.
       *
       */
      def visit(item: Item1, cunit:CompilationUnit, outFolder:AbstractFile) = {
        val cd = item.cd
        val claszSymbol = cd.symbol

        if (checkCaseInsensitively) {
          // GenASM checks this before classfiles are emitted, https://github.com/scala/scala/commit/e4d1d930693ac75d8eb64c2c3c69f2fc22bec739
          val lowercaseJavaClassName = claszSymbol.javaClassName.toLowerCase
          caseInsensitively.get(lowercaseJavaClassName) match {
            case None =>
              caseInsensitively.put(lowercaseJavaClassName, claszSymbol)
            case Some(dupClassSym) =>
              reporter.warning(
                claszSymbol.pos,
                s"Class ${claszSymbol.javaClassName} differs only in case from ${dupClassSym.javaClassName}. " +
                  "Such classes will overwrite one another on case-insensitive filesystems."
              )
          }
        }

        // shim for SBT, see https://github.com/sbt/sbt/issues/2076
        // TODO put this closer to classfile writing once we have closure elimination
        // TODO create a nicer public API to find out the correspondence between sourcefile and ultimate classfiles
        currentUnit.icode += new icodes.IClass(cd.symbol)

        // -------------- mirror class, if needed --------------
        val mirrorC =
          if (isTopLevelModuleClass(claszSymbol)) {
            if (claszSymbol.companionClass == NoSymbol) {
              mirrorCodeGen.genMirrorClass(claszSymbol, cunit)
            } else {
              log(s"No mirror class for module with linked class: ${claszSymbol.fullName}")
              null
            }
          } else null

        // -------------- "plain" class --------------
        val pcb = new PlainClassBuilder(cunit)
        pcb.genPlainClass(cd)
        val plainC = pcb.cnode

        // -------------- bean info class, if needed --------------
        val beanC =
          if (claszSymbol hasAnnotation BeanInfoAttr) {
            beanInfoCodeGen.genBeanInfoClass(
              claszSymbol, cunit,
              fieldSymbols(claszSymbol),
              methodSymbols(cd)
            )
          } else null

        // ----------- hand over to pipeline-2

        Item2(mirrorC, plainC, beanC,
          cunit.source.file.canonicalPath,
          outFolder)


      } // end of method visit(Item1)

    } // end of class BCodePhase.Worker1

  class OptimisationWorkflow(allData: ArrayBuffer[Workflow]) extends Runnable {

    import scala.util.{Try, Success, Failure}

    val compilerSettings = bTypes.compilerSettings
    val byteCodeRepository = bTypes.byteCodeRepository
    val callGraph = bTypes.callGraph

    val optAddToBytecodeRepository = compilerSettings.optAddToBytecodeRepository
    val optBuildCallGraph = compilerSettings.optBuildCallGraph
    val optInlinerEnabled  = compilerSettings.optInlinerEnabled
    val optClosureInvocations =  compilerSettings.optClosureInvocations
    val hasGlobalOptimisations = optInlinerEnabled || optClosureInvocations

    override def run(): Unit = {
      val oldName = Thread.currentThread.getName
      try {
        try Thread.currentThread().setName("GenBcode - OptimisationWorkflow")
        catch { case _ : SecurityException => /*ignore*/ }

        val downstreams = allData map { workflow: Workflow =>
          Await.ready(workflow.optimize.future, Duration.Inf)
          val upstream = workflow.optimize.future.value.get
          workflow.optimize = null //release memory
          try {
            upstream match {
              case Success(items) =>
                for (item <- items) {
                  // add classes to the bytecode repo before building the call graph: the latter needs to
                  // look up classes and methods in the code repo.
                  if (optAddToBytecodeRepository) {
                    val someSourceFilePath = Some(item.sourceFilePath)
                    //byteCodeRepository.add is threadsafe and doesnt access tree
                    if (item.mirror != null) byteCodeRepository.add(item.mirror, someSourceFilePath)
                    if (item.plain != null) byteCodeRepository.add(item.plain, someSourceFilePath)
                    if (item.bean != null) byteCodeRepository.add(item.bean, someSourceFilePath)
                  }
                  if (optBuildCallGraph) {
                    // skip call graph for mirror / bean: wd don't inline into them, and they are not used in the plain class
                    if (item.plain != null) callGraph.addClass(item.plain)
                  }
                }
              case _ =>
            }
            if (!hasGlobalOptimisations) {
              workflow.item2.complete(upstream)
            }
            upstream
          } catch {
            case NonFatal(t) =>
              val downstream = Failure(t)
              if (!hasGlobalOptimisations) {
                workflow.item2.complete(downstream)
              }
              downstream
          }
        }
        //we dont need a tree lock here as the this is a global operation -
        //globally blocking already
        if (hasGlobalOptimisations) {
          if (optInlinerEnabled)
            bTypes.inliner.runInliner()
          if (optClosureInvocations)
            bTypes.closureOptimizer.rewriteClosureApplyInvocations()
          for (i <- 0 to allData.size) {
            allData(i).item2.complete(downstreams(i))
          }
        }

      } catch {
        case t: Throwable =>
          val fail = Failure(t)
          allData map {
            _.item2.tryComplete(fail)
          }
          throw t
      } finally {
        try Thread.currentThread().setName(oldName)
        catch { case _ : SecurityException => /*ignore */ }
      }
    }
  }


          /*
           *  Pipeline that takes ClassNodes from queue-2. The unit of work depends on the optimization level:
           *
           *    (a) no optimization involves:
           *          - converting the plain ClassNode to byte array and placing it on queue-3
           */
    class Worker2 (id: Int, q2: BlockingQueue[Workflow]) extends ParallelWorker[List[Item2], List[Item3]](id, q2, BackendStats.bytesGenStat) {
      val localOpt = bTypes.localOpt
      val backendUtils = bTypes.backendUtils

      override def getWork(workflow: Workflow): Future[List[Item2]] = {
        val res = workflow.item2.future
        workflow.item2 = null //release memory
        res
      }

      override def nextStageSuccess(workflow: Workflow, result: List[Item3]): Unit = {
        workflow.item3.success(result)
      }

      override def nextStageFailed(workflow: Workflow, ex: Throwable): Unit = {
        workflow.item3.failure(ex)
      }

      override def waitReady: Unit = {
        Option(allData.last.optimize) foreach {p => Await.ready(p.future, Duration.Inf)}
      }

      def localOptimizations(classNode: ClassNode): Unit = {
        BackendStats.timed(BackendStats.methodOptTimer)(localOpt.methodOptimizations(classNode))
      }

      def setInnerClasses(classNode: ClassNode): Unit = if (classNode != null) {
        classNode.innerClasses.clear()
        addInnerClasses(classNode, bTypes.backendUtils.collectNestedClasses(classNode))
      }

      override def process(items: List[Item2]): List[Item3] = {
        items map { item =>
          try {
            localOptimizations(item.plain)
            setInnerClasses(item.plain)
            val lambdaImplMethods = getIndyLambdaImplMethods(item.plain.name)
            if (lambdaImplMethods.nonEmpty)
              backendUtils.addLambdaDeserialize(item.plain, lambdaImplMethods)
            setInnerClasses(item.mirror)
            setInnerClasses(item.bean)
            makeItem3(item)
          } catch {
            case e: java.lang.RuntimeException if e.getMessage != null && (e.getMessage contains "too large!") =>
              reporter.error(NoPosition,
                s"Could not write class ${item.plain.name} because it exceeds JVM code size limits. ${e.getMessage}")
              throw e
            case ex: Throwable =>
              ex.printStackTrace()
              reporter.error(NoPosition,
                s"Error while emitting ${item.plain.name}\n${ex.getMessage}")
              throw ex
          }
        }
      }

      private def makeItem3(item: Item2) = {
        def getByteArray(cn: asm.tree.ClassNode): Array[Byte] = {
          val cw = new CClassWriter(extraProc)
          cn.accept(cw)
          cw.toByteArray
        }
        val Item2(mirror, plain, bean, _, outFolder) = item

        val mirrorC = if (mirror == null) null else SubItem3(mirror.name, getByteArray(mirror))
        val plainC = SubItem3(plain.name, getByteArray(plain))
        val beanC = if (bean == null) null else SubItem3(bean.name, getByteArray(bean))

        if (AsmUtils.traceSerializedClassEnabled && plain.name.contains(AsmUtils.traceSerializedClassPattern)) {
          if (mirrorC != null) AsmUtils.traceClass(mirrorC.jclassBytes)
          AsmUtils.traceClass(plainC.jclassBytes)
          if (beanC != null) AsmUtils.traceClass(beanC.jclassBytes)
        }

        Item3(mirrorC, plainC, beanC, outFolder)

      }

    } // end of class BCodePhase.Worker2

    /**
     * The `run` method is overridden because the backend has a different data flow than the default
     * phase: the backend does not transform compilation units one by one, but on all units in the
     * same run. This allows cross-unit optimizations and running some stages of the backend
     * concurrently on multiple units.
     *
     *  A run of the BCodePhase phase comprises:
     *
     *    (a) set-up steps (most notably supporting maps in `BCodeTypes`,
     *        but also "the" writer where class files in byte-array form go)
     *
     *    (b) building of ASM ClassNodes, their optimization and serialization.
     *
     *    (c) tear down (closing the classfile-writer and clearing maps)
     *
     */
    override def run() {
      val bcodeStart = Statistics.startTimer(BackendStats.bcodeTimer)

      val initStart = Statistics.startTimer(BackendStats.bcodeInitTimer)
      allData.clear() // just in case
      scalaPrimitives.init()
      bTypes.initializeCoreBTypes()
      bTypes.javaDefinedClasses.clear()
      bTypes.javaDefinedClasses ++= currentRun.symSource collect {
        case (sym, _) if sym.isJavaDefined => sym.javaBinaryNameString
      }
      Statistics.stopTimer(BackendStats.bcodeInitTimer, initStart)

      // initBytecodeWriter invokes fullName, thus we have to run it before the typer-dependent thread is activated.
      bytecodeWriter  = initBytecodeWriter(cleanup.getEntryPoints)
      mirrorCodeGen   = new JMirrorBuilder
      beanInfoCodeGen = new JBeanInfoBuilder

      val needsOutfileForSymbol = bytecodeWriter.isInstanceOf[ClassBytecodeWriter]
      buildAndSendToDisk(needsOutfileForSymbol)

      // closing output files.
      bytecodeWriter.close()
      Statistics.stopTimer(BackendStats.bcodeTimer, bcodeStart)

      /* TODO Bytecode can be verified (now that all classfiles have been written to disk)
       *
       * (1) asm.util.CheckAdapter.verify()
       *       public static void verify(ClassReader cr, ClassLoader loader, boolean dump, PrintWriter pw)
       *     passing a custom ClassLoader to verify inter-dependent classes.
       *     Alternatively,
       *       - an offline-bytecode verifier could be used (e.g. Maxine brings one as separate tool).
       *       - -Xverify:all
       *
       * (2) if requested, check-java-signatures, over and beyond the syntactic checks in `getGenericSignature()`
       *
       */
    }

    private def onWorkerError(workerFailed: Throwable): Unit = {
      workerFailed.printStackTrace()
      reporter.error(NoPosition, workerFailed.toString)
    }
    private def exec(name:String, priorityDelta:Int): ExecutionContext = {
      import java.util.concurrent.{Executors, ThreadFactory}
      import java.util.concurrent.atomic.AtomicInteger
      object BoostThreadFactory extends ThreadFactory {
        final private val group = Thread.currentThread.getThreadGroup
        final private val threadNumber = new AtomicInteger(1)

        def newThread(r: Runnable): Thread = {
          val t = new Thread(group, r, name + threadNumber.getAndIncrement, 0)
          t.setPriority(Thread.NORM_PRIORITY + priorityDelta)
          t
        }
      }
      ExecutionContext.fromExecutorService(Executors.newCachedThreadPool(BoostThreadFactory), onWorkerError)
    }
    /*
     *  Sequentially:
     *    (a) place all ClassDefs in queue-1
     *    (b) dequeue one at a time from queue-1, convert it to ASM ClassNode, place in queue-2
     *  In Parallel
     *    (c) seperate threads dequeue one at a time from queue-2, convert it to byte-array,    place in queue-3
     *    (d) seperate threads dequeue one at a time serialize to disk by draining queue-3.
     */
    private def buildAndSendToDisk(needsOutFolder: Boolean) {

      import scala.concurrent.Future
      val runInParallel = settings.YgenBcodeParallel.value

      feedPipeline1()
      if (runInParallel) {
        reporter.echo(NoPosition, "running GenBCode in parallel")

        def checkWorker(worker: Future[Unit]) = Await.result(worker, Duration.Inf)

        val ecOptimise = exec("GenBCode:optimise-",1)
        val ecWorker2 = exec("GenBCode:worker2-",2)
        val ecWorker3 = exec("GenBCode:worker3-",3)

        val workerOpt: Future[Unit] = Future(new OptimisationWorkflow(allData).run)(ecOptimise)
        val workers2: List[Future[Unit]] = (1 to 4).map { i => Future(new Worker2(i, q2).run)(ecWorker2) }(scala.collection.breakOut)
        val workers3: List[Future[Unit]] = (1 to (if (bytecodeWriter.isSingleThreaded) 1 else 4)).map {
          i => Future(new Worker3(i, q3, bytecodeWriter).run)(ecWorker3)
        }(scala.collection.breakOut)

        val genStart = Statistics.startTimer(BackendStats.bcodeGenStat)
        (new Worker1(needsOutFolder)).run()
        Statistics.stopTimer(BackendStats.bcodeGenStat, genStart)
        // check for any exception during the operation of the background threads
        checkWorker(workerOpt)
        workers2 foreach checkWorker
        workers3 foreach checkWorker
      } else {
        val genStart = Statistics.startTimer(BackendStats.bcodeGenStat)
        (new Worker1(needsOutFolder)).run()
        Statistics.stopTimer(BackendStats.bcodeGenStat, genStart)
        new OptimisationWorkflow(allData).run
        new Worker2(99, q2).run
        val writeStart = Statistics.startTimer(BackendStats.bcodeWriteTimer)
        new Worker3(99, q3, bytecodeWriter).run
        Statistics.stopTimer(BackendStats.bcodeWriteTimer, writeStart)

      }
      //report any deferred messages
      val globalReporter = reporter
      allData foreach { _.relayReports(globalReporter) }
      allData.clear()


      // we're done
      assert(q2.isEmpty, s"Some classfiles remained in the second queue: $q2")
      assert(q3.isEmpty, s"Some classfiles weren't written to disk: $q3")

    }

    /* Feed pipeline-1: place all ClassDefs on q1, recording their arrival position. */
    private def feedPipeline1() {
      super.run()
    }

    /* Pipeline that writes classfile representations to disk. */
    private class Worker3(id: Int, q3: BlockingQueue[Workflow], bytecodeWriter: BytecodeWriters#BytecodeWriter)
      extends ParallelWorker[List[Item3], Unit](id, q3, BackendStats.bcodeWriteTimer) {

      val localOpt = bTypes.localOpt
      val backendUtils = bTypes.backendUtils

      override def getWork(workflow: Workflow): Future[List[Item3]] = {
        val res = workflow.item3.future
        workflow.item3 = null //release memory
        res
      }

      override def nextStageSuccess(workflow: Workflow, result: Unit): Unit = {
      }

      override def nextStageFailed(workflow: Workflow, ex: Throwable): Unit = {
      }

      override def waitReady: Unit = ()

      def sendToDisk(cfr: SubItem3, outFolder: scala.tools.nsc.io.AbstractFile) {
        if (cfr != null) {
          val SubItem3(jclassName, jclassBytes) = cfr
          try {
            val outFile =
              if (outFolder == null) null
              else getFileForClassfile(outFolder, jclassName, ".class")
            println(s"$outFolder $jclassName $outFile")
            bytecodeWriter.writeClass(jclassName, jclassName, jclassBytes, outFile)
          }
          catch {
            case e: FileConflictException =>
              reporter.error(NoPosition, s"error writing $jclassName: ${e.getMessage}")
          }
        }
      }

      override def process(items: List[Item3]): Unit = {
        for (item <- items) {
          val outFolder = item.outFolder
          sendToDisk(item.mirror, outFolder)
          sendToDisk(item.plain, outFolder)
          sendToDisk(item.bean, outFolder)
        }
      }
    }

    override def apply(cunit: CompilationUnit): Unit = {

      val classesInCompilation = List.newBuilder[Item1]
      def gen(tree: Tree) {
        tree match {
          case EmptyTree            => ()
          case PackageDef(_, stats) => stats foreach gen
          case cd: ClassDef         =>
            classesInCompilation += Item1(cd, cunit)
        }
      }

      gen(cunit.body)
      val workflow = new Workflow(cunit, classesInCompilation.result())

      allData += workflow
      q2 add workflow
      q3 add workflow
      optimise add workflow
    }

  } // end of class BCodePhase


} // end of class GenBCode

object GenBCode {
  def mkFlags(args: Int*) = args.foldLeft(0)(_ | _)

  final val PublicStatic      = asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_STATIC
  final val PublicStaticFinal = asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_STATIC | asm.Opcodes.ACC_FINAL

  val CLASS_CONSTRUCTOR_NAME    = "<clinit>"
  val INSTANCE_CONSTRUCTOR_NAME = "<init>"
}