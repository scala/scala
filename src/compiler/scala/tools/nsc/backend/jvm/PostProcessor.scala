package scala.tools.nsc.backend.jvm

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.NoPosition
import scala.reflect.io.AbstractFile
import scala.tools.asm.ClassWriter
import scala.tools.asm.tree.ClassNode
import scala.tools.nsc.backend.jvm.analysis.BackendUtils
import scala.tools.nsc.backend.jvm.opt._

/**
 * Implements late stages of the backend that don't depend on a Global instance, i.e.,
 * optimizations, post-processing and classfile serialization and writing.
 */
abstract class PostProcessor extends PerRunInit {
  val bTypes: BTypes

  import bTypes._
  import frontendAccess.{backendReporting, compilerSettings, recordPerRunCache}

  val backendUtils        = new { val postProcessor: PostProcessor.this.type = PostProcessor.this } with BackendUtils
  val byteCodeRepository  = new { val postProcessor: PostProcessor.this.type = PostProcessor.this } with ByteCodeRepository
  val localOpt            = new { val postProcessor: PostProcessor.this.type = PostProcessor.this } with LocalOpt
  val inliner             = new { val postProcessor: PostProcessor.this.type = PostProcessor.this } with Inliner
  val inlinerHeuristics   = new { val postProcessor: PostProcessor.this.type = PostProcessor.this } with InlinerHeuristics
  val closureOptimizer    = new { val postProcessor: PostProcessor.this.type = PostProcessor.this } with ClosureOptimizer
  val callGraph           = new { val postProcessor: PostProcessor.this.type = PostProcessor.this } with CallGraph
  val bTypesFromClassfile = new { val postProcessor: PostProcessor.this.type = PostProcessor.this } with BTypesFromClassfile

  // re-initialized per run because it reads compiler settings that might change
  lazy val classfileWriter: LazyVar[ClassfileWriter] = perRunLazy(this)(new ClassfileWriter(frontendAccess))

  lazy val generatedClasses = recordPerRunCache(new ListBuffer[GeneratedClass])

  override def initialize(): Unit = {
    super.initialize()
    backendUtils.initialize()
    inlinerHeuristics.initialize()
  }

  def postProcessAndSendToDisk(): Unit = {
    runGlobalOptimizations()

    for (GeneratedClass(classNode, sourceFile, isArtifact) <- generatedClasses) {
      val bytes = try {
        if (!isArtifact) {
          localOptimizations(classNode)
          val lambdaImplMethods = getIndyLambdaImplMethods(classNode.name)
          if (lambdaImplMethods.nonEmpty)
            backendUtils.addLambdaDeserialize(classNode, lambdaImplMethods)
        }
        setInnerClasses(classNode)
        serializeClass(classNode)
      } catch {
        case e: java.lang.RuntimeException if e.getMessage != null && (e.getMessage contains "too large!") =>
          backendReporting.error(NoPosition,
            s"Could not write class ${classNode.name} because it exceeds JVM code size limits. ${e.getMessage}")
          null
        case ex: Throwable =>
          ex.printStackTrace()
          backendReporting.error(NoPosition, s"Error while emitting ${classNode.name}\n${ex.getMessage}")
          null
      }

      if (bytes != null) {
        if (AsmUtils.traceSerializedClassEnabled && classNode.name.contains(AsmUtils.traceSerializedClassPattern))
          AsmUtils.traceClass(bytes)

        classfileWriter.get.write(classNode.name, bytes, sourceFile)
      }
    }

    classfileWriter.get.close()
  }

  def runGlobalOptimizations(): Unit = {
    // add classes to the bytecode repo before building the call graph: the latter needs to
    // look up classes and methods in the code repo.
    if (compilerSettings.optAddToBytecodeRepository) for (c <- generatedClasses) {
      byteCodeRepository.add(c.classNode, Some(c.sourceFile.canonicalPath))
    }
    if (compilerSettings.optBuildCallGraph) for (c <- generatedClasses if !c.isArtifact) {
      // skip call graph for mirror / bean: we don't inline into them, and they are not referenced from other classes
      callGraph.addClass(c.classNode)
    }
    if (compilerSettings.optInlinerEnabled)
      inliner.runInliner()
    if (compilerSettings.optClosureInvocations)
      closureOptimizer.rewriteClosureApplyInvocations()
  }

  def localOptimizations(classNode: ClassNode): Unit = {
    BackendStats.timed(BackendStats.methodOptTimer)(localOpt.methodOptimizations(classNode))
  }

  def setInnerClasses(classNode: ClassNode): Unit = {
    classNode.innerClasses.clear()
    backendUtils.addInnerClasses(classNode, backendUtils.collectNestedClasses(classNode))
  }

  def serializeClass(classNode: ClassNode): Array[Byte] = {
    val cw = new ClassWriterWithBTypeLub(backendUtils.extraProc.get)
    classNode.accept(cw)
    cw.toByteArray
  }

  /**
   * An asm ClassWriter that uses ClassBType.jvmWiseLUB to compute the common superclass of class
   * types. This operation is used for computing statck map frames.
   */
  final class ClassWriterWithBTypeLub(flags: Int) extends ClassWriter(flags) {
    /**
     * This method is invoked by asm during classfile writing when computing stack map frames.
     *
     * TODO: it might make sense to cache results per compiler run. The ClassWriter caches
     * internally, but we create a new writer for each class. scala/scala-dev#322.
     */
    override def getCommonSuperClass(inameA: String, inameB: String): String = {
      // All types that appear in a class node need to have their ClassBType cached, see [[cachedClassBType]].
      val a = cachedClassBType(inameA).get
      val b = cachedClassBType(inameB).get
      val lub = a.jvmWiseLUB(b).get
      val lubName = lub.internalName
      assert(lubName != "scala/Any")
      lubName
    }
  }
}

/**
 * The result of code generation. [[isArtifact]] is `true` for mirror and bean-info classes.
 */
case class GeneratedClass(classNode: ClassNode, sourceFile: AbstractFile, isArtifact: Boolean)
