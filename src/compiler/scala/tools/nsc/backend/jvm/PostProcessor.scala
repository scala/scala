package scala.tools.nsc.backend.jvm

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.NoPosition
import scala.tools.asm.ClassWriter
import scala.tools.asm.tree.ClassNode
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.io.AbstractFile

/**
 * Implements late stages of the backend that don't depend on a Global instance, i.e.,
 * optimizations, post-processing and classfile serialization and writing.
 */
class PostProcessor[BT <: BTypes](val bTypes: BT, getEntryPoints: () => List[String]) {
  import bTypes._

  // re-initialized per run because it reads compiler settings that might change
  var classfileWriter: ClassfileWriter[bTypes.type] = _

  val generatedClasses = recordPerRunCache(new ListBuffer[GeneratedClass])

  def initialize(): Unit = {
    classfileWriter = new ClassfileWriter[bTypes.type](bTypes, backendReporting, getEntryPoints)
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

        classfileWriter.write(classNode.name, bytes, sourceFile)
      }
    }

    classfileWriter.close()
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
    val cw = new ClassWriterWithBTypeLub(backendUtils.extraProc)
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

// Temporary class, will be refactored in a future commit
trait ClassWriterForPostProcessor {
  def write(bytes: Array[Byte], className: InternalName, sourceFile: AbstractFile)
}