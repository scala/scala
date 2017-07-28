package scala.tools.nsc.backend.jvm

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.NoPosition
import scala.tools.asm
import scala.tools.asm.ClassWriter
import scala.tools.asm.tree.ClassNode
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.io.AbstractFile

/**
 * Implements late stages of the backend that don't depend on a Global instance, i.e.,
 * optimizations, post-processing and classfile serialization and writing.
 */
class PostProcessor[BT <: BTypes](val bTypes: BT) {
  import bTypes._

  val generatedClasses = recordPerRunCache(new ListBuffer[GeneratedClass])

  def postProcessAndSendToDisk(classWriter: ClassWriterForPostProcessor): Unit = {
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

        classWriter.write(bytes, classNode.name, sourceFile)
      }
    }
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
    val cw = new CClassWriter(backendUtils.extraProc)
    classNode.accept(cw)
    cw.toByteArray
  }


  // -----------------------------------------------------------------------------------------
  // finding the least upper bound in agreement with the bytecode verifier (given two internal names handed by ASM)
  // Background:
  //  http://gallium.inria.fr/~xleroy/publi/bytecode-verification-JAR.pdf
  //  http://comments.gmane.org/gmane.comp.java.vm.languages/2293
  //  https://github.com/scala/bug/issues/3872
  // -----------------------------------------------------------------------------------------

  /*  An `asm.ClassWriter` that uses `jvmWiseLUB()`
   *  The internal name of the least common ancestor of the types given by inameA and inameB.
   *  It's what ASM needs to know in order to compute stack map frames, http://asm.ow2.org/doc/developer-guide.html#controlflow
   */
  final class CClassWriter(flags: Int) extends ClassWriter(flags) {

    /**
     * This method is used by asm when computing stack map frames. It is thread-safe: it depends
     * only on the BTypes component, which does not depend on global.
     * TODO @lry move to a different place where no global is in scope, on bTypes.
     */
    override def getCommonSuperClass(inameA: String, inameB: String): String = {
      // All types that appear in a class node need to have their ClassBType cached, see [[cachedClassBType]].
      val a = cachedClassBType(inameA).get
      val b = cachedClassBType(inameB).get
      val lub = a.jvmWiseLUB(b).get
      val lubName = lub.internalName
      assert(lubName != "scala/Any")
      lubName // ASM caches the answer during the lifetime of a ClassWriter. We outlive that. Not sure whether caching on our side would improve things.
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