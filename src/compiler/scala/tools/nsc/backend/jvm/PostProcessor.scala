/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package backend.jvm

import java.util.concurrent.ConcurrentHashMap

import scala.collection.mutable
import scala.reflect.internal.util.{NoPosition, Position}
import scala.reflect.io.AbstractFile
import scala.tools.asm.{ByteVector, ClassVisitor, ClassWriter, MethodVisitor, Opcodes}
import scala.tools.asm.tree.ClassNode
import scala.tools.nsc.backend.jvm.analysis.BackendUtils
import scala.tools.nsc.backend.jvm.opt._
import scala.util.control.NonFatal

/**
 * Implements late stages of the backend that don't depend on a Global instance, i.e.,
 * optimizations, post-processing and classfile serialization and writing.
 */
abstract class PostProcessor extends PerRunInit {
  self =>
  val bTypes: BTypes

  import bTypes._
  import frontendAccess.{backendReporting, compilerSettings, recordPerRunJavaMapCache}

  val backendUtils        : BackendUtils        { val postProcessor: self.type } = new { val postProcessor: self.type = self } with BackendUtils
  val byteCodeRepository  : ByteCodeRepository  { val postProcessor: self.type } = new { val postProcessor: self.type = self } with ByteCodeRepository
  val localOpt            : LocalOpt            { val postProcessor: self.type } = new { val postProcessor: self.type = self } with LocalOpt
  val inliner             : Inliner             { val postProcessor: self.type } = new { val postProcessor: self.type = self } with Inliner
  val inlinerHeuristics   : InlinerHeuristics   { val postProcessor: self.type } = new { val postProcessor: self.type = self } with InlinerHeuristics
  val closureOptimizer    : ClosureOptimizer    { val postProcessor: self.type } = new { val postProcessor: self.type = self } with ClosureOptimizer
  val callGraph           : CallGraph           { val postProcessor: self.type } = new { val postProcessor: self.type = self } with CallGraph
  val bTypesFromClassfile : BTypesFromClassfile { val postProcessor: self.type } = new { val postProcessor: self.type = self } with BTypesFromClassfile
  val classfileWriters    : ClassfileWriters    { val postProcessor: self.type } = new { val postProcessor: self.type = self } with ClassfileWriters

  var classfileWriter: classfileWriters.ClassfileWriter = _

  // from lowercase to first-seen name and position thereof
  private val caseInsensitively = recordPerRunJavaMapCache(new ConcurrentHashMap[String, (String, Position)])

  def initialize(global: Global): Unit = {
    this.initialize()
    backendUtils.initialize()
    inlinerHeuristics.initialize()
    byteCodeRepository.initialize()
    classfileWriter = classfileWriters.ClassfileWriter(global)
  }

  def sendToDisk(clazz: GeneratedClass, sourceFile: AbstractFile): Unit = {
    val classNode = clazz.classNode
    val internalName = classNode.name
    val bytes = try {
      if (!clazz.isArtifact) {
        localOptimizations(classNode)
        val indyLambdaBodyMethods = backendUtils.indyLambdaBodyMethods(internalName)
        if (indyLambdaBodyMethods.nonEmpty)
          backendUtils.addLambdaDeserialize(classNode, indyLambdaBodyMethods)
      }

      warnCaseInsensitiveOverwrite(clazz)
      setInnerClasses(classNode)
      serializeClass(classNode)
    } catch {
      case ex: InterruptedException => throw ex
      case ex: Throwable =>
        // TODO fail fast rather than continuing to write the rest of the class files?
        if (frontendAccess.compilerSettings.debug) ex.printStackTrace()
        backendReporting.error(NoPosition, s"Error while emitting $internalName\n${ex.getMessage}")
        if (NonFatal(ex)) checkConformance(classNode)
        null
    }

    if (bytes != null) {
      if (AsmUtils.traceSerializedClassEnabled && internalName.contains(AsmUtils.traceSerializedClassPattern))
        AsmUtils.traceClass(bytes)

      classfileWriter.writeClass(internalName, bytes, sourceFile)
    }
  }

  // Provide more useful messaging when class writing fails. -Xverify verifies always.
  private def checkConformance(classNode: ClassNode): Unit = {
    val visitor = new ClassVisitor(Opcodes.ASM9) {
      override def visitMethod(access: Int, name: String, descriptor: String, signature: String, exceptions: Array[String]) = {
        val site = s"Method $name"
        checkString(site, "name")(name)
        checkString(site, "descriptor")(descriptor)
        checkString(site, "signature")(signature)
        new MethodVisitor(Opcodes.ASM9) {
          override def visitLdcInsn(value: Object): Unit = {
            value match {
              case value: String =>
                checkString(site, "String constant")(value)
              case _ =>
            }
          }
        }
      }
      private final val max = 0xFFFF
      private final val threshold = 0xFFFF / 6 // char never expands to more than 6 bytes
      private def checkString(site: String, which: String)(s: String): Unit = if (s != null && s.length > threshold) {
        def fail() = backendReporting.error(NoPosition, s"$site in class ${classNode.name} has a bad $which of length ${s.length}${ if (frontendAccess.compilerSettings.debug) s":\n$s" else "" }")
        if (s.length > max) fail()
        else
          try new ByteVector(s.length).putUTF8(s)
          catch { case _: IllegalArgumentException => fail() }
      }
    }
    classNode.accept(visitor)
  }

  private def warnCaseInsensitiveOverwrite(clazz: GeneratedClass): Unit = {
    val name = clazz.classNode.name
    val lowercaseJavaClassName = name.toLowerCase

    val overwrites = caseInsensitively.putIfAbsent(lowercaseJavaClassName, (name, clazz.position))
    if (overwrites ne null) {
      val (dupName, dupPos) = overwrites
      val locationAddendum =
        if (dupPos.source.path != clazz.position.source.path)
          s" (defined in ${dupPos.source.file.name})"
        else ""
      def nicify(name: String): String = name.replace('/', '.')
      backendReporting.warning(
        clazz.position,
        sm"""Generated class ${nicify(name)} differs only in case from ${nicify(dupName)}$locationAddendum.
            |  Such classes will overwrite one another on case-insensitive filesystems."""
      )
    }
  }

  def runGlobalOptimizations(generatedUnits: Iterable[GeneratedCompilationUnit]): Unit = {
    // add classes to the bytecode repo before building the call graph: the latter needs to
    // look up classes and methods in the code repo.
    if (compilerSettings.optAddToBytecodeRepository) {
      for (u <- generatedUnits; c <- u.classes) {
        byteCodeRepository.add(c.classNode, Some(u.sourceFile.canonicalPath))
      }
      if (compilerSettings.optBuildCallGraph) for (u <- generatedUnits; c <- u.classes if !c.isArtifact) {
        // skip call graph for mirror / bean: we don't inline into them, and they are not referenced from other classes
        callGraph.addClass(c.classNode)
      }
      if (compilerSettings.optInlinerEnabled)
        inliner.runInlinerAndClosureOptimizer()
      else if (compilerSettings.optClosureInvocations)
        closureOptimizer.rewriteClosureApplyInvocations(None, mutable.Map.empty)
    }
  }

  def localOptimizations(classNode: ClassNode): Unit = {
    val stats = frontendAccess.unsafeStatistics
    stats.timed(stats.methodOptTimer)(localOpt.methodOptimizations(classNode))
  }

  def setInnerClasses(classNode: ClassNode): Unit = {
    classNode.innerClasses.clear()
    val (declared, referred) = backendUtils.collectNestedClasses(classNode)
    backendUtils.addInnerClasses(classNode, declared, referred)
  }

  def serializeClass(classNode: ClassNode): Array[Byte] = {
    val cw = new ClassWriterWithBTypeLub(backendUtils.extraProc.get)
    classNode.accept(cw)
    cw.toByteArray
  }

  /**
   * An asm ClassWriter that uses ClassBType.jvmWiseLUB to compute the common superclass of class
   * types. This operation is used for computing stack map frames.
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
      val a = cachedClassBType(inameA)
      val b = cachedClassBType(inameB)
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
case class GeneratedClass(classNode: ClassNode, sourceClassName: String, position: Position, isArtifact: Boolean)
case class GeneratedCompilationUnit(sourceFile: AbstractFile, classes: List[GeneratedClass])
