package scala.tools.nsc.backend.jvm

import org.junit.Assert._

import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.VirtualDirectory
import scala.tools.asm.Opcodes
import scala.tools.asm.tree.{AbstractInsnNode, LabelNode, ClassNode, MethodNode}
import scala.tools.cmd.CommandLineParser
import scala.tools.nsc.{Settings, Global}
import scala.tools.partest.ASMConverters
import scala.collection.JavaConverters._

object CodeGenTools {
  import ASMConverters._

  def genMethod( flags: Int = Opcodes.ACC_PUBLIC,
                 name: String = "m",
                 descriptor: String = "()V",
                 genericSignature: String = null,
                 throwsExceptions: Array[String] = null,
                 handlers: List[ExceptionHandler] = Nil,
                 localVars: List[LocalVariable] = Nil)(body: Instruction*): MethodNode = {
    val node = new MethodNode(flags, name, descriptor, genericSignature, throwsExceptions)
    applyToMethod(node, Method(body.toList, handlers, localVars))
    node
  }

  def wrapInClass(method: MethodNode): ClassNode = {
    val cls = new ClassNode()
    cls.visit(Opcodes.V1_6, Opcodes.ACC_PUBLIC, "C", null, "java/lang/Object", null)
    cls.methods.add(method)
    cls
  }

  private def resetOutput(compiler: Global): Unit = {
    compiler.settings.outputDirs.setSingleOutput(new VirtualDirectory("(memory)", None))
  }

  def newCompiler(defaultArgs: String = "-usejavacp", extraArgs: String = ""): Global = {
    val settings = new Settings()
    val args = (CommandLineParser tokenize defaultArgs) ++ (CommandLineParser tokenize extraArgs)
    settings.processArguments(args, processAll = true)
    val compiler = new Global(settings)
    resetOutput(compiler)
    compiler
  }

  def compile(compiler: Global)(code: String): List[(String, Array[Byte])] = {
    compiler.reporter.reset()
    resetOutput(compiler)
    val run = new compiler.Run()
    run.compileSources(List(new BatchSourceFile("unitTestSource.scala", code)))
    val outDir = compiler.settings.outputDirs.getSingleOutput.get
    (for (f <- outDir.iterator if !f.isDirectory) yield (f.name, f.toByteArray)).toList
  }

  def compileClasses(compiler: Global)(code: String): List[ClassNode] = {
    compile(compiler)(code).map(p => AsmUtils.readClass(p._2)).sortBy(_.name)
  }

  def compileMethods(compiler: Global)(code: String): List[MethodNode] = {
    compileClasses(compiler)(s"class C { $code }").head.methods.asScala.toList.filterNot(_.name == "<init>")
  }

  def singleMethodInstructions(compiler: Global)(code: String): List[Instruction] = {
    val List(m) = compileMethods(compiler)(code)
    instructionsFromMethod(m)
  }

  def singleMethod(compiler: Global)(code: String): Method = {
    val List(m) = compileMethods(compiler)(code)
    convertMethod(m)
  }

  def assertSameCode(actual: List[Instruction], expected: List[Instruction]): Unit = {
    assertTrue(s"\nExpected: $expected\nActual  : $actual", actual === expected)
  }
}
