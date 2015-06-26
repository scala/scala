package scala.tools.nsc.backend.jvm

import org.junit.Assert._

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.VirtualDirectory
import scala.tools.asm.Opcodes
import scala.tools.asm.tree.{AbstractInsnNode, ClassNode, MethodNode}
import scala.tools.cmd.CommandLineParser
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.settings.MutableSettings
import scala.tools.nsc.{Settings, Global}
import scala.tools.partest.ASMConverters
import scala.collection.JavaConverters._
import scala.tools.testing.TempDir
import AsmUtils._

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
    val compiler = newCompilerWithoutVirtualOutdir(defaultArgs, extraArgs)
    resetOutput(compiler)
    compiler
  }
  
  def newCompilerWithoutVirtualOutdir(defaultArgs: String = "-usejavacp", extraArgs: String = ""): Global = {
    val settings = new Settings()
    val args = (CommandLineParser tokenize defaultArgs) ++ (CommandLineParser tokenize extraArgs)
    settings.processArguments(args, processAll = true)
    new Global(settings, new StoreReporter)
  }

  def newRun(compiler: Global): compiler.Run = {
    compiler.reporter.reset()
    resetOutput(compiler)
    new compiler.Run()
  }

  def reporter(compiler: Global) = compiler.reporter.asInstanceOf[StoreReporter]

  def makeSourceFile(code: String, filename: String): BatchSourceFile = new BatchSourceFile(filename, code)

  def getGeneratedClassfiles(outDir: AbstractFile): List[(String, Array[Byte])] = {
    def files(dir: AbstractFile): List[(String, Array[Byte])] = {
      val res = ListBuffer.empty[(String, Array[Byte])]
      for (f <- dir.iterator) {
        if (!f.isDirectory) res += ((f.name, f.toByteArray))
        else if (f.name != "." && f.name != "..") res ++= files(f)
      }
      res.toList
    }
    files(outDir)
  }

  def checkReport(compiler: Global, allowMessage: StoreReporter#Info => Boolean = _ => false): Unit = {
    val disallowed = reporter(compiler).infos.toList.filter(!allowMessage(_)) // toList prevents an infer-non-wildcard-existential warning.
    if (disallowed.nonEmpty) {
      val msg = disallowed.mkString("\n")
      assert(false, "The compiler issued non-allowed warnings or errors:\n" + msg)
    }
  }

  def compile(compiler: Global)(scalaCode: String, javaCode: List[(String, String)] = Nil, allowMessage: StoreReporter#Info => Boolean = _ => false): List[(String, Array[Byte])] = {
    val run = newRun(compiler)
    run.compileSources(makeSourceFile(scalaCode, "unitTestSource.scala") :: javaCode.map(p => makeSourceFile(p._1, p._2)))
    checkReport(compiler, allowMessage)
    getGeneratedClassfiles(compiler.settings.outputDirs.getSingleOutput.get)
  }

  /**
   * Compile multiple Scala files separately into a single output directory.
   *
   * Note that a new compiler instance is created for compiling each file because symbols survive
   * across runs. This makes separate compilation slower.
   *
   * The output directory is a physical directory, I have not figured out if / how it's possible to
   * add a VirtualDirectory to the classpath of a compiler.
   */
  def compileSeparately(codes: List[String], extraArgs: String = "", allowMessage: StoreReporter#Info => Boolean = _ => false, afterEach: AbstractFile => Unit = _ => ()): List[(String, Array[Byte])] = {
    val outDir = AbstractFile.getDirectory(TempDir.createTempDir())
    val outDirPath = outDir.canonicalPath
    val argsWithOutDir = extraArgs + s" -d $outDirPath -cp $outDirPath"

    for (code <- codes) {
      val compiler = newCompilerWithoutVirtualOutdir(extraArgs = argsWithOutDir)
      new compiler.Run().compileSources(List(makeSourceFile(code, "unitTestSource.scala")))
      checkReport(compiler, allowMessage)
      afterEach(outDir)
    }

    val classfiles = getGeneratedClassfiles(outDir)
    outDir.delete()
    classfiles
  }

  def compileClassesSeparately(codes: List[String], extraArgs: String = "", allowMessage: StoreReporter#Info => Boolean = _ => false, afterEach: AbstractFile => Unit = _ => ()) = {
    readAsmClasses(compileSeparately(codes, extraArgs, allowMessage, afterEach))
  }

  def readAsmClasses(classfiles: List[(String, Array[Byte])]) = {
    classfiles.map(p => AsmUtils.readClass(p._2)).sortBy(_.name)
  }

  def compileClasses(compiler: Global)(code: String, javaCode: List[(String, String)] = Nil, allowMessage: StoreReporter#Info => Boolean = _ => false): List[ClassNode] = {
    readAsmClasses(compile(compiler)(code, javaCode, allowMessage))
  }

  def compileMethods(compiler: Global)(code: String, allowMessage: StoreReporter#Info => Boolean = _ => false): List[MethodNode] = {
    compileClasses(compiler)(s"class C { $code }", allowMessage = allowMessage).head.methods.asScala.toList.filterNot(_.name == "<init>")
  }

  def singleMethodInstructions(compiler: Global)(code: String, allowMessage: StoreReporter#Info => Boolean = _ => false): List[Instruction] = {
    val List(m) = compileMethods(compiler)(code, allowMessage = allowMessage)
    instructionsFromMethod(m)
  }

  def singleMethod(compiler: Global)(code: String, allowMessage: StoreReporter#Info => Boolean = _ => false): Method = {
    val List(m) = compileMethods(compiler)(code, allowMessage = allowMessage)
    convertMethod(m)
  }

  def assertSameCode(actual: List[Instruction], expected: List[Instruction]): Unit = {
    assertTrue(s"\nExpected: $expected\nActual  : $actual", actual === expected)
  }

  def getSingleMethod(classNode: ClassNode, name: String): Method =
    convertMethod(classNode.methods.asScala.toList.find(_.name == name).get)

  /**
   * Instructions that match `query` when textified.
   * If `query` starts with a `+`, the next instruction is returned.
   */
  def findInstr(method: MethodNode, query: String): List[AbstractInsnNode] = {
    val useNext = query(0) == '+'
    val instrPart = if (useNext) query.drop(1) else query
    val insns = method.instructions.iterator.asScala.find(i => textify(i) contains instrPart).toList
    if (useNext) insns.map(_.getNext) else insns
  }

  def assertHandlerLabelPostions(h: ExceptionHandler, instructions: List[Instruction], startIndex: Int, endIndex: Int, handlerIndex: Int): Unit = {
    val insVec = instructions.toVector
    assertTrue(h.start == insVec(startIndex) && h.end == insVec(endIndex) && h.handler == insVec(handlerIndex))
  }

  import scala.language.implicitConversions

  implicit def aliveInstruction(ins: Instruction): (Instruction, Boolean) = (ins, true)

  implicit class MortalInstruction(val ins: Instruction) extends AnyVal {
    def dead: (Instruction, Boolean) = (ins, false)
  }
}
