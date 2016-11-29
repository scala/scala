package scala.tools.testing

import junit.framework.AssertionFailedError
import org.junit.Assert._

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.VirtualDirectory
import scala.tools.asm.Opcodes
import scala.tools.asm.tree.{AbstractInsnNode, ClassNode, MethodNode}
import scala.tools.cmd.CommandLineParser
import scala.tools.nsc.backend.jvm.AsmUtils
import scala.tools.nsc.backend.jvm.AsmUtils._
import scala.tools.nsc.backend.jvm.opt.BytecodeUtils
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.{Global, Settings}
import scala.tools.partest.ASMConverters._

trait BytecodeTesting extends ClearAfterClass {
  def compilerArgs = "" // to be overridden
  val compiler = cached("compiler", () => BytecodeTesting.newCompiler(extraArgs = compilerArgs))
}

class Compiler(val global: Global) {
  import BytecodeTesting._

  def resetOutput(): Unit = {
    global.settings.outputDirs.setSingleOutput(new VirtualDirectory("(memory)", None))
  }

  def newRun: global.Run = {
    global.reporter.reset()
    resetOutput()
    new global.Run()
  }

  private def reporter = global.reporter.asInstanceOf[StoreReporter]

  def checkReport(allowMessage: StoreReporter#Info => Boolean = _ => false): Unit = {
    val disallowed = reporter.infos.toList.filter(!allowMessage(_)) // toList prevents an infer-non-wildcard-existential warning.
    if (disallowed.nonEmpty) {
      val msg = disallowed.mkString("\n")
      assert(false, "The compiler issued non-allowed warnings or errors:\n" + msg)
    }
  }

  def compileToBytes(scalaCode: String, javaCode: List[(String, String)] = Nil, allowMessage: StoreReporter#Info => Boolean = _ => false): List[(String, Array[Byte])] = {
    val run = newRun
    run.compileSources(makeSourceFile(scalaCode, "unitTestSource.scala") :: javaCode.map(p => makeSourceFile(p._1, p._2)))
    checkReport(allowMessage)
    getGeneratedClassfiles(global.settings.outputDirs.getSingleOutput.get)
  }

  def compileClasses(code: String, javaCode: List[(String, String)] = Nil, allowMessage: StoreReporter#Info => Boolean = _ => false): List[ClassNode] = {
    readAsmClasses(compileToBytes(code, javaCode, allowMessage))
  }

  def compileClass(code: String, javaCode: List[(String, String)] = Nil, allowMessage: StoreReporter#Info => Boolean = _ => false): ClassNode = {
    val List(c) = compileClasses(code, javaCode, allowMessage)
    c
  }

  def compileToBytesTransformed(scalaCode: String, javaCode: List[(String, String)] = Nil, beforeBackend: global.Tree => global.Tree): List[(String, Array[Byte])] = {
    import global._
    settings.stopBefore.value = "jvm" :: Nil
    val run = newRun
    val scalaUnit = newCompilationUnit(scalaCode, "unitTestSource.scala")
    val javaUnits = javaCode.map(p => newCompilationUnit(p._1, p._2))
    val units = scalaUnit :: javaUnits
    run.compileUnits(units, run.parserPhase)
    settings.stopBefore.value = Nil
    scalaUnit.body = beforeBackend(scalaUnit.body)
    checkReport(_ => false)
    val run1 = newRun
    run1.compileUnits(units, run1.phaseNamed("jvm"))
    checkReport(_ => false)
    getGeneratedClassfiles(settings.outputDirs.getSingleOutput.get)
  }

  def compileClassesTransformed(scalaCode: String, javaCode: List[(String, String)] = Nil, beforeBackend: global.Tree => global.Tree): List[ClassNode] =
    readAsmClasses(compileToBytesTransformed(scalaCode, javaCode, beforeBackend))

  def compileAsmMethods(code: String, allowMessage: StoreReporter#Info => Boolean = _ => false): List[MethodNode] = {
    val c = compileClass(s"class C { $code }", allowMessage = allowMessage)
    getAsmMethods(c, _ != "<init>")
  }

  def compileAsmMethod(code: String, allowMessage: StoreReporter#Info => Boolean = _ => false): MethodNode = {
    val List(m) = compileAsmMethods(code, allowMessage)
    m
  }

  def compileMethods(code: String, allowMessage: StoreReporter#Info => Boolean = _ => false): List[Method] =
    compileAsmMethods(code, allowMessage).map(convertMethod)

  def compileMethod(code: String, allowMessage: StoreReporter#Info => Boolean = _ => false): Method = {
    val List(m) = compileMethods(code, allowMessage = allowMessage)
    m
  }

  def compileInstructions(code: String, allowMessage: StoreReporter#Info => Boolean = _ => false): List[Instruction] = {
    val List(m) = compileMethods(code, allowMessage = allowMessage)
    m.instructions
  }
}

object BytecodeTesting {
  def genMethod(flags: Int = Opcodes.ACC_PUBLIC,
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

  def newCompiler(defaultArgs: String = "-usejavacp", extraArgs: String = ""): Compiler = {
    val compiler = newCompilerWithoutVirtualOutdir(defaultArgs, extraArgs)
    compiler.resetOutput()
    compiler
  }

  def newCompilerWithoutVirtualOutdir(defaultArgs: String = "-usejavacp", extraArgs: String = ""): Compiler = {
    def showError(s: String) = throw new Exception(s)
    val settings = new Settings(showError)
    val args = (CommandLineParser tokenize defaultArgs) ++ (CommandLineParser tokenize extraArgs)
    val (_, nonSettingsArgs) = settings.processArguments(args, processAll = true)
    if (nonSettingsArgs.nonEmpty) showError("invalid compiler flags: " + nonSettingsArgs.mkString(" "))
    new Compiler(new Global(settings, new StoreReporter))
  }

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

  /**
   * Compile multiple Scala files separately into a single output directory.
   *
   * Note that a new compiler instance is created for compiling each file because symbols survive
   * across runs. This makes separate compilation slower.
   *
   * The output directory is a physical directory, I have not figured out if / how it's possible to
   * add a VirtualDirectory to the classpath of a compiler.
   */
  def compileToBytesSeparately(codes: List[String], extraArgs: String = "", allowMessage: StoreReporter#Info => Boolean = _ => false, afterEach: AbstractFile => Unit = _ => ()): List[(String, Array[Byte])] = {
    val outDir = AbstractFile.getDirectory(TempDir.createTempDir())
    val outDirPath = outDir.canonicalPath
    val argsWithOutDir = extraArgs + s" -d $outDirPath -cp $outDirPath"

    for (code <- codes) {
      val compiler = newCompilerWithoutVirtualOutdir(extraArgs = argsWithOutDir)
      new compiler.global.Run().compileSources(List(makeSourceFile(code, "unitTestSource.scala")))
      compiler.checkReport(allowMessage)
      afterEach(outDir)
    }

    val classfiles = getGeneratedClassfiles(outDir)
    outDir.delete()
    classfiles
  }

  def compileClassesSeparately(codes: List[String], extraArgs: String = "", allowMessage: StoreReporter#Info => Boolean = _ => false, afterEach: AbstractFile => Unit = _ => ()): List[ClassNode] = {
    readAsmClasses(compileToBytesSeparately(codes, extraArgs, allowMessage, afterEach))
  }

  def readAsmClasses(classfiles: List[(String, Array[Byte])]) = classfiles.map(p => AsmUtils.readClass(p._2)).sortBy(_.name)

  def assertSameCode(method: Method, expected: List[Instruction]): Unit = assertSameCode(method.instructions.dropNonOp, expected)
  def assertSameCode(actual: List[Instruction], expected: List[Instruction]): Unit = {
    assert(actual === expected, s"\nExpected: $expected\nActual  : $actual")
  }

  def assertSameSummary(method: Method, expected: List[Any]): Unit = assertSameSummary(method.instructions, expected)
  def assertSameSummary(actual: List[Instruction], expected: List[Any]): Unit = {
    def expectedString = expected.map({
      case s: String => s""""$s""""
      case i: Int => opcodeToString(i, i)
    }).mkString("List(", ", ", ")")
    assert(actual.summary == expected, s"\nFound   : ${actual.summaryText}\nExpected: $expectedString")
  }

  def assertNoInvoke(m: Method): Unit = assertNoInvoke(m.instructions)
  def assertNoInvoke(ins: List[Instruction]): Unit = {
    assert(!ins.exists(_.isInstanceOf[Invoke]), ins.stringLines)
  }

  def assertInvoke(m: Method, receiver: String, method: String): Unit = assertInvoke(m.instructions, receiver, method)
  def assertInvoke(l: List[Instruction], receiver: String, method: String): Unit = {
    assert(l.exists {
      case Invoke(_, `receiver`, `method`, _, _) => true
      case _ => false
    }, l.stringLines)
  }

  def assertDoesNotInvoke(m: Method, method: String): Unit = assertDoesNotInvoke(m.instructions, method)
  def assertDoesNotInvoke(l: List[Instruction], method: String): Unit = {
    assert(!l.exists {
      case i: Invoke => i.name == method
      case _ => false
    }, l.stringLines)
  }

  def assertInvokedMethods(m: Method, expected: List[String]): Unit = assertInvokedMethods(m.instructions, expected)
  def assertInvokedMethods(l: List[Instruction], expected: List[String]): Unit = {
    def quote(l: List[String]) = l.map(s => s""""$s"""").mkString("List(", ", ", ")")
    val actual = l collect { case i: Invoke => i.owner + "." + i.name }
    assert(actual == expected, s"\nFound   : ${quote(actual)}\nExpected: ${quote(expected)}")
  }

  def assertNoIndy(m: Method): Unit = assertNoIndy(m.instructions)
  def assertNoIndy(l: List[Instruction]) = {
    val indy = l collect { case i: InvokeDynamic => i }
    assert(indy.isEmpty, indy)
  }

  def findClass(cs: List[ClassNode], name: String): ClassNode = {
    val List(c) = cs.filter(_.name == name)
    c
  }

  def getAsmMethods(c: ClassNode, p: String => Boolean): List[MethodNode] =
    c.methods.iterator.asScala.filter(m => p(m.name)).toList.sortBy(_.name)

  def getAsmMethods(c: ClassNode, name: String): List[MethodNode] =
    getAsmMethods(c, _ == name)

  def getAsmMethod(c: ClassNode, name: String): MethodNode = {
    val methods = getAsmMethods(c, name)
    def fail() = {
      val allNames = getAsmMethods(c, _ => true).map(_.name)
      throw new AssertionFailedError(s"Could not find method named $name among ${allNames}")
    }
    methods match {
      case List(m) => m
      case ms @ List(m1, m2) if BytecodeUtils.isInterface(c) =>
        val (statics, nonStatics) = ms.partition(BytecodeUtils.isStaticMethod)
        (statics, nonStatics) match {
          case (List(staticMethod), List(_)) => m1 // prefer the static method of the pair if methods in traits
          case _ => fail()
        }
      case ms => fail()
    }
  }

  def getMethods(c: ClassNode, name: String): List[Method] =
    getAsmMethods(c, name).map(convertMethod)

  def getMethod(c: ClassNode, name: String): Method =
    convertMethod(getAsmMethod(c, name))

  def getInstructions(c: ClassNode, name: String): List[Instruction] =
    getMethod(c, name).instructions

  /**
   * Instructions that match `query` when textified.
   * If `query` starts with a `+`, the next instruction is returned.
   */
  def findInstrs(method: MethodNode, query: String): List[AbstractInsnNode] = {
    val useNext = query(0) == '+'
    val instrPart = if (useNext) query.drop(1) else query
    val insns = method.instructions.iterator.asScala.filter(i => textify(i) contains instrPart).toList
    if (useNext) insns.map(_.getNext) else insns
  }

  /**
   * Instruction that matches `query` when textified.
   * If `query` starts with a `+`, the next instruction is returned.
   */
  def findInstr(method: MethodNode, query: String): AbstractInsnNode = {
    val List(i) = findInstrs(method, query)
    i
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

  implicit class listStringLines[T](val l: List[T]) extends AnyVal {
    def stringLines = l.mkString("\n")
  }
}
