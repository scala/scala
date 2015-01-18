package scala.tools.nsc
package backend.jvm
package opt

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import scala.collection.generic.Clearable
import scala.tools.asm.Opcodes._
import org.junit.Assert._

import scala.tools.asm.tree._
import scala.tools.asm.tree.analysis._
import scala.tools.nsc.backend.jvm.opt.BytecodeUtils.BasicAnalyzer
import scala.tools.testing.AssertUtil._

import CodeGenTools._
import scala.tools.partest.ASMConverters
import ASMConverters._
import AsmUtils._

import scala.collection.convert.decorateAsScala._
import scala.tools.testing.ClearAfterClass

object InlinerTest extends ClearAfterClass.Clearable {
  var compiler = newCompiler(extraArgs = "-Ybackend:GenBCode -Yopt:l:project")

  // allows inspecting the caches after a compilation run
  def notPerRun: List[Clearable] = List(compiler.genBCode.bTypes.classBTypeFromInternalName, compiler.genBCode.bTypes.byteCodeRepository.classes)
  notPerRun foreach compiler.perRunCaches.unrecordCache

  def clear(): Unit = { compiler = null }
}

@RunWith(classOf[JUnit4])
class InlinerTest extends ClearAfterClass {
  ClearAfterClass.stateToClear = InlinerTest

  val compiler = InlinerTest.compiler
  import compiler.genBCode.bTypes._

  def compile(code: String): List[ClassNode] = {
    InlinerTest.notPerRun.foreach(_.clear())
    val cls = compileClasses(compiler)(code)
    // the compiler doesn't add classes being compiled to the code repo yet, so we do it manually.
    // this line is removed in the next commit.
    for (c <- cls) byteCodeRepository.classes(c.name) = (c, ByteCodeRepository.Classfile)
    cls
  }

  // inline first invocation of f into g in class C
  def inlineTest(code: String, mod: ClassNode => Unit = _ => ()): (MethodNode, Option[String]) = {
    val List(cls) = compile(code)
    mod(cls)
    val clsBType = classBTypeFromParsedClassfile(cls.name)

    val List(f, g) = cls.methods.asScala.filter(m => Set("f", "g")(m.name)).toList.sortBy(_.name)
    val fCall = g.instructions.iterator.asScala.collect({ case i: MethodInsnNode if i.name == "f" => i }).next()

    val analyzer = new BasicAnalyzer(g, clsBType.internalName)

    val r = inliner.inline(
      fCall,
      analyzer.frameAt(fCall).getStackSize,
      g,
      clsBType,
      f,
      clsBType,
      receiverKnownNotNull = true,
      keepLineNumbers = true)
    (g, r)
  }

  @Test
  def simpleInlineOK(): Unit = {
    val code =
      """class C {
        |  def f = 1
        |  def g = f + f
        |}
      """.stripMargin

    val (g, _) = inlineTest(code)

    val gConv = convertMethod(g)
    assertSameCode(gConv.instructions.dropNonOp,
      List(
        VarOp(ALOAD, 0), VarOp(ASTORE, 1), // store this
        Op(ICONST_1), VarOp(ISTORE, 2), Jump(GOTO, Label(10)), // store return value
        Label(10), VarOp(ILOAD, 2), // load return value
        VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "C", "f", "()I", false), Op(IADD), Op(IRETURN)))

    // line numbers are kept, so there's a line 2 (from the inlined f)
    assert(gConv.instructions exists {
      case LineNumber(2, _) => true
      case _ => false
    }, gConv.instructions.filter(_.isInstanceOf[LineNumber]))

    assert(gConv.localVars.map(_.name).sorted == List("f_this", "this"), gConv.localVars)
    assert(g.maxStack == 2 && g.maxLocals == 3, s"${g.maxLocals} - ${g.maxStack}")
  }

  @Test
  def nothingTypedOK(): Unit = {
    val code =
      """class C {
        |  def f: Nothing = ???
        |  def g: Int = { f; 1 }
        |}
      """.stripMargin

    // On the bytecode level, methods of type Nothing have return type Nothing$.
    // This can be treated like any other result object.

    // See also discussion around ATHROW in BCodeBodyBuilder

    val (g, _) = inlineTest(code)
    val expectedInlined = List(
      VarOp(ALOAD, 0), VarOp(ASTORE, 1), // store this
      Field(GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;"), Invoke(INVOKEVIRTUAL, "scala/Predef$", "$qmark$qmark$qmark", "()Lscala/runtime/Nothing$;", false)) // inlined call to ???

    assertSameCode(convertMethod(g).instructions.dropNonOp.take(4), expectedInlined)

    localOpt.methodOptimizations(g, "C")
    assertSameCode(convertMethod(g).instructions.dropNonOp,
      expectedInlined ++ List(VarOp(ASTORE, 2), VarOp(ALOAD, 2), Op(ATHROW)))
  }

  @Test
  def synchronizedNoInline(): Unit = {
    val code =
      """class C {
        |  def f: Int = 0
        |  def g: Int = f
        |}
      """.stripMargin

    val (_, can) = inlineTest(code, cls => {
      val f = cls.methods.asScala.find(_.name == "f").get
      f.access |= ACC_SYNCHRONIZED
    })
    assert(can.get contains "synchronized", can)
  }

  @Test
  def tryCatchOK(): Unit = {
    val code =
      """class C {
        |  def f: Int = try { 1 } catch { case _: Exception => 2 }
        |  def g = f + 1
        |}
      """.stripMargin
    val (_, r) = inlineTest(code)
    assert(r.isEmpty, r)
  }

  @Test
  def tryCatchNoInline(): Unit = {
    // cannot inline f: there's a value on g's stack. if f throws and enters the handler, all values
    // on the stack are removed, including the one of g's stack that we still need.
    val code =
      """class C {
        |  def f: Int = try { 1 } catch { case _: Exception => 2 }
        |  def g = println(f)
        |}
      """.stripMargin
    val (_, r) = inlineTest(code)
    assert(r.get contains "operand stack at the callsite", r)
  }

  @Test
  def illegalAccessNoInline(): Unit = {
    val code =
      """package a {
        |  class C {
        |    private def f: Int = 0
        |    def g: Int = f
        |  }
        |}
        |package b {
        |  class D {
        |    def h(c: a.C): Int = c.g + 1
        |  }
        |}
      """.stripMargin

    val List(c, d) = compile(code)

    val cTp = classBTypeFromParsedClassfile(c.name)
    val dTp = classBTypeFromParsedClassfile(d.name)

    val g = c.methods.asScala.find(_.name == "g").get
    val h = d.methods.asScala.find(_.name == "h").get
    val gCall = h.instructions.iterator.asScala.collect({
      case m: MethodInsnNode if m.name == "g" => m
    }).next()

    val analyzer = new BasicAnalyzer(h, dTp.internalName)

    val r = inliner.inline(
      gCall,
      analyzer.frameAt(gCall).getStackSize,
      h,
      dTp,
      g,
      cTp,
      receiverKnownNotNull = true,
      keepLineNumbers = true)

    assert(r.get contains "would cause an IllegalAccessError", r)
  }
}
