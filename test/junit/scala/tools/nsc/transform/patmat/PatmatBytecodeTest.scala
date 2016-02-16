package scala.tools.nsc
package transform.patmat

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import scala.tools.asm.Opcodes._
import org.junit.Assert._

import scala.tools.nsc.backend.jvm.AsmUtils._
import scala.tools.nsc.backend.jvm.CodeGenTools
import scala.tools.testing.AssertUtil._

import CodeGenTools._
import scala.tools.partest.ASMConverters
import ASMConverters._
import scala.tools.testing.ClearAfterClass

object PatmatBytecodeTest extends ClearAfterClass.Clearable {
  var compiler = newCompiler()
  var optCompiler = newCompiler(extraArgs = "-Yopt:l:project")
  def clear(): Unit = { compiler = null; optCompiler = null }
}

@RunWith(classOf[JUnit4])
class PatmatBytecodeTest extends ClearAfterClass {
  ClearAfterClass.stateToClear = PatmatBytecodeTest

  val compiler = PatmatBytecodeTest.compiler
  val optCompiler = PatmatBytecodeTest.optCompiler

  @Test
  def t6956(): Unit = {
    val code =
      """class C {
        |  private[this] final val ONE = 1
        |
        |  def s1(i: Byte): Int = i match {
        |    case ONE => 1
        |    case 2 => 2
        |    case 3 => 3
        |    case _ => 0
        |  }
        |
        |  def s2(i: Byte): Int = i match {
        |    case 1 => 1
        |    case 2 => 2
        |    case 3 => 3
        |    case _ => 0
        |  }
        |}
      """.stripMargin

    val List(c) = compileClasses(compiler)(code)
    assert(getSingleMethod(c, "s1").instructions.count(_.opcode == TABLESWITCH) == 1, textify(c))
    assert(getSingleMethod(c, "s2").instructions.count(_.opcode == TABLESWITCH) == 1, textify(c))
  }

  @Test
  def t6955(): Unit = {
    val code =
      """class C {
        |  type Tag = Byte
        |
        |  def s1(i: Tag): Int = i match { // notice type of i is Tag = Byte
        |    case 1 => 1
        |    case 2 => 2
        |    case 3 => 3
        |    case _ => 0
        |  }
        |
        |  // this worked before, should keep working
        |  def s2(i: Byte): Int = i match {
        |    case 1 => 1
        |    case 2 => 2
        |    case 3 => 3
        |    case _ => 0
        |  }
        |}
      """.stripMargin

    val List(c) = compileClasses(compiler)(code)
    assert(getSingleMethod(c, "s1").instructions.count(_.opcode == TABLESWITCH) == 1, textify(c))
    assert(getSingleMethod(c, "s2").instructions.count(_.opcode == TABLESWITCH) == 1, textify(c))
  }

  @Test
  def optNoPrimitiveTypetest(): Unit = {
    val code =
      """case class Foo(x: Int, y: String)
        |class C {
        |  def a = Foo(1, "a") match {
        |    case Foo(_: Int, y) => y
        |  }
        |}
      """.stripMargin
    val c = compileClasses(optCompiler)(code).head

    assertSameSummary(getSingleMethod(c, "a"), List(
      NEW, DUP, ICONST_1, LDC, "<init>",
      "y", ARETURN))
  }

  @Test
  def optNoNullCheck(): Unit = {
    val code =
      """case class Foo(x: Any)
        |class C {
        |  def a = (Foo(1): Any) match {
        |    case Foo(_: String) =>
        |  }
        |}
      """.stripMargin
    val c = compileClasses(optCompiler)(code).head
    assert(!getSingleMethod(c, "a").instructions.exists(i => i.opcode == IFNULL || i.opcode == IFNONNULL), textify(findAsmMethod(c, "a")))
  }

  @Test
  def optNoLoacalForUnderscore(): Unit = {
    val code =
      """case class Foo(x: Any, y: String)
        |class C {
        |  def a = (Foo(1, "a"): @unchecked) match {
        |    case Foo(_: String, y) => y
        |  }
        |}
      """.stripMargin
    val c = compileClasses(optCompiler)(code).head
    assertSameSummary(getSingleMethod(c, "a"), List(
      NEW, DUP, ICONST_1, "boxToInteger", LDC, "<init>", ASTORE /*1*/,
      ALOAD /*1*/, "y", ASTORE /*2*/,
      ALOAD /*1*/, "x", INSTANCEOF, IFNE /*R*/,
      NEW, DUP, ALOAD /*1*/, "<init>", ATHROW,
      /*R*/ -1, ALOAD /*2*/, ARETURN))
  }

  @Test
  def t6941(): Unit = {
    val code =
      """class C {
        |  def a(xs: List[Int]) = xs match {
        |    case x :: _ => x
        |  }
        |  def b(xs: List[Int]) = xs match {
        |    case xs: ::[Int] => xs.head
        |  }
        |}
      """.stripMargin
    val c = compileClasses(optCompiler)(code, allowMessage = _.msg.contains("may not be exhaustive")).head

    val expected = List(
      ALOAD /*1*/ , INSTANCEOF /*::*/ , IFEQ /*A*/ ,
      ALOAD, CHECKCAST /*::*/ , "head", "unboxToInt",
      ISTORE, GOTO /*B*/ ,
      -1 /*A*/ , NEW /*MatchError*/ , DUP, ALOAD /*1*/ , "<init>", ATHROW,
      -1 /*B*/ , ILOAD, IRETURN)

    assertSameSummary(getSingleMethod(c, "a"), expected)
    assertSameSummary(getSingleMethod(c, "b"), expected)
  }

  @Test
  def valPatterns(): Unit = {
    val code =
      """case class C(a: Any, b: Int) {
        |  def tplCall = ("hi", 3)
        |  @inline final def tplInline = (true, 'z')
        |
        |  def t1 = { val (a, b) = (1, 2); a + b }
        |  def t2 = { val (a, _) = (1, 3); a }
        |  def t3 = { val (s, i) = tplCall; s.length + i }
        |  def t4 = { val (_, i) = tplCall; i }
        |  def t5 = { val (b, c) = tplInline; b || c == 'e' }
        |  def t6 = { val (_, c) = tplInline; c }
        |
        |  def t7 = { val C(s: String, b) = this; s.length + b }
        |  def t8 = { val C(_, b) = this; b }
        |  def t9 = { val C(a, _) = C("hi", 23); a.toString }
        |}
      """.stripMargin
    val List(c, cMod) = compileClasses(optCompiler)(code)
    assertSameSummary(getSingleMethod(c, "t1"), List(ICONST_1, ICONST_2, IADD, IRETURN))
    assertSameSummary(getSingleMethod(c, "t2"), List(ICONST_1, IRETURN))
    assertInvokedMethods(getSingleMethod(c, "t3"), List("C.tplCall", "scala/Tuple2._1", "scala/Tuple2._2$mcI$sp", "scala/MatchError.<init>", "java/lang/String.length"))
    assertInvokedMethods(getSingleMethod(c, "t4"), List("C.tplCall", "scala/Tuple2._2$mcI$sp", "scala/MatchError.<init>"))
    assertNoInvoke(getSingleMethod(c, "t5"))
    assertSameSummary(getSingleMethod(c, "t6"), List(BIPUSH, IRETURN))

    // MatchError reachable because of the type pattern `s: String`
    assertInvokedMethods(getSingleMethod(c, "t7"), List("C.a", "C.b", "scala/MatchError.<init>", "java/lang/String.length"))
    assertSameSummary(getSingleMethod(c, "t8"), List(ALOAD, "b", IRETURN))
    // C allocation not eliminated - constructor may have side-effects.
    assertSameSummary(getSingleMethod(c, "t9"), List(NEW, DUP, LDC, BIPUSH, "<init>", "a", "toString", ARETURN))
  }
}
