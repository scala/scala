package scala.tools.nsc
package transform.patmat

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.asm.Opcodes._
import scala.tools.nsc.backend.jvm.AsmUtils._
import scala.tools.testkit.ASMConverters.Instruction
import scala.tools.testkit.BytecodeTesting
import scala.tools.testkit.BytecodeTesting._

import PartialFunction.cond

@RunWith(classOf[JUnit4])
class PatmatBytecodeTest extends BytecodeTesting {
  val optCompiler = cached("optCompiler", () => newCompiler(extraArgs = "-opt:inline:**"))

  import compiler._

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

    val c = compileClass(code)
    assert(getInstructions(c, "s1").count(_.opcode == TABLESWITCH) == 1, textify(c))
    assert(getInstructions(c, "s2").count(_.opcode == TABLESWITCH) == 1, textify(c))
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

    val c = compileClass(code)
    assert(getInstructions(c, "s1").count(_.opcode == TABLESWITCH) == 1, textify(c))
    assert(getInstructions(c, "s2").count(_.opcode == TABLESWITCH) == 1, textify(c))
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
    val c :: _ = optCompiler.compileClasses(code): @unchecked

    assertSameSummary(getMethod(c, "a"), List(
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
        |    case x              => throw new MatchError(x)
        |  }
        |}
      """.stripMargin
    val c :: _ = optCompiler.compileClasses(code): @unchecked
    assert(!getInstructions(c, "a").exists(i => i.opcode == IFNULL || i.opcode == IFNONNULL), textify(getAsmMethod(c, "a")))
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
    val c :: _ = optCompiler.compileClasses(code): @unchecked
    assertSameSummary(getMethod(c, "a"), List(
      NEW, DUP, ICONST_1, "valueOf", LDC, "<init>", ASTORE /*1*/,
      ALOAD /*1*/, "y", ASTORE /*2*/,
      ALOAD /*1*/, "x", INSTANCEOF, IFEQ /*E*/,
      ALOAD /*2*/, ARETURN,
      -1 /*E*/, NEW, DUP, ALOAD /*1*/, "<init>", ATHROW))
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
    val c = optCompiler.compileClass(code, allowMessage = _.msg.contains("may not be exhaustive"))

    val expected = List[Any](
      ALOAD /*1*/ , INSTANCEOF /*::*/ , IFEQ /*A*/ ,
      ALOAD, CHECKCAST /*::*/ , "head", "unboxToInt", IRETURN,
      -1 /*A*/ , NEW /*MatchError*/ , DUP, ALOAD /*1*/ , "<init>", ATHROW)

    assertSameSummary(getMethod(c, "a"), expected)
    assertSameSummary(getMethod(c, "b"), expected)
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
    val List(c, cMod) = optCompiler.compileClasses(code)
    assertSameSummary(getMethod(c, "t1"), List(ICONST_1, ICONST_2, IADD, IRETURN))
    assertSameSummary(getMethod(c, "t2"), List(ICONST_1, IRETURN))
    assertInvokedMethods(getMethod(c, "t3"), List("C.tplCall", "scala/Tuple2._1", "scala/Tuple2._2$mcI$sp", "scala/MatchError.<init>", "java/lang/String.length"))
    assertInvokedMethods(getMethod(c, "t4"), List("C.tplCall", "scala/Tuple2._2$mcI$sp", "scala/MatchError.<init>"))
    assertNoInvoke(getMethod(c, "t5"))
    assertSameSummary(getMethod(c, "t6"), List(BIPUSH, IRETURN))

    // MatchError reachable because of the type pattern `s: String`
    assertInvokedMethods(getMethod(c, "t7"), List("C.a", "C.b", "scala/MatchError.<init>", "java/lang/String.length"))
    assertSameSummary(getMethod(c, "t8"), List(ALOAD, "b", IRETURN))
    // C allocation not eliminated - constructor may have side-effects.
    assertSameSummary(getMethod(c, "t9"), List(NEW, DUP, LDC, BIPUSH, "<init>", "a", "toString", ARETURN))
  }

  @Test
  def stringSwitch(): Unit = {
    val code =
      """import annotation.switch
        |class Switches {
        |  val cond = true
        |  def two   = ("foo" : @switch) match { case "foo" => case "bar" =>                   }
        |  def guard = ("foo" : @switch) match { case "z"   => case "y"   => case x if cond => }
        |  def colli = ("foo" : @switch) match { case "DB" =>  case "Ca"  =>                   }
        |}
      """.stripMargin
    val List(switches) = compiler.compileClasses(code)
    def isSwitchInsn(insn: Instruction) = cond(insn.opcode) { case LOOKUPSWITCH | TABLESWITCH => true }
    List("two", "guard", "colli") foreach { m =>
      assert(getInstructions(switches, m).exists(isSwitchInsn))
    }
  }
}
