package scala.tools.nsc
package backend.jvm

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.ASMConverters._
import scala.tools.testing.BytecodeTesting
import scala.tools.testing.BytecodeTesting._

@RunWith(classOf[JUnit4])
class StringConcatTest extends BytecodeTesting {
  import compiler._

  @Test
  def appendOverloadNoBoxing(): Unit = {
    val code =
      """class C {
        |  def t1(
        |         v: Unit,
        |         z: Boolean,
        |         c: Char,
        |         b: Byte,
        |         s: Short,
        |         i: Int,
        |         l: Long,
        |         f: Float,
        |         d: Double,
        |         str: String,
        |         sbuf: java.lang.StringBuffer,
        |         chsq: java.lang.CharSequence,
        |         chrs: Array[Char]) = str + this + v + z + c + b + s + i + f + l + d + sbuf + chsq + chrs
        |
        |  def t2(
        |         v: Unit,
        |         z: Boolean,
        |         c: Char,
        |         b: Byte,
        |         s: Short,
        |         i: Int,
        |         l: Long,
        |         f: Float,
        |         d: Double,
        |         str: String,
        |         sbuf: java.lang.StringBuffer,
        |         chsq: java.lang.CharSequence,
        |         chrs: Array[Char]) = s"$str$this$v$z$c$b$s$i$f$l$d$sbuf$chsq$chrs"
        |
        |  def t3(
        |         v: Unit,
        |         z: Boolean,
        |         c: Char,
        |         b: Byte,
        |         s: Short,
        |         i: Int,
        |         l: Long,
        |         f: Float,
        |         d: Double,
        |         str: String,
        |         sbuf: java.lang.StringBuffer,
        |         chsq: java.lang.CharSequence,
        |         chrs: Array[Char]) = raw"$str$this$v$z$c$b$s$i$f$l$d$sbuf$chsq$chrs"
        |
        |}
      """.stripMargin
    val c = compileClass(code)

    def invokeNameDesc(m: String): List[String] = getInstructions(c, m) collect {
      case Invoke(_, _, name, desc, _) => name + desc
    }
    val t1Expected = List(
      "<init>(I)V",
      "append(Ljava/lang/String;)Ljava/lang/StringBuilder;",
      "append(Ljava/lang/Object;)Ljava/lang/StringBuilder;",
      "append(Ljava/lang/Object;)Ljava/lang/StringBuilder;",
      "append(Z)Ljava/lang/StringBuilder;",
      "append(C)Ljava/lang/StringBuilder;",
      "append(I)Ljava/lang/StringBuilder;",
      "append(I)Ljava/lang/StringBuilder;",
      "append(I)Ljava/lang/StringBuilder;",
      "append(F)Ljava/lang/StringBuilder;",
      "append(J)Ljava/lang/StringBuilder;",
      "append(D)Ljava/lang/StringBuilder;",
      "append(Ljava/lang/StringBuffer;)Ljava/lang/StringBuilder;",
      "append(Ljava/lang/CharSequence;)Ljava/lang/StringBuilder;",
      "append(Ljava/lang/Object;)Ljava/lang/StringBuilder;", // test that we're not using the [C overload
      "toString()Ljava/lang/String;")
    assertEquals(invokeNameDesc("t1"), t1Expected)

    // intrinsics for StringContext.{raw,s}
    assertEquals(invokeNameDesc("t2"), t1Expected)
    assertEquals(invokeNameDesc("t3"), t1Expected)
  }

  @Test
  def concatPrimitiveCorrectness(): Unit = {
    val obj: Object = new { override def toString = "TTT" }
    def t(
           v: Unit,
           z: Boolean,
           c: Char,
           b: Byte,
           s: Short,
           i: Int,
           l: Long,
           f: Float,
           d: Double,
           str: String,
           sbuf: java.lang.StringBuffer,
           chsq: java.lang.CharSequence,
           chrs: Array[Char]) = {
      str + obj + v + z + c + b + s + i + f + l + d + sbuf + chsq + chrs
    }
    def sbuf = { val r = new java.lang.StringBuffer(); r.append("sbuf"); r }
    def chsq: java.lang.CharSequence = "chsq"
    val s = t((), true, 'd', 3: Byte, 12: Short, 3, -32l, 12.3f, -4.2d, "me", sbuf, chsq, Array('a', 'b'))
    val r = s.replaceAll("""\[C@\w+""", "<ARRAY>")
    assertEquals(r, "meTTT()trued312312.3-32-4.2sbufchsq<ARRAY>")
  }
}
