package scala.tools.nsc
package backend.jvm

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.partest.ASMConverters._
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
        |  // similar, but starting off with any2stringadd
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
        |         chrs: Array[Char]) = this + str + v + z + c + b + s + i + f + l + d + sbuf + chsq + chrs
        |}
      """.stripMargin
    val c = compileClass(code)

    def invokeNameDesc(m: String): List[String] = getInstructions(c, m) collect {
      case Invoke(_, _, name, desc, _) => name + desc
    }
    assertEquals(invokeNameDesc("t1"), List(
      "<init>()V",
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
      "toString()Ljava/lang/String;"))

    assertEquals(invokeNameDesc("t2"), List(
      "<init>()V",
      "any2stringadd(Ljava/lang/Object;)Ljava/lang/Object;",
      "$plus$extension(Ljava/lang/Object;Ljava/lang/String;)Ljava/lang/String;",
      "append(Ljava/lang/String;)Ljava/lang/StringBuilder;",
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
      "toString()Ljava/lang/String;"))
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
      val s1 = str + obj + v + z + c + b + s + i + f + l + d + sbuf + chsq + chrs
      val s2 = obj + str + v + z + c + b + s + i + f + l + d + sbuf + chsq + chrs
      s1 + "//" + s2
    }
    def sbuf = { val r = new java.lang.StringBuffer(); r.append("sbuf"); r }
    def chsq: java.lang.CharSequence = "chsq"
    val s = t((), true, 'd', 3: Byte, 12: Short, 3, -32l, 12.3f, -4.2d, "me", sbuf, chsq, Array('a', 'b'))
    val r = s.replaceAll("""\[C@\w+""", "<ARRAY>")
    assertEquals(r, "meTTT()trued312312.3-32-4.2sbufchsq<ARRAY>//TTTme()trued312312.3-32-4.2sbufchsq<ARRAY>")
  }
}
