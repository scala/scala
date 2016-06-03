package scala.tools.nsc
package backend.jvm
package opt

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.JavaConverters._
import scala.tools.asm.tree.ClassNode
import scala.tools.nsc.backend.jvm.BTypes.{InlineInfo, MethodInlineInfo}
import scala.tools.testing.BytecodeTesting

@RunWith(classOf[JUnit4])
class ScalaInlineInfoTest extends BytecodeTesting {
  override def compilerArgs = "-opt:l:none"
  import compiler._

  def inlineInfo(c: ClassNode): InlineInfo = c.attrs.asScala.collect({ case a: InlineInfoAttribute => a.inlineInfo }).head

  def mapDiff[A, B](a: Map[A, B], b: Map[A, B]) = {
    val r = new StringBuilder
    for ((a, av) <- a) {
      if (!b.contains(a)) r.append(s"missing in b: $a\n")
      else if (av != b(a)) r.append(s"different for $a: $av != ${b(a)}\n")
    }
    for (b <- b.keys.toList diff a.keys.toList) {
      r.append(s"missing in a: $b\n")
    }
    r.toString
  }

  @Test
  def traitMembersInlineInfo(): Unit = {
    val code =
      """trait T {
        |  def f1 = 1                   // concrete method
        |  private def f2 = 1           // default method only (not in subclass)
        |  def f3 = {
        |    def nest = 0               // nested method (does not end up in the interface)
        |    nest
        |  }
        |
        |  @inline
        |  def f4 = super.toString      // super accessor
        |
        |  object O                     // module accessor (method is generated)
        |  final def f5 = {
        |    object L { val x = 0 }     // nested module (just flattened out)
        |    L.x
        |  }
        |
        |  @noinline
        |  def f6: Int                  // abstract method
        |
        |  // fields
        |
        |  val x1 = 0
        |  var y2 = 0
        |  var x3: Int
        |  lazy val x4 = 0
        |
        |  final val x5 = 0
        |}
        |class C extends T {
        |  def f6 = 0
        |  var x3 = 0
        |}
      """.stripMargin

    val cs @ List(c, t, tl, to) = compileClasses(code)
    val infoT = inlineInfo(t)
    val expectT = InlineInfo (
      false, // final class
      None, // not a sam
      Map(
        ("O()LT$O$;",                                                 MethodInlineInfo(false ,false,false)),
        ("T$$super$toString()Ljava/lang/String;",                     MethodInlineInfo(true ,false,false)),
        ("T$_setter_$x1_$eq(I)V",                                     MethodInlineInfo(false,false,false)),
        ("f1()I",                                                     MethodInlineInfo(false,false,false)),
        ("f2()I",                                                     MethodInlineInfo(true, false,false)),
        ("f3()I",                                                     MethodInlineInfo(false,false,false)),
        ("f4()Ljava/lang/String;",                                    MethodInlineInfo(false,true, false)),
        ("f5()I",                                                     MethodInlineInfo(true ,false,false)),
        ("f6()I",                                                     MethodInlineInfo(false,false,true )),
        ("x1()I",                                                     MethodInlineInfo(false,false,false)),
        ("y2()I",                                                     MethodInlineInfo(false,false,false)),
        ("y2_$eq(I)V",                                                MethodInlineInfo(false,false,false)),
        ("x3()I",                                                     MethodInlineInfo(false,false,false)),
        ("x3_$eq(I)V",                                                MethodInlineInfo(false,false,false)),
        ("x4()I",                                                     MethodInlineInfo(false,false,false)),
        ("x5()I",                                                     MethodInlineInfo(true, false,false)),
        ("L$lzycompute$1(Lscala/runtime/VolatileObjectRef;)LT$L$2$;", MethodInlineInfo(true, false,false)),
        ("T$$L$1(Lscala/runtime/VolatileObjectRef;)LT$L$2$;",            MethodInlineInfo(true ,false,false)),
        ("nest$1()I",                                                 MethodInlineInfo(true, false,false)),
        ("$init$()V",                                                 MethodInlineInfo(false,false,false))),
      None // warning
    )

    assert(infoT == expectT, mapDiff(expectT.methodInfos, infoT.methodInfos) + infoT)

    val infoC = inlineInfo(c)
    val expectC = InlineInfo(false, None, Map(
      "O()LT$O$;"                             -> MethodInlineInfo(true ,false,false),
      "O$lzycompute()LT$O$;"                  -> MethodInlineInfo(true, false,false),
      "f6()I"                                 -> MethodInlineInfo(false,false,false),
      "x1()I"                                 -> MethodInlineInfo(false,false,false),
      "T$_setter_$x1_$eq(I)V"                 -> MethodInlineInfo(false,false,false),
      "y2()I"                                 -> MethodInlineInfo(false,false,false),
      "y2_$eq(I)V"                            -> MethodInlineInfo(false,false,false),
      "x3()I"                                 -> MethodInlineInfo(false,false,false),
      "x3_$eq(I)V"                            -> MethodInlineInfo(false,false,false),
      "x4$lzycompute()I"                      -> MethodInlineInfo(true ,false,false),
      "x4()I"                                 -> MethodInlineInfo(false,false,false),
//      "x5()I"                                 -> MethodInlineInfo(true ,false,false), -- there is no x5 in the class as it's implemented fully in the interface
      "T$$super$toString()Ljava/lang/String;" -> MethodInlineInfo(true ,false,false),
      "<init>()V"                             -> MethodInlineInfo(false,false,false)),
      None)

    assert(infoC == expectC, mapDiff(expectC.methodInfos, infoC.methodInfos) + infoC)
  }

  @Test
  def inlineInfoSam(): Unit = {
    val code =
      """trait C { // expected to be seen as sam: g(I)I
        |  def f = 0
        |  def g(x: Int): Int
        |  val foo = "hi"
        |}
        |abstract class D {
        |  val biz: Int
        |}
        |trait T { // expected to be seen as sam: h(Ljava/lang/String;)I
        |  def h(a: String): Int
        |}
        |trait E extends T { // expected to be seen as sam: h(Ljava/lang/String;)I
        |  def hihi(x: Int) = x
        |}
        |class F extends T {
        |  def h(a: String) = 0
        |}
        |trait U {
        |  def conc() = 10
        |  def nullary: Int
        |}
      """.stripMargin
    val cs = compileClasses(code)
    val sams = cs.map(c => (c.name, inlineInfo(c).sam))
    assertEquals(sams,
      List(
        ("C",Some("g(I)I")),
        ("D",None),
        ("E",Some("h(Ljava/lang/String;)I")),
        ("F",None),
        ("T",Some("h(Ljava/lang/String;)I")),
        ("U",None)))

  }

  @Test
  def lzyComputeInlineInfo(): Unit = {
    val code = "class C { object O }"
    val List(c, om) = compileClasses(code)
    val infoC = inlineInfo(c)
    val expected = Map(
      "<init>()V"            -> MethodInlineInfo(false,false,false),
      "O$lzycompute()LC$O$;" -> MethodInlineInfo(true,false,false),
      "O()LC$O$;"            -> MethodInlineInfo(true,false,false))
    assert(infoC.methodInfos == expected, mapDiff(infoC.methodInfos, expected))
  }
}
