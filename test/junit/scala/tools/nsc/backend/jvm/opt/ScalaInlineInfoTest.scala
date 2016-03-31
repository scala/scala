package scala.tools.nsc
package backend.jvm
package opt

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import scala.tools.asm.Opcodes._
import org.junit.Assert._

import CodeGenTools._
import scala.tools.asm.tree.ClassNode
import scala.tools.nsc.backend.jvm.BTypes.{MethodInlineInfo, InlineInfo}
import scala.tools.partest.ASMConverters
import ASMConverters._
import scala.collection.convert.decorateAsScala._
import scala.tools.testing.ClearAfterClass

object ScalaInlineInfoTest extends ClearAfterClass.Clearable {
  var compiler = newCompiler(extraArgs = "-Yopt:l:none")
  def clear(): Unit = { compiler = null }
}

@RunWith(classOf[JUnit4])
class ScalaInlineInfoTest extends ClearAfterClass {
  ClearAfterClass.stateToClear = ScalaInlineInfoTest

  val compiler = newCompiler()

  def inlineInfo(c: ClassNode): InlineInfo = c.attrs.asScala.collect({ case a: InlineInfoAttribute => a.inlineInfo }).head

  @Test
  def traitMembersInlineInfo(): Unit = {
    val code =
      """trait T {
        |  def f1 = 1                   // concrete method
        |  private def f2 = 1           // implOnly method (does not end up in the interface)
        |  def f3 = {
        |    def nest = 0               // nested method (does not end up in the interface)
        |    nest
        |  }
        |
        |  @inline
        |  def f4 = super.toString      // super accessor
        |
        |  object O                     // module accessor (method is generated)
        |  def f5 = {
        |    object L { val x = 0 }     // nested module (just flattened out)
        |    L.x
        |  }
        |
        |  @noinline
        |  def f6: Int                  // abstract method (not in impl class)
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
      """.stripMargin

    val cs @ List(t, tl, to) = compileClasses(compiler)(code)
    val info = inlineInfo(t)
    val expect = InlineInfo (
      None,  // self type
      false, // final class
      None, // not a sam
      Map(
        // TODO SD-86: the module accessor used to be `effectivelyFinal` before nuke-impl-classes
        ("O()LT$O$;",                                                MethodInlineInfo(false,false,false,false)),
        ("T$$super$toString()Ljava/lang/String;",                    MethodInlineInfo(false,false,false,false)),
        ("T$_setter_$x1_$eq(I)V",                                    MethodInlineInfo(false,false,false,false)),
        ("f1()I",                                                    MethodInlineInfo(false,false,false,false)),
        ("f3()I",                                                    MethodInlineInfo(false,false,false,false)),
        ("f4()Ljava/lang/String;",                                   MethodInlineInfo(false,false,true, false)),
        ("f5()I",                                                    MethodInlineInfo(false,false,false,false)),
        ("f6()I",                                                    MethodInlineInfo(false,false,false,true )),
        ("x1()I",                                                    MethodInlineInfo(false,false,false,false)),
        ("x3()I",                                                    MethodInlineInfo(false,false,false,false)),
        ("x3_$eq(I)V",                                               MethodInlineInfo(false,false,false,false)),
        ("x4()I",                                                    MethodInlineInfo(false,false,false,false)),
        ("x5()I",                                                    MethodInlineInfo(true, false,false,false)),
        ("y2()I",                                                    MethodInlineInfo(false,false,false,false)),
        ("y2_$eq(I)V",                                               MethodInlineInfo(false,false,false,false)),
        ("f2()I",                                                    MethodInlineInfo(true, false,false,false)),
        ("L$lzycompute$1(Lscala/runtime/VolatileObjectRef;)LT$L$2$;",MethodInlineInfo(true, false,false,false)),
        // TODO SD-86: should probably be effectivelyFinal
        ("L$1(Lscala/runtime/VolatileObjectRef;)LT$L$2$;",           MethodInlineInfo(false,false,false,false)),
        ("nest$1()I",                                                MethodInlineInfo(true, false,false,false)),
        ("$init$()V",                                                MethodInlineInfo(false,false,false,false))),
      None // warning
    )
    assert(info == expect, info)
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
    val cs = compileClasses(compiler)(code)
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
}
