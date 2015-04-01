package scala.tools.nsc
package backend.jvm
package opt

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import scala.tools.asm.Opcodes._
import org.junit.Assert._

import CodeGenTools._
import scala.tools.nsc.backend.jvm.BTypes.{MethodInlineInfo, InlineInfo}
import scala.tools.partest.ASMConverters
import ASMConverters._
import scala.collection.convert.decorateAsScala._

object ScalaInlineInfoTest {
  var compiler = newCompiler(extraArgs = "-Ybackend:GenBCode -Yopt:l:none")
  def clear(): Unit = { compiler = null }
}

@RunWith(classOf[JUnit4])
class ScalaInlineInfoTest {
  val compiler = newCompiler()

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

    val cs @ List(t, tl, to, tCls) = compileClasses(compiler)(code)
    val List(info) = t.attrs.asScala.collect({ case a: InlineInfoAttribute => a.inlineInfo }).toList
    val expect = InlineInfo(
      None,  // self type
      false, // final class
      Map(
        ("O()LT$O$;",                            MethodInlineInfo(true, false,false,false)),
        ("T$$super$toString()Ljava/lang/String;",MethodInlineInfo(false,false,false,false)),
        ("T$_setter_$x1_$eq(I)V",                MethodInlineInfo(false,false,false,false)),
        ("f1()I",                                MethodInlineInfo(false,true, false,false)),
        ("f3()I",                                MethodInlineInfo(false,true, false,false)),
        ("f4()Ljava/lang/String;",               MethodInlineInfo(false,true, true, false)),
        ("f5()I",                                MethodInlineInfo(false,true, false,false)),
        ("f6()I",                                MethodInlineInfo(false,false,false,true )),
        ("x1()I",                                MethodInlineInfo(false,false,false,false)),
        ("x3()I",                                MethodInlineInfo(false,false,false,false)),
        ("x3_$eq(I)V",                           MethodInlineInfo(false,false,false,false)),
        ("x4()I",                                MethodInlineInfo(false,false,false,false)),
        ("x5()I",                                MethodInlineInfo(true, false,false,false)),
        ("y2()I",                                MethodInlineInfo(false,false,false,false)),
        ("y2_$eq(I)V",                           MethodInlineInfo(false,false,false,false))),
      None // warning
    )
    assert(info == expect, info)
  }
}
