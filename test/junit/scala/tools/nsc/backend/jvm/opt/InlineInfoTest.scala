package scala.tools.nsc
package backend.jvm
package opt

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.JavaConverters._
import scala.collection.generic.Clearable
import scala.tools.nsc.backend.jvm.BTypes.MethodInlineInfo
import scala.tools.nsc.backend.jvm.BackendReporting._
import scala.tools.testing.BytecodeTesting

@RunWith(classOf[JUnit4])
class InlineInfoTest extends BytecodeTesting {
  import compiler._
  import global.genBCode.bTypes

  override def compilerArgs = "-opt:l:classpath"

  def notPerRun: List[Clearable] = List(
    bTypes.classBTypeFromInternalName,
    bTypes.byteCodeRepository.compilingClasses,
    bTypes.byteCodeRepository.parsedClasses)
  notPerRun foreach global.perRunCaches.unrecordCache

  def compile(code: String) = {
    notPerRun.foreach(_.clear())
    compiler.compileClasses(code)
  }

  @Test
  def inlineInfosFromSymbolAndAttribute(): Unit = {
    val code =
      """trait T {
        |  @inline def f: Int
        |  @noinline final def g = 0
        |}
        |trait U { self: T =>
        |  @inline def f = 0
        |  final def h = 0
        |  final class K {
        |    @inline def i = 0
        |  }
        |}
        |sealed trait V {
        |  @inline def j = 0
        |}
        |class C extends T with U
      """.stripMargin
    val classes = compile(code)

    val fromSyms = classes.map(c => global.genBCode.bTypes.classBTypeFromInternalName(c.name).info.get.inlineInfo)

    val fromAttrs = classes.map(c => {
      assert(c.attrs.asScala.exists(_.isInstanceOf[InlineInfoAttribute]), c.attrs)
      global.genBCode.bTypes.inlineInfoFromClassfile(c)
    })

    assert(fromSyms == fromAttrs)
  }

  @Test // scala-dev#20
  def javaStaticMethodsInlineInfoInMixedCompilation(): Unit = {
    val jCode =
      """public class A {
        |  public static final int bar() { return 100; }
        |  public final int baz() { return 100; }
        |}
      """.stripMargin
    compileClasses("class C { new A }", javaCode = List((jCode, "A.java")))
    val info = global.genBCode.bTypes.classBTypeFromInternalName("A").info.get.inlineInfo
    assertEquals(info.methodInfos, Map(
      "bar()I"    -> MethodInlineInfo(true,false,false),
      "<init>()V" -> MethodInlineInfo(false,false,false),
      "baz()I"    -> MethodInlineInfo(true,false,false)))
  }
}
