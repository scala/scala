package scala.tools.nsc
package backend.jvm
package opt

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import scala.collection.generic.Clearable
import org.junit.Assert._

import CodeGenTools._
import scala.tools.partest.ASMConverters
import ASMConverters._
import AsmUtils._
import scala.tools.testing.ClearAfterClass

import BackendReporting._

import scala.collection.convert.decorateAsScala._

object InlineInfoTest extends ClearAfterClass.Clearable {
  var compiler = newCompiler(extraArgs = "-Ybackend:GenBCode -Yopt:l:classpath")
  def clear(): Unit = { compiler = null }

  def notPerRun: List[Clearable] = List(compiler.genBCode.bTypes.classBTypeFromInternalName, compiler.genBCode.bTypes.byteCodeRepository.classes)
  notPerRun foreach compiler.perRunCaches.unrecordCache
}

@RunWith(classOf[JUnit4])
class InlineInfoTest {
  val compiler = InlineInfoTest.compiler

  def compile(code: String) = {
    InlineInfoTest.notPerRun.foreach(_.clear())
    compileClasses(compiler)(code)
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
    val fromSyms = classes.map(c => compiler.genBCode.bTypes.classBTypeFromInternalName(c.name).info.get.inlineInfo)

    val fromAttrs = classes.map(c => {
      assert(c.attrs.asScala.exists(_.isInstanceOf[InlineInfoAttribute]), c.attrs)
      compiler.genBCode.bTypes.inlineInfoFromClassfile(c)
    })

    assert(fromSyms == fromAttrs)
  }
}
