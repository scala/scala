import scala.tools.nsc.Settings
import scala.tools.partest.ReplTest

object Test extends ReplTest {
    override def transformSettings(s: Settings) = {
    s.Yreplclassbased.value = false // macros are object-based only
    s
  }

  override def extraSettings = "-language:experimental.macros"
  def code = """
    |def bar1(c: scala.reflect.macros.blackbox.Context) = ???
    |def foo1 = macro bar1
    |def bar2(c: scala.reflect.macros.whitebox.Context) = ???
    |def foo2 = macro bar2
    |""".stripMargin
}
