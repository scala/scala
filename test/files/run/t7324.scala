
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import scala.reflect.internal.util.ScalaClassLoader
import scala.tools.partest.StoreReporterDirectTest

object Test extends StoreReporterDirectTest {

  //override def extraSettings: String = "-usejavacp -Xlint"

  def code = classOfArgs(254)

  def show() = {
    val compiler = newCompiler()
    assertTrue(compileString(compiler)(code))
    load()
    assertFalse(compileString(compiler)(classOfArgs(255)))
    check()
    assertTrue(compileString(compiler)(methodOfArgs(254)))
    load()
    assertFalse(compileString(compiler)(methodOfArgs(255)))
    check()
    assertFalse(compileString(compiler)(methodOfArgs(254, "Long")))
    check(isLongly = true)
    assertTrue(compileString(compiler)(methodOfArgs(254/2, "Long")))
    load()
    assertFalse(compileString(compiler)(methodOfArgs(254/2 + 1, "Long")))
    check(isLongly = true)
    assertFalse(compileString(compiler)(methodOfArgs(254, "Double")))
    check(isLongly = true)
    assertTrue(compileString(compiler)(methodOfArgs(254/2, "Double")))
    load()
    assertFalse(compileString(compiler)(methodOfArgs(254/2 + 1, "Double")))
    check(isLongly = true)
  }

  private def check(isLongly: Boolean = false) = {
    assertTrue("Expected diagnostics", storeReporter.infos.nonEmpty)
    val expected =
      if (isLongly) "Platform restriction: a parameter list's length cannot exceed 254 (Long and Double count as 2)."
      else "Platform restriction: a parameter list's length cannot exceed 254."
    storeReporter.infos.foreach(info => assertEquals(expected, info.msg))
  }
  private def load() = {
    val loader = ScalaClassLoader.fromURLs(Seq(testOutput.jfile.toURI.toURL))
    assertFalse("Expected loadable class B", loader.tryToLoadClass("B").isEmpty)
  }

  private def classOfArgs(n: Int, t: String = "Int") = s"class B${mkArgs(n, t)}"

  private def methodOfArgs(n: Int, t: String = "Int") = s"class B { def m${mkArgs(n, t)}: Unit = () }"

  private def mkArgs(n: Int, t: String) = 1.to(n).map(i => s"x$i: $t").mkString("(",",",")")
}
