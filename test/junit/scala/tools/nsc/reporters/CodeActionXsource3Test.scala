package scala.tools.nsc.reporters

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class CodeActionXsource3Test extends AbstractCodeActionTest {
  override def compilerArgs: String = "-Ystop-after:typer -Yvalidate-pos:typer -Yrangepos -deprecation -Xsource:3"

  @Test
  def testLambdaParameterList(): Unit =
    assertCodeSuggestion(
      """trait Test {
        |  def foo: Any = {
        |    x: Int => x * 2
        |  }
        |  def bar: Any = {
        |    x: Int=> x * 2
        |  }
        |  def tavern: Any = { x: Int =>
        |    x * 2
        |  }
        |}
      """.stripMargin,
      """trait Test {
        |  def foo: Any = {
        |    (x: Int) => x * 2
        |  }
        |  def bar: Any = {
        |    (x: Int)=> x * 2
        |  }
        |  def tavern: Any = { (x: Int) =>
        |    x * 2
        |  }
        |}
      """.stripMargin,
    )
}
