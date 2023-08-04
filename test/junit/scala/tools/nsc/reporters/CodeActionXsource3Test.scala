package scala.tools.nsc.reporters

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class CodeActionXsource3Test extends AbstractCodeActionTest {
  override def compilerArgs: String = "-Ystop-after:refchecks -deprecation -Xlint -Xsource:3"

  @Test
  def lambdaParameterParens(): Unit =
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

  @Test
  def qmark(): Unit = {
    assertCodeSuggestion("class C[?]", "class C[`?`]")
  }

  @Test
  def scala3Keyword(): Unit = {
    assertCodeSuggestion("class C { val export = 1 }", "class C { val `export` = 1 }")
  }

  @Test
  def infixTypeArg(): Unit = {
    assertCodeSuggestion("class C { List apply[Int] 1 }", "class C { List.apply[Int](1) }")
    assertCodeSuggestion("class C { List apply 1 map[Int] identity }", "class C { (List apply 1).map[Int](identity) }")
    assertCodeSuggestion("class C { List apply 1 map[Int] (_ + 1) }", "class C { (List apply 1).map[Int](_ + 1) }")
  }

  @Test
  def overrideInferredType(): Unit = {
    assertCodeSuggestion(
      """trait T { def f: Object }
        |class C extends T { def f = "" }
        |""".stripMargin,
      """trait T { def f: Object }
        |class C extends T { def f: String = "" }
        |""".stripMargin)
  }
}
