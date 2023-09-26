package scala.tools.nsc.reporters

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class CodeActionXsource3Test extends AbstractCodeActionTest {
  override def compilerArgs: String = "-Ystop-after:refchecks -deprecation -Xlint -Xsource:3"

  @Test def t12860(): Unit = {
    assertCodeSuggestion(
      """class A {
        |  def f(x: Int) = new A
        |  def g(x: Int = 1.max(2)) = new A
        |  def h() = new A
        |  def i(h: Int)() = new A
        |  def j = new A
        |  def k[T] = new A
        |}
        |class B extends A {
        |  override def f(x: Int)
        |    = new B
        |  override def g(x: Int = 1.max(2)) = new B
        |  override def h() = new B
        |  override def i(h: Int)() = new B
        |  override val j =
        |    new B
        |  def k[T] = new B
        |}
        |""".stripMargin,
      """class A {
        |  def f(x: Int) = new A
        |  def g(x: Int = 1.max(2)) = new A
        |  def h() = new A
        |  def i(h: Int)() = new A
        |  def j = new A
        |  def k[T] = new A
        |}
        |class B extends A {
        |  override def f(x: Int): B
        |    = new B
        |  override def g(x: Int = 1.max(2)): B = new B
        |  override def h(): B = new B
        |  override def i(h: Int)(): B = new B
        |  override val j: B =
        |    new B
        |  def k[T]: B = new B
        |}
        |""".stripMargin)
  }

  @Test def t12882(): Unit = {
    assertCodeSuggestion(
      """|class Holiday(date: java.time.LocalDate)
         |
         |object HolidayConversion {
         |  implicit def dateToHoliday(date: java.time.LocalDate) = new Holiday(date)
         |}
         |""".stripMargin,
      """|class Holiday(date: java.time.LocalDate)
         |
         |object HolidayConversion {
         |  implicit def dateToHoliday(date: java.time.LocalDate): Holiday = new Holiday(date)
         |}
         |""".stripMargin)
  }

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
