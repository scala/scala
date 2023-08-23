package scala.tools.nsc.reporters

import org.junit.Test
import org.junit.Assert._

import scala.reflect.internal.util.CodeAction
import scala.tools.testkit.BytecodeTesting
import scala.tools.testkit.Compiler

abstract class AbstractCodeActionTest extends BytecodeTesting {
  @Test
  def procedureSyntax(): Unit =
    assertCodeSuggestion(
      """trait Test {
        |  def foo
        |  def bar;
        |  def pub { println() }
        |  def club{ println() }
        |  def saloon = { println() }
        |}""".stripMargin,
      """trait Test {
        |  def foo: Unit
        |  def bar: Unit;
        |  def pub: Unit = { println() }
        |  def club: Unit ={ println() }
        |  def saloon = { println() }
        |}""".stripMargin,
      // disable lint to avoid conflicting patches with `nullaryUnit`
      BytecodeTesting.newCompiler(extraArgs = compilerArgs.replaceAll("-Xlint", "")))

  @Test
  def autoApplication(): Unit = {
    assertCodeSuggestion(
      """import scala.language.postfixOps
        |trait Test {
        |  def foo = {
        |    println
        |    Predef.println
        |    toString + this.toString
        |  }
        |  def bar: Unit = Predef println
        |}
      """.stripMargin,
      """import scala.language.postfixOps
        |trait Test {
        |  def foo = {
        |    println()
        |    Predef.println()
        |    toString + this.toString
        |  }
        |  def bar(): Unit = Predef println()
        |}
      """.stripMargin,
    )
  }

  @Test
  def valInFor(): Unit =
    assertCodeSuggestion(
      """trait Test {
        |  def foo: Unit = {
        |    for {
        |      val x <- 1 to 5
        |      val y = x
        |    } yield x+y
        |  }
        |}
      """.stripMargin,
      """trait Test {
        |  def foo: Unit = {
        |    for {
        |      x <- 1 to 5
        |      y = x
        |    } yield x+y
        |  }
        |}
      """.stripMargin,
    )

  @Test def outerReference(): Unit = {
    assertCodeSuggestion(
      """trait T { def f = 1 }
        |object O1 { def f = 2; class C extends T { def t = f } }
        |object O2 { def f = 2; class C extends T { object I { def t = f } } }
        |object O3 { def f = 2; object C extends T { object I { def t = f } } }
        |""".stripMargin,
      """trait T { def f = 1 }
        |object O1 { def f = 2; class C extends T { def t = this.f } }
        |object O2 { def f = 2; class C extends T { object I { def t = C.this.f } } }
        |object O3 { def f = 2; object C extends T { object I { def t = C.this.f } } }
        |""".stripMargin
    )
  }

  @Test
  def unitLiteralType(): Unit = {
    assertCodeSuggestion("class C { def f: () = () }", "class C { def f: Unit = () }")
    assertCodeSuggestion("class C { def f(a: Any) = a match { case _: () => 0 } }", "class C { def f(a: Any) = a match { case _: Unit => 0 } }")
  }

  @Test
  def constructorProcedureSyntax(): Unit = {
    assertCodeSuggestion("class C { def this(x: Int) { this() } }", "class C { def this(x: Int) = { this() } }")
  }

  @Test
  def symbolLiteral(): Unit = {
    assertCodeSuggestion("class C { 'hai }", """class C { Symbol("hai") }""")
  }

  @Test
  def unaryNilary(): Unit = {
    assertCodeSuggestion("class C { def unary_-() = 1 }", "class C { def unary_- = 1 }")
  }

  @Test
  def symbolicExtends(): Unit = {
    assertCodeSuggestion("trait T <: Object", "trait T extends Object")
  }

  @Test
  def unicodeArrow(): Unit = {
    assertCodeSuggestion("class C { val f = (x: Int) ⇒ x }", "class C { val f = (x: Int) => x }")
    assertCodeSuggestion("class C { for (x ← List(1)) yield x }", "class C { for (x <- List(1)) yield x }")
  }

  @Test
  def lowerLongL(): Unit = {
    assertCodeSuggestion("class C { val lo = 1l }", "class C { val lo = 1L }")
  }

  @Test
  def inharomnicNumeric(): Unit = {
    assertCodeSuggestion("class C { val a = 1L; val b: Double = a }", "class C { val a = 1L; val b: Double = a.toDouble }")
    assertCodeSuggestion("class C { val a = 1L; val b: Double = a + a }", "class C { val a = 1L; val b: Double = (a + a).toDouble }")
  }

  @Test
  def widenedDiv(): Unit = {
    assertCodeSuggestion("class C { val a = 1; val b: Double = a / 2 }", "class C { val a = 1; val b: Double = (a / 2).toDouble }")
  }

  @Test
  def etaNullary(): Unit = {
    assertCodeSuggestion("class C { def f = 1; val g = f _ }", "class C { def f = 1; val g = () => f }")
  }

  @Test
  def explicitImplicit(): Unit = {
    assertCodeSuggestion("class C { implicit val x = 1 }", "class C { implicit val x: Int = 1 }")
  }

  @Test
  def adaptToTuple(): Unit = {
    assertCodeSuggestion("class C { def f(x: (Int, Int)) = 0; f(1, 1) }", "class C { def f(x: (Int, Int)) = 0; f((1, 1)) }")
  }

  @Test
  def nilaryNullaryOverride(): Unit = {
    assertCodeSuggestion(
      """trait T { def f(): Int; def g: Int }
        |class C extends T { def f: Int = 1; def g() = 2 }
        |""".stripMargin,
      """trait T { def f(): Int; def g: Int }
        |class C extends T { def f(): Int = 1; def g = 2 }
        |""".stripMargin
    )
  }

  @Test
  def nullaryUnit(): Unit = {
    assertCodeSuggestion("class C { def f: Unit = println() }", "class C { def f(): Unit = println() }")
  }

  def assertCodeSuggestion(original: String, expected: String, comp: Compiler = compiler): Unit = {
    val run = comp.newRun()
    run.compileSources(comp.global.newSourceFile(original) :: Nil)
    val reporter = comp.global.reporter.asInstanceOf[StoreReporter]
    val actions = reporter.infos.flatMap(_.actions).toList
    val newCode = applyChanges(original, actions)
    assertEquals(s"\n$newCode", expected, newCode)
  }

  def applyChanges(code: String, as: List[CodeAction]): String = {
    var offset = 0
    var res = code.toVector
    for (change <- as.flatMap(_.edits).sortBy(_.position.start)) {
      // not the most efficient but it's just for tests
      res = res.take(change.position.start + offset) ++ change.newText.toVector ++ res.drop(change.position.end + offset)
      offset = offset - (change.position.end - change.position.start) + change.newText.length
    }
    new String(res.toArray)
  }
}
