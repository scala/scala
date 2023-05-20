package scala.tools.nsc.reporters

import org.junit.Test
import org.junit.Assert._

import scala.reflect.internal.util.CodeAction
import scala.tools.testkit.BytecodeTesting

abstract class AbstractCodeActionTest extends BytecodeTesting {
  override def compilerArgs: String = "-Ystop-after:typer -Yvalidate-pos:typer -Yrangepos -deprecation -Xsource:3"
  protected def reporter = compiler.global.reporter.asInstanceOf[StoreReporter]

  @Test
  def testProcedureSyntaxDecl(): Unit =
    assertCodeSuggestion(
      """trait Test {
        |  def foo
        |  def bar;
        |  def pub { print() }
        |  def club{ print() }
        |  def saloon = { print() }
        |}""".stripMargin,
      """trait Test {
        |  def foo: Unit
        |  def bar: Unit;
        |  def pub: Unit = { print() }
        |  def club: Unit ={ print() }
        |  def saloon = { print() }
        |}""".stripMargin,
    )

  @Test
  def testGreatParenInsert(): Unit = {
    assertCodeSuggestion(
      """trait Test {
        |  def foo = {
        |    println
        |    Predef.println
        |    toString + this.toString
        |  }
        |  def bar: Unit = Predef println()
        |}
      """.stripMargin,
      """trait Test {
        |  def foo = {
        |    println()
        |    Predef.println()
        |    toString + this.toString
        |  }
        |  def bar: Unit = Predef println()
        |}
      """.stripMargin,
    )
  }

  @Test
  def testValInFor(): Unit =
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

  def assertCodeSuggestion(original: String, expected: String): Unit = {
    val run = compiler.newRun()
    run.compileSources(compiler.global.newSourceFile(original) :: Nil)
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
