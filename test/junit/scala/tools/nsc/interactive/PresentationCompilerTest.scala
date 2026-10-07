package scala.tools.nsc.interactive

import org.junit.Test

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.interactive.tests.InteractiveTest

class PresentationCompilerTest {
  @Test def run13112(): Unit = {
    t13112.main(null)
  }

  // https://github.com/scalameta/metals/issues/8779
  // completionsAt returns NoResults after a broken string interpolation
  @Test
  def completionsAtAfterBrokenInterpolation(): Unit = {
    new completionsAfterBrokenLiteralTest(
      """object M {
        |  val myName = "x"
        |  val a = s"$"
        |  val b = myNa
        |}
        |""".stripMargin
    ).main(null)
  }

  @Test
  def completionsAtAfterBrokenString(): Unit = {
    new completionsAfterBrokenLiteralTest(
      """object M {
        |  val myName = "x"
        |  val a = "
        |  val b = myNa
        |}
        |""".stripMargin
    ).main(null)
  }
}

object t13112 extends InteractiveTest {
  val code =
    """case class Foo(name: String = "")
      |object Foo extends Foo("")
      |""".stripMargin

  override def execute(): Unit = {
    val source = new BatchSourceFile("Foo.scala", code)

    val res = new Response[Unit]
    compiler.askReload(List(source), res)
    res.get
    askLoadedTyped(source).get

    // the second round was failing (see scala/bug#13112 for details)
    compiler.askReload(List(source), res)
    res.get
    val reloadRes = askLoadedTyped(source).get
    assert(reloadRes.isLeft)
  }
}

// https://github.com/scalameta/metals/issues/8779
class completionsAfterBrokenLiteralTest(code: String) extends InteractiveTest {
  override def execute(): Unit = {
    val source = new BatchSourceFile("Test.scala", code)

    val res = new Response[Unit]
    compiler.askReload(List(source), res)
    res.get
    askLoadedTyped(source).get

    val nameOffset = code.lastIndexOf("myNa")
    assert(nameOffset >= 0, "test code must contain 'myNa'")
    val cursorPos = source.position(nameOffset + "myNa".length)

    val result = compiler.ask(() => compiler.completionsAt(cursorPos))

    result match {
      case compiler.CompletionResult.NoResults =>
        throw new AssertionError("completionsAt returned NoResults, expected completions including 'myName'")
      case _ =>
        val names = compiler.ask(() => result.matchingResults().map(_.sym.name.decoded.trim))
        assert(names.contains("myName"), s"Expected 'myName' in completions, got: ${names.mkString(", ")}")
    }
  }
}
