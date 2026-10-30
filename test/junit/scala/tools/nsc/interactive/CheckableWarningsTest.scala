package scala.tools.nsc.interactive

import org.junit.Test

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.interactive.tests.InteractiveTest

class CheckableWarningsTest {
  // Checkability warnings that depend on the children of a sealed class compiled in the current run are reported in
  // refchecks, which the presentation compiler doesn't run.
  @Test def deferredCheckabilityWarnings(): Unit = {
    deferredCheckabilityWarningsTest.main(null)
  }
}

object deferredCheckabilityWarningsTest extends InteractiveTest {
  val code =
    """sealed trait A
      |final class A1 extends A
      |trait B
      |object T {
      |  def f(x: Option[Int]) = x match { case _: Seq[B] => 0; case _ => 1 }
      |  def g(x: A) = x.isInstanceOf[B]
      |  def h(x: List[A]) = x match { case _: Seq[B] => 0; case _ => 1 }
      |}
      |""".stripMargin

  override def execute(): Unit = {
    val source = new BatchSourceFile("Test.scala", code)

    val res = new Response[Unit]
    compiler.askReload(List(source), res)
    res.get
    askLoadedTyped(source).get

    val warnings = compiler.ask { () =>
      compiler.getUnit(source).get.problems.toList.filter(_.severityLevel == 1).map(p => (p.pos.line, p.msg))
    }
    // `f` is checked in typer, `Option` is not compiled in the current run. `g` and `h` depend on the children of `A`.
    assert(warnings.map(_._1).sorted == List(5), warnings.mkString("unexpected warnings, got:\n", "\n", ""))
  }
}
