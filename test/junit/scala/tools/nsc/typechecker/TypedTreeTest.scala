package scala.tools.nsc.typechecker

import org.junit.Assert.assertEquals
import org.junit.Test

import scala.tools.testkit.BytecodeTesting

class TypedTreeTest extends BytecodeTesting {
  override def compilerArgs = "-Ystop-after:typer"

  @Test
  def constantFoldedOriginalTreeAttachment(): Unit = {
    val code =
      """object O {
        |  final val x = 42
        |  def f(x: Int) = x
        |  def f(x: Boolean) = x
        |  f(O.x)
        |}
      """.stripMargin
    val run = compiler.newRun
    run.compileSources(List(BytecodeTesting.makeSourceFile(code, "UnitTestSource.scala")))
    val tree = run.units.next().body
    val List(t) = tree.filter(_.attachments.all.nonEmpty).toList
    assertEquals("42:Set(OriginalTreeAttachment(O.x))", s"$t:${t.attachments.all}")
  }
}
