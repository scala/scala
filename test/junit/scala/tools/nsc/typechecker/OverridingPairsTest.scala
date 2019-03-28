package scala.tools.nsc.typechecker

import org.junit.Assert.assertEquals
import org.junit.Test

import scala.tools.testkit.BytecodeTesting

class OverridingPairsTest extends BytecodeTesting {
  override def compilerArgs = "-Ystop-after:typer"

  @Test
  def overridingPairs_11136(): Unit = {
    val code =
      """package p1
        |trait IO { def c(x: Int): Int = ??? }
        |trait SO extends IO { override final def c(x: Int): Int = ??? }
        |trait SOIO extends IO { override def c(x: Int): Int = ??? }
        |trait SOSO extends SOIO with SO // interloper
        |abstract class AS extends SO
        |class L extends AS with SOSO
      """.stripMargin
    val run = compiler.newRun
    run.compileSources(List(BytecodeTesting.makeSourceFile(code, "UnitTestSource.scala")))

    val g = compiler.global
    val C = g.rootMirror.getRequiredClass("p1.L")

    val opcs = new g.overridingPairs.RefchecksCursor(C).iterator.filter(_.low.nameString == "c").map(c => (c.lowString, c.highString)).toList

    assertEquals(List(("override def c(x: Int): Int in trait SOIO", "final override def c(x: Int): Int in trait SO"),
                      ("override def c(x: Int): Int in trait SOIO", "def c(x: Int): Int in trait IO")), opcs)
  }
}
