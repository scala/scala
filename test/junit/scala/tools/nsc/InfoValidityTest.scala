package scala.tools.nsc

import org.junit.Assert.{assertFalse, assertTrue}
import org.junit.Test

import scala.tools.testkit.BytecodeTesting

class InfoValidityTest extends BytecodeTesting {
  override def compilerArgs = "-Ystop-after:typer"

  // A run that stops before uncurry installs no info transformer. An info or a base type sequence computed
  // at the namer's phase is then still valid at the typer's, and the other way around.
  @Test def validAcrossPhasesWithoutInfoTransformer(): Unit = {
    val global = compiler.global
    import global._
    val run = compiler.newRun()
    run.compileSources(List(BytecodeTesting.makeSourceFile("class C", "C.scala")))
    compiler.checkReport()
    val atNamer = period(currentRunId, run.namerPhase.id)
    val atTyper = period(currentRunId, run.typerPhase.id)
    enteringPhase(run.typerPhase) {
      assertTrue(isValid(atNamer))
      assertTrue(isValidForBaseClasses(atNamer))
    }
    enteringPhase(run.namerPhase) {
      assertTrue(isValid(atTyper))
      assertTrue(isValidForBaseClasses(atTyper))
    }
    // a period at NoPhase is before every transformer of a full compile and stays invalid
    enteringPhase(run.typerPhase) {
      assertFalse(isValidForBaseClasses(period(currentRunId, NoPhase.id)))
    }
  }
}
