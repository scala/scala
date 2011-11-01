package scala.tools.nsc.interactive.tests.core

import scala.tools.nsc.interactive.Global
import scala.tools.nsc.util.Position

trait PresentationCompilerTestDef {

  def compiler: Global

  protected val marker: TestMarker

  private[tests] def runTest(): Unit

  protected def withResponseDelimiter(block: => Unit)(implicit reporter: Reporter) {
    def printDelimiter() = reporter.println("=" * 80)
    printDelimiter()
    block
    printDelimiter()
  }

  protected def format(pos: Position): String =
    (if(pos.isDefined) "(%d,%d)".format(pos.line, pos.column) else "<no position>")
}