package scala.tools.nsc.interactive.tests.core

import scala.reflect.internal.util.Position

trait PresentationCompilerTestDef {

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
