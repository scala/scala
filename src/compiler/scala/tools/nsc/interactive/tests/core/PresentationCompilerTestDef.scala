package scala.tools.nsc.interactive.tests.core

trait PresentationCompilerTestDef {

  protected val marker: TestMarker

  private[tests] def runTest(): Unit

  protected def withResponseDelimiter(block: => Unit)(implicit reporter: Reporter) {
    def printDelimiter() = reporter.println("=" * 80)
    printDelimiter()
    block
    printDelimiter()
  }
}