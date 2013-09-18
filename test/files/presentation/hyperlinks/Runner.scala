import scala.tools.nsc.interactive.tests.InteractiveTest

object Test extends InteractiveTest {
  override def runDefaultTests() {
    // make sure typer is done.. the virtual pattern matcher might translate
    // some trees and mess up positions. But we'll catch it red handed!
    sourceFiles foreach (src => askLoadedTyped(src).get)
    super.runDefaultTests()
  }

}