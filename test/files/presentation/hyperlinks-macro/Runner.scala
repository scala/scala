import scala.tools.nsc.interactive.tests.InteractiveTest

object Test extends InteractiveTest {
  override def runDefaultTests(): Unit = {
    sourceFiles foreach (src => askLoadedTyped(src).get)
    super.runDefaultTests()
  }
}
