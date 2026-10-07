import scala.tools.nsc.interactive.tests._

object Test extends InteractiveTest {
  override protected def filterOutLines(line: String) =
    line.contains("package") ||
      // this disappeared on JDK 26
      line.contains("applet")
}
