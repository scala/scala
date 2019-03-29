import scala.tools.nsc._

// extending Global is a pretty thorough test case of bridge generation
// make sure we don't cause VerifyErrors when messing with symbol pairs
object Test extends Global(new Settings) {
  def main(args: Array[String]): Unit = {
  }
}
