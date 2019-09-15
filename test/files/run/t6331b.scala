
import scala.tools.testkit.AssertUtil._

object Test extends App {

  assertThrows[NotImplementedError] {
    if (???) 0.0d else -0.0d
  }
  assertThrows[NotImplementedError] {
    if (???) -0.0d else 0.0d
  }
  assertThrows[NotImplementedError] {
    if (???) () else ()
  }
  assertThrows[NotImplementedError] {
    if (???) ()
  }
}
