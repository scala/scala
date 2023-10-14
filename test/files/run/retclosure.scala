/* Test return expressions inside closures.
 * scalac: -Xlint:-nonlocal-return
 * See bug#834  */

object Test {
  def response: String = {
    def check: Option[String] = {
      val closure: String => Nothing =
        p => return Some("some problem") // should return from check

      closure("whatever")
    }

    check match {
        case Some(problem) => "check failed: " + problem
        case None => "ok"
    }
  }

  def main(args: Array[String]): Unit = assert(response == "check failed: some problem")
}
