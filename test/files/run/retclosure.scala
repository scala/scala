/* Test return expressions inside closures.
 *
 * See bug#  */

object Test {
  def response: String = {
    def check: Option[String] = {
      val closure: String=>Nothing =
        p => return Some("deep return") // should return from check

      closure("whatever")
    }

    check

    return "ok"
  }

  def main(args: Array[String]) {
    Console.println(response)
  }
}
