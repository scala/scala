

import scala.language.reflectiveCalls

object Test extends App {
  val foo = new {
    def apply(args : String*) = args foreach println
  }

  foo("var", "args")
}
