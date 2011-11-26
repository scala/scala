import scala.reflect._

object Foo extends App {
  Code.lift{def printf(format: String, args: Any*): String = null }
  Code.lift{def printf(format: String, args: Any*): String = ("abc": @cloneable)}
}
