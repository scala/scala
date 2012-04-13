import scala.reflect.mirror._

object Foo extends App {
  reify{def printf(format: String, args: Any*): String = null }
  reify{def printf(format: String, args: Any*): String = ("abc": @cloneable)}
}
