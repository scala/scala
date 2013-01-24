import scala.reflect.runtime.universe._

object Foo extends App {
  reify{def printf(format: String, args: Any*): String = null }
  reify{def printf(format: String, args: Any*): String = ("abc": @deprecated)}
}