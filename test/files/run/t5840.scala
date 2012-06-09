import scala.reflect.runtime.universe._

object Test extends App {
  reify {
    class C[T <: String with Singleton]
  }
}