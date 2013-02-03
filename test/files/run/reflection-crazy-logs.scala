import scala.reflect.runtime.universe._

object Test extends App {
  println(typeOf[List[Any]] <:< typeOf[List[T] forSome { type T }])
}