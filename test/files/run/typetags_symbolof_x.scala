import scala.reflect.runtime.universe._

class C
object C

object Test extends App {
  type T = Int
  type Id[X] = X
  println(symbolOf[Int])
  println(symbolOf[C.type])
  println(symbolOf[T])
  println(symbolOf[Id[_]])
  println(symbolOf[Nothing])
  println(symbolOf[Null])
}
