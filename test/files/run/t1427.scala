
import scala.language.{ higherKinds }

class Bob[K[_]] {
  def foo(other: Any) = other match {
    case x: (Bob[X] forSome { type X[_] })  => true
    case _                                  => false
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val x = new Bob[List]
    val results = List(x, new Bob[Set], 55) map (x foo _)

    assert(results == List(true, true, false))
  }
}
