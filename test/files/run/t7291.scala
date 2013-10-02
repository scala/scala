
import scala.language.{ higherKinds, implicitConversions }

trait Fooable[T]
object Fooable {
  implicit def conjure[T]: Fooable[T] = {
    println("conjure")
    new Fooable[T]{}
  }

}

object Test {
  implicit def traversable[T, Coll[_] <: Traversable[_]](implicit
elem: Fooable[T]): Fooable[Coll[T]] = {
    println("traversable")
    new Fooable[Coll[T]]{}
  }
  def main(args: Array[String]) {
    implicitly[Fooable[List[Any]]]
  }
}
