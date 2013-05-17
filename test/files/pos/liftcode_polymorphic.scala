import scala.reflect.runtime.universe._

object Append extends App {

  def append[A](l1: List[A], l2: List[A]):List[A] =
    l1 match {
      case Nil => l2
      case x::xs => x :: append(xs, l2)
    }

  println(reify(append _).tree)
}
