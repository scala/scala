package s

trait Stream[+A]
case class Unfold[S,+A](s: S, f: S => Option[(A,S)]) extends Stream[A]

object Stream {
  def fromList[A](a: List[A]): Stream[A] =
    Unfold(a, (l:List[A]) => l.headOption.map((_,l.tail)))
}

object Test {
  def main(args: Array[String]): Unit = {
    val res = Stream.fromList(List(1,2,3,4))

    res match { case Unfold(s, f) => f("a string!") }
  }
}
