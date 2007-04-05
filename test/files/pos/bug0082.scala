
object Main {

    def min0[A](less: (A, A) => Boolean, xs: List[A]): Option[A] = xs match {
        case List()  => None
        case List(x) => Some(x)
//      case x :: Nil => Some(x)
        case y :: ys => (min0(less, ys): @unchecked) match {
            case Some(m) => if (less(y, m)) Some(y) else Some(m)
        }
    }

    def min(xs: List[Int]) = min0((x: Int, y: Int) => x < y, xs);

    def main(args: Array[String]) =
        Console.println(min(List()));

}
