import scala.annotation.tailrec

object TestCase {

  sealed trait Result[+A]

  type Operation[A] = Int => Result[A]

  case class Terminate[A](state: Int, value: A) extends Result[A]
  case class Continue[A](state: Int, cont: Operation[A]) extends Result[A]

  @tailrec
  def runConversion[A](state: Int, op: Operation[A]): (Int, A) = {
    op(state) match {
      case Continue(s, c) =>
        runConversion(s, c)
      case Terminate(s, v) =>
        (s, v)
    }
  }
}
