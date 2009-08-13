package scala.util.control

abstract class TailRec[+A]

object TailRec {

  case class Call[A](rest: () => TailRec[A]) extends TailRec[A]
  case class Done[A](result: A) extends TailRec[A]

  def tailcall[A](rest: => TailRec[A]) = new Call(() => rest)
  def done    [A](result: A) = new Done(result)
  def trampoline[A](body: TailRec[A]): A = {
    def loop(body: TailRec[A]): A = body match {
      case Call(rest) => loop(rest())
      case Done(result) => result
    }
    loop(body)
  }
  def loop[A](body: TailRec[A]): A = body match {
    case Call(rest) => loop[A](rest())
    case Done(result) => result
  }
}

