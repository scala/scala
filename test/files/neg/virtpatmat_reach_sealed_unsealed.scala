sealed abstract class X
sealed case class A(x: Int) extends X

// test reachability on mixed sealed / non-sealed matches
object Test extends App {
  val B: X = A(0)
  val C: X = A(1)

  // all cases are reachable and the match is exhaustive
  (C: X) match {
    case B =>
    case C =>
    case A(_) =>
  }

  (true: Boolean) match { case true => } // not exhaustive, but reachable
  (true: Boolean) match { case true => case false => } // exhaustive, reachable
  (true: Boolean) match { case true => case false =>  case _ => } // exhaustive, last case is unreachable
  (true: Boolean) match { case true => case false =>  case _: Boolean => } // exhaustive, last case is unreachable
  (true: Boolean) match { case true => case false =>  case _: Any => } // exhaustive, last case is unreachable
}