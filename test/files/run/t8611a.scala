trait K
trait L

object O {
  type LK = K with L
  val A: LK = new K with L
  val B: LK = new K with L
}

object Test extends App {
  val scrut: O.LK = O.B
  scrut match {
    case O.A => ???
    case O.B => // spurious unreachable
  }
}
