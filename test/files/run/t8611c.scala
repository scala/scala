trait K
trait L

object O {
  type LK = K with L
}

object Test extends App {
  local

  def local = {
    val A: O.LK = new K with L
    val B: O.LK = new K with L
    val scrut: O.LK = A
    scrut match {
      case B if "".isEmpty => ???
      case A =>
      case B => ???
    }
  }
}
