//> using options -Werror
class Out {
  sealed trait P
  case class K(x: Int) extends P
  case class L(x: Int) extends P
}
class C[O <: Out](val o: O) {
  import o._
  def t1(t: P) = t match {
    case _: K => 0
    case _: L => 0
  }
  def t2(t: P) = t match {
    case _: K => 0
  }
}
