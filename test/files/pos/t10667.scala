
case class C(i: Int)(j: Int)(s: String)
case class D(i: Int)(j: Int)(implicit s: String)

trait T {
  val v = C(42)(17)("hello")
  def f: C = v match {
    case c @ C(_) => c
    case C(_) if true => v
  }

  val c @ C(_) = v

  def g = D(42)(17)("hello") match {
    case d @ D(_) => "OK"
  }
}
