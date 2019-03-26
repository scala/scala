package t11446

object A {
  def unapply(s: String, strict: Boolean = false) =
    if (s == "") None else Some(s.length)
}
object B {
  def unapply(s: String)(u: Int) =
    if (s == "") None else Some(u)
}
object C {
  def unapply(s: String, i: Int) =
    if (s == "") None else Some(i)
}
object D {
  def unapply(): Option[Int] = Some(1)
}
object E {
  def unapply: Option[Int] = Some(1)
}
object F {
  val unapply: Option[Int] = Some(1)
}
object G {
  def unapply(va: String*): Option[String] = va.headOption
}

object Test {
  "a" match { case A(i) => i }
  "b" match { case B(i) => i }
  "c" match { case C(i) => i }
  "d" match { case D(i) => i }
  "e" match { case E(i) => i }
  "f" match { case F(i) => i }
  "g" match { case F(i) => i }
}