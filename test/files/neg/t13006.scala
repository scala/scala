//> using options -deprecation -Werror

@deprecated case class C[T](x: T)
case class D @deprecated() (x: Int)

object A {
  type X[T] = C[T] // warn
  val X = C        // no warn

  type Y = D       // no warn
  val Y = D        // no warn
}

class T {
  def t1 = A.X.apply(10) // no warn
  def t2 = new A.X(10)   // no warn
  def t3 = C.apply(20)   // warn
  def t4 = new C(10)     // warn
  def t5 = C.hashCode    // no warn

  def u1 = A.Y.apply(10) // warn
  def u2 = new A.Y(10)   // warn
  def u3 = D.apply(10)   // warn
  def u4 = new D(10)     // warn
  def u5(d: D) = 10      // no warn
}

object oho {
  @deprecated case class C private[oho] (x: Int)
  case class D @deprecated() private[oho] (x: Int)
}

class T1 {
  def t1 = oho.C(10) // warn
  def t2 = oho.D(10) // warn
}
