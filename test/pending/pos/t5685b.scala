trait X[+A] {
  def x: A
}

trait XPrint[+A] extends X[A] {
  abstract override def x: A = {
    val a = super.x
    println(a)
    a
  }
}

trait F[-A, +B] { outer =>
  def apply(xv: X[A]): X[B]

  def andThen[C](f: F[B, C]): F[A, C] = new F[A, C] {
    def apply(xv: X[A]): X[C] = f(new XX(xv) with XPrint[B])
  }

  class XX(xv: X[A]) extends X[B] {
    def x = outer(xv).x
  }
}
