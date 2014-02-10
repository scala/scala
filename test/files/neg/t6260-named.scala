class C[A](private val a: Any) extends AnyVal
trait T[A] {
  def apply(a: A): A
}

object Test {
  (x: C[Any]) => {println(s"f($x)"); x} // okay
  new T[C[Any]] { def apply(a: C[Any]) = a } // okay

  // we can't rename the specific apply method to avoid the clash
  object O extends Function1[C[Any], C[Any]] {
    def apply(a: C[Any]) = a
  }
  class X extends T[C[Any]] { def apply(a: C[Any]) = a }
}
