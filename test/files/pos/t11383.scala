object Test {
  trait DepFn[A] { type B; def apply(a: A): B }
  object DepFn { type Aux[A, C] = DepFn[A] { type B = C } }
  class Syntax(val i: Int) extends AnyVal {
    def foo[A](e: Either[Int => A, DepFn.Aux[Int, A]]) = e.fold(_(i), _(i))
  }
}
