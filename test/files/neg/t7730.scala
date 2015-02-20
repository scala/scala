class Foo {
  def f(x: Any) = x match { case (a: AnyRef, b: (a.type forSome { type T })) => () }
  def g(x: Any) = x match { case (a: AnyRef, b: Array[a.type]) => () }

  // TODO this is harder to prevent
  def h(x: Any) = x match { case (a: AnyRef, b: ({type L = a.type})#L) => () }
}
