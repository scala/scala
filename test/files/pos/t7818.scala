class Observable1[+T](val asJava: JObservable[_ <: T]) extends AnyVal {
  private def foo[X](a: JObservable[X]): JObservable[X] = ???
  // was generating a type error as the type of the RHS included an existential
  // skolem based on the class type parameter `T`, which did not conform
  // to the typer parameter of the extension method into which the RHS is
  // transplanted.
  def synchronize: Observable1[T] = new Observable1(foo(asJava))
}

class JObservable[T]
