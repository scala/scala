class Test {
  def f[T](xs: Set[T]) /* no expected type to trigger inference */ =
    xs collect { case x => x }

  def g[T](xs: Set[T]): Set[T] = f[T](xs) // check that f's inferred type is Set[T]

  // check that this type checks:
  List(1).flatMap(n => Set(1).collect { case w => w })
}