class T {
  def app[T, U](x: T)(f: F[T, U]): U = f(x)
  app(1)(x => List(x))
}
