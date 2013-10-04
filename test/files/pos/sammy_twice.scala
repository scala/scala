// test repeated synthesizeSAMFunction where the sam type is not fully defined
// the naive implementation would enter the same apply$body in the same scope twice
trait F[T, U] { def apply(x: T): U }

class C {
  def app[T, U](x: T)(f: F[T, U]): U = f(x)
  app(1)(x => List(x))
  app(2)(x => List(x))
}