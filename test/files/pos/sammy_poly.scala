// test synthesizeSAMFunction where the sam type is not fully defined
trait F[T, R]{ def apply(x: T): R }

class PolySammy {
  (x => x + 1): F[Int, Int]
  ((x: Int) => x + 1): F[Int, Int]
  ((x: String) => 1): F[String, Int]
  ((x: Object) => 1): F[String, Int]

  def app[T, U](x: T)(f: F[T, U]): U = f(x)
  app(1)(x => List(x))
}
