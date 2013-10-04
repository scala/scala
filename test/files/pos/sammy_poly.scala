// test synthesizeSAMFunction where the sam type is not fully defined
class T {
  trait F[T, U] { def apply(x: T): U }
  def app[T, U](x: T)(f: F[T, U]): U = f(x)
  app(1)(x => List(x))
}