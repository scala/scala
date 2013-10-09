// test synthesizeSAMFunction where the sam type is not fully defined
class T {
  trait F[T, U] { def apply(x: T): U }
  // NOTE: the f(x) desugaring for now assumes the single abstract method is called 'apply'
  def app[T, U](x: T)(f: F[T, U]): U = f(x)
  app(1)(x => List(x))
}