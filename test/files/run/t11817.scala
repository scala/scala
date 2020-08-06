object Test extends App {
  type Id[A] = A
  def f[A](pf: PartialFunction[A, Unit]): Unit = ()
  def g[A <: Id[Int]](a: A) = a
  f[Id[Int]] { case _ => }
  g(42)
}
