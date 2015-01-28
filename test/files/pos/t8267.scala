class Bippy { trait Foo[A] }

final class RichBippy[C <: Bippy with Singleton](val c1: C) {
  def f: Int = 1
  def f[A](x: A)(ev: c1.Foo[A]): Int = 2

  def g[A <: Nothing](x: A): Int = 1
  def g[A](x: A)(ev: c1.Foo[A]): Int = 2

  def h[A](x: A)(ev: c1.Foo[A]): Int = 1

  def i(x: Nothing): Int = 1
  def i(x: AnyRef)(ev: c1.Foo[x.type]): Int = 2
}

object p {

  val c  = new Bippy
  val d0 = new RichBippy[c.type](c)
  def d1 = new RichBippy[c.type](c)
 
  d0.f[Int](5)(null: c.Foo[Int])  // ok
  d1.f[Int](5)(null: c.Foo[Int])  // fails

  d0.g[Int](5)(null: c.Foo[Int])  // ok
  d1.g[Int](5)(null: c.Foo[Int])  // fails

  d0.h[Int](5)(null: c.Foo[Int])  // ok
  d1.h[Int](5)(null: c.Foo[Int])  // ok

  d0.i("")(null)  // ok
  d1.i("")(null)  // ok
}
