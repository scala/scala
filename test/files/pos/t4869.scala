// /scala/trac/4869/a.scala
// Wed Jan  4 21:17:29 PST 2012

class C[T]
class A {
  def f[T](x: T): C[_ <: T] = null
  def g = List(1d) map f
}
