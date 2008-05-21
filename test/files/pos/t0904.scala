trait A {
  def apply(x: Int): Int
  def update(x: Int, y: Int): Unit
}

trait B extends A

abstract class Foo {
  val a: A = null
  val b: B = null

  a(0) = 1
  b(0) = 1

  a(0) += 1
  b(0) += 1 // this one does not type check.
}
