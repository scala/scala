case class Foo() extends Serializable
case object Bar extends Serializable

case class Bippy[T, U](x: T, y: U) extends Product2[T, U] { }

case class Bounded[T <: util.Random, U <: util.Random](x: T, y: U) { }

class A {
  def f(x: Bounded[_, _]) = x.productIterator foreach g
  def g(rand: util.Random) = ()
}