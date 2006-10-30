class Foo[T]

abstract class A {
  type T <: Foo[T]
  def x: T;
}

abstract class B {
  def a: A;
  val y  = a.x;
}

abstract class A2 {
  type T <: String
  def x: Array[T]
}

abstract class B2 {
  def a: A2;
  val y: Array[String]  = a.x;
}
