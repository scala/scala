abstract class A {
  type T <: String;
  def x: T;
}

abstract class B {
  def a: A;
  val y: String  = a.x;
}
