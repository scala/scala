package bug1049

abstract class Test {
  type T <: A
  class A { self: T => }
  class B extends A { self: T => }
}
