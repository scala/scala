package t1050

abstract class A {
  type T <: scala.AnyRef
  class A { this: T =>
    def b = 3
    def c = b
    b
  }
}
