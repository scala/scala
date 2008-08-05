package bug1050

abstract class A {
  type T <: scala.ScalaObject
  class A requires T {
    def b = 3
    def c = b
    b
  }
}
