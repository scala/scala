package bug1049

abstract class Test {
  type T <: A
  class A requires T
  class B requires T extends A
}
