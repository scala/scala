package p2 {
  abstract class A {
    private[p2] def f2(): Int
  }
  class B1 extends A {
    private def f2(): Int = 1
  }
  abstract class B2 extends A {
    private def f2(): Int = 1
  }
}
