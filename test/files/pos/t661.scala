package test;

object test {
  abstract class A {
    abstract class C {
      type M;
      def foo(n : M) : Unit = {}
    }
  }
  trait B extends A {
    type N;
    trait C extends super.C {
      type M = N;
      override def foo(n : M) : Unit = super.foo(n);
    }
  }
}
