package test;

trait A {
  type T;

  trait B extends A {
    type T = A.this.T;
  }
}
