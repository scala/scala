package cls1;

trait A {

  type T;

  trait B extends A {
    type T = A.this.T;
  }

}