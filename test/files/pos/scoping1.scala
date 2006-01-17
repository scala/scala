object This extends Application {
     trait A {
         def foo(): unit;
     }
     class C requires A {
         def bar() = this.foo();
     }
  class D extends C with A {
    def foo() = ()
  }
  val c: C = new D;
}
