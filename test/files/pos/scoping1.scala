object This with Executable {
     trait A {
         def foo(): unit;
     }
     class C: A {
         def bar() = this.foo();
     }
  class D extends C with A {
    def foo() = ()
  }
  val c: C = new D;
}
