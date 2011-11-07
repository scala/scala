object O {
  abstract class A {
    def f:A; 
  }
  class B extends A {
    def f = if(1 == 2) new C else new D;
  }
  class C extends A { 
    def f = this;
  }
  class D extends A {
    def f = this;
  }
}
