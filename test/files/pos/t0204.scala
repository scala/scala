class A {
  object B {
    def f() = {
      class C extends A {}; new C : A
    }
  }
}
