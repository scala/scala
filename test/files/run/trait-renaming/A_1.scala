package bippy {
  class A {
    def f = {
      trait B {
        def f = 5
      }
      trait C {
        def g = 10
      }
      new B with C { }
    }

    def g = Class.forName("bippy.A$B$1$class")
  }
}
