class A {
  class AA {
    // add a member to an inner class, dependencies on A shouldn't be recompiled
    def foo: Int = 123
  }
}

class A2
