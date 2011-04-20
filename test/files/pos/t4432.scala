object Main {
  def foo1 = {
    class A {
      val x = {
        lazy val cc = 1 //
        cc
        ()
      }
    }
    new A
  }

  def foo2 = {
    class B {
      val x = {
        object cc
        cc
        ()
      }
    }
    new B
  }

  def foo3 = {
    object C {
      val x = {
        lazy val cc = 1
        cc
      }
    }
    C
  }

  def foo4 = {
    class D {
      lazy val cc = 1
      cc
    }
    new D
  }

}
