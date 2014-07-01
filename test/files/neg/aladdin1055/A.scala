object A {
  sealed trait T { def f: Int }
  class TT extends T { def f = 0 }

  def foo = new T { def f = 1 } // local subclass of sealed trait T
}
