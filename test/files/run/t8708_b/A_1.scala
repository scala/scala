package p

class C {

  sealed trait T { def f: Int }

  def foo: T = new T { def f = 1 }
}
