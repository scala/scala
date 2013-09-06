import language.reflectiveCalls

package test {
  class C1 {
    object O {
      def struct(s: {def foo: Any}) = s.foo
    }
  }
  trait T {
    object O {
      def struct(s: {def foo: Any}) = s.foo
    }
  }
  object O1 extends T

  object O2 {
    object O {
      def struct(s: {def foo: Any}) = s.foo
    }
  }
}

object Test extends App {
  object fooable { def foo = "foo" }
  def check(result: Any) = assert(result == "foo", result.toString)

  val s = new test.C1
  check(s.O.struct(fooable))
  check(test.O1.O.struct(fooable))
  check(test.O2.O.struct(fooable))
}
