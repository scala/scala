package tastytest

object PathDep {

  trait Foo {
    type T
    def foo: T
  }

  trait Goo {
    type F <: Foo
    val foo: F
  }

  class Bar {
    def bar(member: Foo): member.T = {
      member.foo
    }
    def baz(member: Goo): member.foo.T = {
      member.foo.foo
    }
    def qux(member: Goo): member.foo.type = {
      member.foo
    }
  }

}
