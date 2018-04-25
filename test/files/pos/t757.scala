package foo {
  object C {
    def foo: Unit = {
      Console.println("foo")
    }
  }
}

package bar {
  object Main extends App {
    foo.C.foo
  }
}
