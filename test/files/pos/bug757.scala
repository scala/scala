package foo { 
  object C {
    def foo {
      Console.println("foo")
    }
  }
}

package bar { 
  object Main extends Application {
    foo.C.foo
  }
}
