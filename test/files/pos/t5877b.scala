package foo

class Foo

object Test {
  new Foo().huzzah
}

object `package` {
  implicit class PimpedFoo2(value: Foo) {
    def huzzah = ""
  }
}
