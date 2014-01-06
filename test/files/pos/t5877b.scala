package foo

class Foo

object Test {
  new Foo().huzzah
}

object `package` {
  implicit class EnrichedFoo2(value: Foo) {
    def huzzah = ""
  }
}
