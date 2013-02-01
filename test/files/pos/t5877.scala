package foo {
  class Foo

  object Test {
    new Foo().huzzah
  }
}

package object foo {
  // Crasher: No synthetics for method EnrichedFoo2: synthetics contains
  implicit class EnrichedFoo2(value: Foo) {
    def huzzah = ""
  }
}
