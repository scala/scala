package foo {
  class Foo

  object Test {
    new Foo().huzzah
  }
}

package object foo {
  // Crasher: No synthetics for method PimpedFoo2: synthetics contains
  implicit class PimpedFoo2(value: Foo) {
    def huzzah = ""
  }
}
