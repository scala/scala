package object foo {
  implicit class EnrichedInt(foo: Int) {
    def bar = ???
    def bippy = foo
  }
}
