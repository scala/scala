//> using options -Xlint -Xfatal-warnings
//
package object foo {
  implicit class EnrichedInt(foo: Int) {
    def bar = ???
    def bippy = foo
  }
}
