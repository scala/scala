class Foo(value: Int) {
  implicit def this(a: String) = this(a.toInt)
}
