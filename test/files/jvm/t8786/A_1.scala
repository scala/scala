class A {
  @annotation.varargs def foo[T](a: Int, b: T*): T = b.head
}
