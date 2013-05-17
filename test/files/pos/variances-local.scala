class Foo1[+T] {
  private[this] type MyType = T
}

class Foo2[+T] {
  protected[this] type MyType = T
}
