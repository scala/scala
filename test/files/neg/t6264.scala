class Foo {
  def foo(x: AnyRef): Unit = {
    x.isInstanceOf[Tuple2[_, Tuple1[_]]]
    ()
  }
}
