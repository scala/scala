object Test {
  // if the following line is uncommented, things compile
  // type X = Foo.Values


  def foo(f: Foo) = f.bar(0 /* : Foo.Values */)
}
