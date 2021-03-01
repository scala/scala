class Foo {
  @annotation.nowarn def foo = try 1 /*?*/
  def bar = try 1/*?*/
  @annotation.nowarn def bzz = try 1 /*?*/
}
