class C

trait Foo {
  @annot.MyAnnotation(cls = classOf[C])
  def function: Any = ???
}
