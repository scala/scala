
object Bar

trait Foo {
  @AnnotationWithClassType(cls = classOf[Bar.type])
  def function: Any = ???
}
