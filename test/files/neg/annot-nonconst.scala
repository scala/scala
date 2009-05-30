class Length(value: Int) extends ClassfileAnnotation

object Test {
  def n = 15
  @Length(n) def foo = "foo"
}
