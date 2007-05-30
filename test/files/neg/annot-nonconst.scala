class Length(n: Int) extends ClassfileAnnotation

object Test {
  def n = 15
  @Length(n) def foo = "foo"
}
