class Length(value: Int) extends annotation.ClassfileAnnotation
class Ann2(value: String) extends annotation.ClassfileAnnotation

object Test {
  def n = 15
  @Length(n) def foo = "foo"
  @Ann2(null) def bar = "bar"
}
