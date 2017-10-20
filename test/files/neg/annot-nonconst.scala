class Length(value: Int) extends annotation.ConstantAnnotation
class Ann2(value: String) extends annotation.ConstantAnnotation

object Test {
  def n = 15
  @Length(n) def foo = "foo"
  @Ann2(null) def bar = "bar"
}
