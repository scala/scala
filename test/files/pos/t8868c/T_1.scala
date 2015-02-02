class C(a: Any) extends annotation.StaticAnnotation

@C({val x = 0; x})
trait T {
  class X

  @C({val x = 0; x})
  def foo = 42
}
