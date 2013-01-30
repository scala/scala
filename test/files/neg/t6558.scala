class AnnotNotFound {
  def foo(a: Any) = ()

  @classs
  class C

  class D[@typeparam T]

  class E(
  	@valueparam x: Any
  )
}
