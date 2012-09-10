package scala.t6162

class Bar {
  @deprecatedOverriding("`bar` will be made private in a future version.", "2.10.0")
  def bar = 42

  @deprecatedOverriding()
  def baz = 42

  def baz(a: Any) = 0
}

class SubBar extends Bar {
  override def bar = 43
  override def baz = 43
  override def baz(a: Any) = 43 // okay
}
