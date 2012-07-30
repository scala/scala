class Bar {
  @deprecatedOverriding("`bar` will be made private in a future version.", "2.10.0")
  def bar = 42
}

class SubBar extends Bar {
  override def bar = 43
}