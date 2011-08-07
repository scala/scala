class Foo {
  val searchField = new AnyRef { search() }
  def search() = searchField
}
