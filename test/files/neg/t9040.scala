object Foo {
  type Alias = {
    // def m: Alias
  }
  def foo {
    type LocalAlias = {
      def m: LocalAlias
    }
  }
}
