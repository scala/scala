class Fooo {
  // This one is problematic because of expanded names
  private[Fooo] object Bar
}

package issue127 {
  class Foo {
    private[Foo] object Bar
    class Baz {
      private[Baz] object Bazz
    }
  }

  object Foo {
    private[issue127] class Bippy
    // This one is problematic because of expanded names
    private[issue127] object Bippy
  }
}
