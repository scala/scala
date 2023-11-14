object Foo {
  // Compilation will also fail if we change Providers.type#SomeProvider to Provider
  def provide: Providers.type#SomeProvider#Operations = ???
}