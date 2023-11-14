package foo

trait Usage {
  def x: FactoryProvider.type#MyFactory#Product

  x.foo
}
