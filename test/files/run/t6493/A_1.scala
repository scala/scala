// SI-6493: existential abstraction misplaces some symbols,
// while hiding the class symbols in foo()'s inferred result type,
// it doesn't replace them by the corresponding existential quantifiers,
// which lead to a NoSuchMethodError in calling foo() because its signature was messed up
object one {
  def foo() = { object Foo { class Bar } ; new Foo.Bar }
  def module() = { trait T { object Bar } ; (new T {}).Bar }
}
