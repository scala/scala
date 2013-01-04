import language.experimental.macros

object Macros {
  type Foo(x: Int) = macro Impls.foo
  type Foo = Int
}