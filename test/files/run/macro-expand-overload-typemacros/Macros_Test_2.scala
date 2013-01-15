object Macros {
  type Foo(x: String) = macro Impls.fooObjectString
  type Foo(x: Int) = macro Impls.fooObjectInt
}

class Macros {
  type Foo(x: String) = macro Impls.fooClassString
  type Foo(x: Int) = macro Impls.fooClassInt
}

object Test extends App {
  println((new Macros.Foo("42"){}).msg)
  println((new Macros.Foo(42){}).msg)

  val prefix = new Macros()
  println((new prefix.Foo("42"){}).msg)
  println((new prefix.Foo(42){}).msg)
}