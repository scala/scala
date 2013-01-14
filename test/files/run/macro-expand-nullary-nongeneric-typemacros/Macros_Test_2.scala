object Macros {
  type Foo1 = macro Impls.fooNullary
  type Foo2() = macro Impls.fooEmpty
  type Bar1(x: Int) = macro Impls.barNullary
  type Bar2(x: Int)() = macro Impls.barEmpty
}

object Test extends App {
  println((new Macros.Foo1{}).msg)
  println((new Macros.Foo2{}).msg)
  println((new Macros.Foo2(){}).msg)
  println((new Macros.Bar1(42){}).msg)
  println((new Macros.Bar2(42)(){}).msg)
  println("kkthxbai")
}