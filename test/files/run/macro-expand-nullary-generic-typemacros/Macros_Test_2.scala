object Macros {
  type Foo1[T] = macro Impls.fooNullary[T]
  type Foo2[T]() = macro Impls.fooEmpty[T]
  type Bar1[T](x: Int) = macro Impls.barNullary[T]
  type Bar2[T](x: Int)() = macro Impls.barEmpty[T]
}

object Test extends App {
  println((new Macros.Foo1[Int]{}).msg)
  println((new Macros.Foo2[Int]{}).msg)
  println((new Macros.Foo2[Int](){}).msg)
  println((new Macros.Bar1[Int](42){}).msg)
  println((new Macros.Bar2[Int](42)(){}).msg)
  println("kkthxbai")
}