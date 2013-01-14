object Macros1 {
  type Foo[U <: String] = macro Impls1.foo[U]
}

object Macros2 {
  type Foo[U <: BoundChild] = macro Impls2.foo[U]
}

object Test extends App {
  class D1 extends Macros1.Foo[String]
  val x1: Macros1.Foo[String] = new C

  class D2 extends Macros1.Foo[String]
  val x2: Macros1.Foo[String] = new C
}