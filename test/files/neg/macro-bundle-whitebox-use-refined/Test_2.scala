object Test extends App {
  println(ReturnTypeRefinement.foo.x)

  case class Foo(i: Int, s: String, b: Boolean)
  def foo[C, L](c: C)(implicit iso: FundepMaterialization[C, L]): L = iso.to(c)
  locally {
    val equiv = foo(Foo(23, "foo", true))
    def typed[T](t: => T) {}
    typed[(Int, String, Boolean)](equiv)
    println(equiv)
  }

  println(implicitly[DynamicMaterialization[C1]])
  println(implicitly[DynamicMaterialization[C2]])

  42 match {
    case ExtractorMacro(x) => println(x)
  }
}
