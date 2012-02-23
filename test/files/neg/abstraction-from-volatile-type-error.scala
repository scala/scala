class A {
  type T
  type Tv = AnyRef with T
}

object Test {
  type B = a.type forSome {
    val a : A
    val tv : a.Tv
  }
}
