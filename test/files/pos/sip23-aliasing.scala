object Test {
  trait Foo0 {
    type T0
  }

  object Foo0 {
    type Aux[T] = Foo0 {type T0 = T}
    implicit def apply[T](implicit v: ValueOf[T]): Aux[T] = new Foo0 {
      type T0 = T
    }
  }

  object Bar {
    type Aux[T] = Foo0 { type T0 = T }
  }

  type Foo[T] = Foo0 { type T0 = T }
  val Foo = Foo0

  Foo[5] //OK
  implicitly[Foo.Aux[5]] //OK!
  implicitly[Foo[5]] //implicit not found error!


  val three: 3 = 3
  type Three = three.type
  Foo[Three]
  implicitly[Foo.Aux[Three]] //works
  implicitly[Foo[Three]] //implicit not found

  final object bar
  type Bar = bar.type
  Foo[Bar]
  implicitly[Foo.Aux[Bar]] //works
  implicitly[Foo[Bar]] //implicit not found
}
