object Example extends App {
  trait Bound { type T }

  trait X {
    type A <: Bound

    val x: A
    var n: x.T
  }

  trait Y extends X {
    trait YA extends Bound { type T >: Int <: Int }
    type A <: YA
    var n: x.T = 3
  }

  trait W extends Y {
    type ZA <: Bound

    type A = YA with ZA
  }

  trait Z extends X {
    trait ZA extends Bound { type T >: String <: String }
    type A <: ZA
    n = "foo"
  }

  new W with Z {
    lazy val x: A = x
  }
}
