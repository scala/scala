// for 2.7.x compatibility

object A {
  implicit val one: Int = 1
}

object Test extends App {

  locally {
    import A._
    locally {
      // assert(implicitly[Int] == 1) // error: could not find implicit value for parameter e: Int.
                                     // !!! Why one A.one?
      // (I assume you mean: why _not_ A.one? A.one is shadowed by local one.
      // but the local one cannot be used yet because it does not have an explicit type.
      implicit val one = 2
      assert(implicitly[Int] == 2)
      assert(one == 2)
    }
  }

  locally {
    import A._
    implicit val one: Int = 2
    assert(implicitly[Int] == 2)
    assert(one == 2)
  }

  locally {
    import A.one // warning: imported `one` is permanently hidden by definition of value one.
                 // !!! Really?
    //assert(implicitly[Int] == 1)
    implicit val one = 2
    assert(implicitly[Int] == 2) // !!! why not 2?
    assert(one == 2)
  }

  locally {
    import A.{one => _, _}
    implicit val two = 2
    assert(implicitly[Int] == 2) // not ambiguous in 2.8.0 nor im ambiguous in 2.7.6
  }

}
