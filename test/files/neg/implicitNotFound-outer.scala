import annotation.{implicitNotFound => inf}

trait Outer[O1, O2 <: Serializable] {

  @inf("Mid not found ... O1: ${O1}; O2: ${O2}; M1: ${M1}; M2: ${M2}")
  trait Mid[M1, M2 <: O1] {

    @inf("Inner not found ... O1: ${O1}; O2: ${O2}; M1: ${M1}; M2: ${M2}; I1: ${I1}")
    trait Inner[O1, I1]

  }
}

trait Foo; trait Bar; trait Baz

object Test {

  // error: Mid not found ... O1: Int; O2: String; M1: Long; M2: Long
  implicitly[Outer[Int, String]#Mid[Long, Long]]

  // error: Mid not found ... O1: Int; O2: String; M1: Long; M2: Outer[Symbol,String]
  implicitly[Outer[Int, String]#Mid[Long, Outer[Symbol, String]]]

  // error: Inner not found ... O1: Int; O2: Bar; M1: Foo; M2: Baz; I1: Foo
  implicitly[Outer[Double, Bar]#Mid[Foo, Baz]#Inner[Int, Foo]]

  object AnOuter extends Outer[Foo, Long] { ao =>

    // error: Mid not found ... O1: Foo; O2: Long; M1: Bar; M2: Baz
    implicitly[this.Mid[Bar, Baz]]

    // error: Inner not found ... O1: String; O2: Long; M1: Baz; M2: Nothing; I1: String
    implicitly[Mid[Baz, Nothing]#Inner[String, String]]

    object AnInner extends ao.Mid[Bar, Long] {

      // error: Mid not found ... O1: Foo; O2: Long; M1: Long; M2: Bar
      implicitly[ao.Mid[Long, Bar]]

      // error: Inner not found ... O1: Test.AnOuter.type; O2: Long; M1: Bar; M2: Long; I1: Long
      implicitly[Inner[ao.type, Long]]

    }

  }

  // error: Mid not found ... O1: Foo; O2: Long; M1: Long; M2: Baz
  implicitly[AnOuter.Mid[Long, Baz]]

  class HalfOuter[R <: Serializable] extends Outer[R, R]

  val ho = new HalfOuter[java.time.Instant]

  // error: Mid not found ... O1: java.time.Instant; O2: java.time.Instant; M1: Long; M2: Baz
  implicitly[ho.Mid[Long, Baz]]

  val anon = new AnyRef with Outer[Symbol, Serializable]

  // error: Inner not found ... O1: Bar; O2: Serializable; M1: Int; M2: Baz; I1: Long
  implicitly[anon.Mid[Int, Baz]#Inner[Bar, Long]]
}

