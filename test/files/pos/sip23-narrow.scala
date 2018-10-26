object Test {
  def one[T <: 1](t: T): T = t
  final val o = one(1)
  o: 1

  def narrow[T <: Singleton](t: T): T {} = t
  final val fn0 = narrow(23)
  fn0: 23

  val n0 = narrow(23)
  n0: 23

  def id[T](t: T): T = t

  final val fi0 = id(23)
  fi0: Int
  final val fi1 = id[23](23)
  fi1: 23
  final val fi2 = id(23: 23 {})
  fi2: 23
  final val fi3 = id(narrow(23))
  fi3: 23

  val i0 = id(23)
  i0: Int
  val i1 = id[23](23)
  i1: Int
  val i2 = id(23: 23 {})
  i2: 23
  val i3 = id(narrow(23))
  i3: 23

  def opt[T](t: T): Option[T] = Some(t)

  final val fo0 = opt(23)
  fo0: Option[Int]
  final val fo1 = opt[23](23)
  fo1: Option[23]
  final val fo2 = opt(23: 23 {})
  fo2: Option[23]
  final val fo3 = opt(narrow(23))
  fo3: Option[23]

  val o0 = opt(23)
  o0: Option[Int]
  val o1 = opt[23](23)
  o1: Option[23]
  val o2 = opt(23: 23 {})
  o2: Option[23]
  val o3 = opt(narrow(23))
  o3: Option[23]

  sealed trait HList
  final case class ::[+H, +T <: HList](h: H, t: T) extends HList {
    def ::[HH](h: HH): HH :: H :: T = Test.::(h, this)
  }
  sealed trait HNil extends HList {
    def ::[H](h: H): H :: HNil = Test.::(h, this)
  }
  object HNil extends HNil

  val l0 = 23 :: "foo" :: true :: HNil
  l0: Int :: String :: Boolean :: HNil

  val l1 = narrow(23) :: narrow("foo") :: narrow(true) :: HNil
  l1: 23 :: "foo" :: true :: HNil

  def bar[T](t: T): t.type = t

  final val b0 = bar(23)
  b0: 23

  trait Skidoo[T] { type T <: Boolean }
  object Skidoo extends Skidoo0 {
    // Ideally we would have,
    //   implicit def twentyThree: Skidoo[23] { type T = true } = ???
    // however this the empty refinement returned by narrow interferes
    // with the commented example below. Using Id instead of and {}
    // solves this problem.
    implicit def twentyThree[T <: 23]: Skidoo[T] { type T = true } = ???
  }
  trait Skidoo0 {
    implicit def default[T <: Int]: Skidoo[T] { type T = false } = ???
  }

  def skidoo1(i: Int)(implicit s: Skidoo[i.type]): s.T = ???
  skidoo1(23): true
  skidoo1(13): false
  skidoo1(narrow(23)): true // This requires the <: 23 bound
  skidoo1(narrow(13)): false

  def skidoo2[T](t: T)(implicit s: Skidoo[T]): s.T = ???
  skidoo2(23): false
  skidoo2(13): false
  skidoo2(narrow(23)): true
  skidoo2(narrow(13)): false

  def skidoo3[T <: Singleton](t: T)(implicit s: Skidoo[T]): s.T = ???
  skidoo3(23): true
  skidoo3(13): false
  skidoo3(narrow(23)): true
  skidoo3(narrow(13)): false

  implicit class NarrowSyntax[T <: Singleton](val t: T) extends AnyVal {
    def narrow: T {} = t
  }

  val ns0 = 23.narrow
  ns0: 23
  val ns1 = 23L.narrow
  ns1: 23L
  val ns2 = 23.0F.narrow
  ns2: 23F
  val ns3 = 23.0.narrow
  ns3: 23.0
  val ns4 = true.narrow
  ns4: true
  val ns5 = '*'.narrow
  ns5: '*'
  val ns6 = "foo".narrow
  ns6: "foo"

  sealed trait Nat
  sealed trait Succ[N <: Nat] extends Nat
  sealed trait _0 extends Nat
  object _0 extends _0
  type _1 = Succ[_0]
  object _1 extends _1
  type _2 = Succ[_1]
  object _2 extends _2
  type _3 = Succ[_2]
  object _3 extends _3

  object Nat {
    implicit def zero(i: 0): _0 = _0
    implicit def one(i: 1): _1 = _1
    implicit def two(i: 2): _2 = _2
    implicit def three(i: 3): _3 = _3
  }

  trait Unroll[-N <: Nat] {
    type Out <: HList
  }

  object Unroll {
    implicit def zero: Unroll[_0] { type Out = HNil } = ???
    implicit def succ[N <: Nat](implicit un: Unroll[N]): Unroll[Succ[N]] { type Out = Int :: un.Out } = ???
  }

  def unroll(n: Nat)(implicit u: Unroll[n.type]): u.Out = ???

  val u0 = unroll(0)
  u0: HNil
  val u1 = unroll(1)
  u1: Int :: HNil
  val u2 = unroll(2)
  u2: Int :: Int :: HNil
  val u3 = unroll(3)
  u3: Int :: Int :: Int :: HNil

  type SInt = Int with Singleton
  def narrowAliased[A <: SInt](x: A): A {} = x
  val na = narrowAliased(5)
  na: 5
}
