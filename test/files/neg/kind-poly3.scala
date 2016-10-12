object Test {

  // Checking edge cases that should not compile with kind-polymorphism

  trait X[A <: AnyKind, F[_ <: AnyKind]] { type B = F[A] }
  
  // KO
  val i0: X[Option, Double]#B = Some(5)  

  // KO
  val i1: X[Option, List]#B = Some(5)  
  
  // KO
  val i2: X[Option[Double], List]#B = Some(5)  

  // KO
  val i3: X[Option[Double], ({ type l[X[_]] = X[Int] })#l]#B = Some(5)

  // KO
  val i4: X[Double, ({ type l[X[_]] = X[Int] })#l]#B = 5.0

  // KO
  val i6: X[Either, ({ type l[X[_]] = X[Int] })#l]#B = Some(5)

  // KO
  val i7: X[Either, List]#B = Some(5)

  // KO
  trait Foo[A[_[_]]]
  val i8: X[Foo, ({ type l[X[_]] = X[Int] })#l]#B = Some(5)


  trait X2[A <: AnyKind, B <: AnyKind] { def run[F[_ <: AnyKind]]: F[A] => F[B] }

  val x21 = {
    new X2[Int, Int] { def run[F[_]]: F[Int] => F[Int] = identity[F[Int]] }
      .asInstanceOf[X2[List, Option[Double]]].run[({ type l[X[_]] = X[Int] })#l]
  }

  val x22 = {
    new X2[Int, Int] { def run[F[_]]: F[Int] => F[Int] = identity[F[Int]] }
      .asInstanceOf[X2[List, Option[Double]]].run[List]
  }

  trait X3[A <: AnyKind, B <: AnyKind, C <: AnyKind] { def run[F[_ <: AnyKind, _ <: AnyKind]]: F[A, C] => F[B, C] }

  val x31 = {
    new X3[Int, Int, String] { def run[F[_, _]]: F[Int, String] => F[Int, String] = identity[F[Int, String]] }
      .asInstanceOf[X3[Option[Double], List, String]].run[Map]
  }  
  val x32 = {
    new X3[Int, Int, String] { def run[F[_, _]]: F[Int, String] => F[Int, String] = identity[F[Int, String]] }
      .asInstanceOf[X3[List, Option[Double], String]].run[Map]
  }


  trait X4[A <: AnyKind, B <: AnyKind, C] { def run[F[_ <: AnyKind, _]]: F[A, C] => F[B, C] }

  trait Foo[A]
  trait Bar[A]
  trait Bar2[A, B]

  trait Toto[F[_], A]

  val x41 = {
    new X3[Foo, Foo, Int] { def run[F[_[_], A]]: F[Foo, Int] => F[Foo, Int] = identity[F[Foo, Int]] }
      .asInstanceOf[X3[Bar, Bar, Int]].run[Bar2]
  }  
}
