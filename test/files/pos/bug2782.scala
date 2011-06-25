import scala.{collection => sc}

object Test {
  trait Foo[T]

  // Haven't managed to repro without using a CanBuild or CanBuildFrom implicit parameter
  implicit def MapFoo[A, B, M[A, B] <: sc.Map[A,B]](implicit aFoo: Foo[A], bFoo: Foo[B], cb: sc.generic.CanBuild[(A, B), M[A, B]]) = new Foo[M[A,B]] {}
  implicit object Tuple2IntIntFoo extends Foo[(Int, Int)] // no difference if this line is uncommented
  implicit def Tuple2Foo[A, B] = new Foo[(A, B)] {}       // nor this one

  implicitly[Foo[(Int, Int)]]
}

class A {
  def x[N[X] >: M[X], M[_], G](n: N[G], m: M[G]) = null

  x(Some(3), Seq(2))
}
