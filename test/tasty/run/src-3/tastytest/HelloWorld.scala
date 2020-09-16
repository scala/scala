package tastytest

import scala.annotation.StaticAnnotation

object HelloWorld {

  val msg1: String = "Hello, World!"
  def msg2: String = "Hello, World!"
  final val msg3 = "Hello, World!"
  final val msg4: String = "Hello, World!"
  val ints: List[Int] = List(1,2,3)
  val one: 1 = 1
  def inferred[A: Numeric](a: A) = the[Numeric[A]].plus(a, a)
  def the[T](implicit x: T): x.type = x
  def bounded[T >: Null <: String](a: T): String = a + a
  def higher[F[_], G[_]](fInt: F[Int])(implicit ev: F[Int] <:< G[Int]): G[Int] = ev(fInt)
  def higherBounded[F[_] >: Null <: List[_], A](f: F[A]): F[A] = f
  def higherBounded2[T <: List[_ <: Int]](f: T): T = f
  def higherBounded3[T <: List[List[_ <: Int]]](f: T): T = f
  def higherBounded4[T <: Either[_ <: Int, String]](f: T): T = f
  def higherBounded5[F[+_], A](fa: F[A]): F[A] = fa
  def higherBounded6[F[-_], A](fa: F[A]): F[A] = fa
  def higherBounded7[F[_] <: Either[_ <: Int, _], A](f: F[A]): f.type = f
  def repeated(s: String*): String = s.mkString(",")
  val func: Int => String = _.toString
  def func1[A]: A => A = x => x
  def acceptsOnlyMsg4(m: msg4.type): String = m + m
  final lazy val lzy = "lazy"
  def `1) my long test assertion that 1^3 == 2`: Int = 1^3
  // def `<init>`: Int = 157 // broken in https://github.com/lampepfl/dotty/issues/7799

}
