abstract class SamImplicitConvert {
  trait Fun[A, B] { def apply(a: A): B }
  class Lst[T]
  abstract class Str { def getBytes: Array[Int] }
  def flatMap[B](f: Fun[Str, Lst[B]]): List[B] = ???

  implicit def conv(xs: Array[Int]): Lst[Int]

  def encoded = flatMap (_.getBytes)
}
