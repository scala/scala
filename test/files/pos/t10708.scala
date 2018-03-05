trait BaseStream[T, S <: BaseStream[T, S]]
trait Stream[T] extends BaseStream[T, Stream[T]]
trait IntStream extends BaseStream[Integer, IntStream]

sealed trait SS[T, S <: BaseStream[_, S]]
object SSImplicits extends Low {
  implicit val IntValue: SS[Int, IntStream] = null
}
trait Low {
  implicit def anyStreamShape[T]: SS[T, Stream[T]] = null
}

import SSImplicits.{IntValue, anyStreamShape}

class Test {
  implicit def f[A, S <: BaseStream[_, S], CC](a: A)(implicit ss: SS[A, S]): S = ???

  y
  x

  def x = f(0): IntStream
  def y = f[String, Stream[String], Vector[String]]("")

}
