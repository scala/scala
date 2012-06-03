import scala.reflect.runtime.universe._

object Test {
  def fn[@specialized T, @specialized U](t : T => Int, u : U => Int) : T =
    null.asInstanceOf[T]
}

trait A[@specialized(Int) T] {
  var value: T
  def getWith[@specialized(Int) Z](f: T => Z) = f(value)
}

class C extends A[Int] {
  var value = 10
  override def getWith[@specialized(Int) Z](f: Int => Z) = f(value)
}

abstract class B[T, @specialized(scala.Int) U : TypeTag, @specialized(scala.Int) V <% Ordered[V]] {
    val u: U
    val v: V

    def f(t: T, v2: V): Pair[U, V] = {
        val m: Array[U] = null
        if (m.isEmpty) {
            Pair(u, v)
        } else {
            Pair(u, v2)
        }
    }
}