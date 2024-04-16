//> using options -Werror

import scala.Predef._

final class ArrayOps[A](private val xs: Array[A]) extends AnyVal {
  def startsWith[B >: A](that: Array[B]): Boolean = eq(that)

  def endsWith[B >: A](that: Array[B]): Boolean = {
    import scala.`package`._
    eq(that)
  }
}

//warning: comparing values of types type and Array[B] using `eq` will always yield false
