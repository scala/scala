import scala.util.Random

import scala.reflect.macros.blackbox.Context

package object tastytest {

  implicit final class SafeEq[T](private val t: T) extends AnyVal {
    final def ===[U](u: U)(implicit ev: T =:= U): Boolean = t == u
  }

  object Macros {

    def hasStaticAnnotImpl[T, A](c: Context)(implicit T: c.WeakTypeTag[T], A: c.WeakTypeTag[A]) : c.Expr[Boolean] = {
      import c.universe._
      if (weakTypeOf[T].members.filter(_.isMethod).exists(_.annotations.exists(_.tree.tpe =:= weakTypeOf[A]))) {
        c.Expr[Boolean](q"true")
      }
      else {
        c.error(NoPosition, s"${weakTypeOf[T]} does not have a member with annotation ${weakTypeOf[A]}")
        c.Expr[Boolean](q"false")
      }
    }

  }

  def getRandomNat: Int = {
    val r = Random.nextInt
    val n = if (r == Int.MinValue) Int.MaxValue else r
    math.abs(n)
  }

  def getRandomPos: Int = {
    val potential = getRandomNat
    if (potential == 0) 1 else potential
  }
}
