import scala.util.Random

import scala.reflect.macros.blackbox.Context

package object tastytest {

  implicit final class SafeEq[T](private val t: T) extends AnyVal {
    final def ===[U](u: U)(implicit ev: T =:= U): Boolean = t == u
  }

  object Macros {

    // def hasStrictFPImpl[T](c: Context)(x: c.Expr[T])(implicit T: c.WeakTypeTag[T]) : c.Expr[Boolean] = {
    //   import c.universe._
    //   if (symbolOf[T].annotations.exists(_.tree.tpe === typeOf[scala.annotation.strictfp]))
    //     c.Expr[Boolean](q"true")
    //   else
    //     c.error(x.tree.pos, s"t")
    // }

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
