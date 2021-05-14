import scala.util.Random

import scala.reflect.macros.blackbox.Context

package object tastytest {

  def anyObj[T]: T = null.asInstanceOf[T]

  trait Aspect {
    def applyTo(op: => Unit): Unit
  }

  implicit class AspectOps(op: => Unit) {
    def @@(aspect: Aspect): Unit = aspect.applyTo(op)
  }

  object ExpectCastOrNull extends Aspect {
    def applyTo(op: => Unit): Unit = {
      try {
        op
        throw new AssertionError("expected a failure")
      }
      catch {
        case npe: NullPointerException => // swallow
        case cce: ClassCastException => // swallow
      }
    }
  }

  implicit final class SafeEq[T](private val t: T) extends AnyVal {
    final def ===[U](u: U)(implicit ev: T =:= U): Boolean = t == u
  }

  class MacroImpl(val c: Context) {
    import c.universe._
    def mono: Literal = q"1"
    def poly[T: c.WeakTypeTag]: Tree = q"${c.weakTypeOf[T].toString}"
  }

  object Macros {

    def hasStaticAnnotImpl[T, A](c: Context)(implicit T: c.WeakTypeTag[T], A: c.WeakTypeTag[A]): c.Expr[Boolean] = {
      import c.universe._
      if (weakTypeOf[T].members.filter(_.isMethod).exists(_.annotations.exists(_.tree.tpe =:= weakTypeOf[A]))) {
        c.Expr[Boolean](q"true")
      }
      else {
        c.error(c.enclosingPosition, s"${weakTypeOf[T]} does not have a member with annotation ${weakTypeOf[A]}")
        c.Expr[Boolean](q"false")
      }
    }

    def constIntImpl[T](c: Context)(x: c.Expr[T])(implicit T: c.WeakTypeTag[T]): c.Expr[Int] = {
      import c.universe._
      c.Expr[Int](q"1")
    }

  }

  def getRandomNat: Int = {
    val r = Random.nextInt()
    val n = if (r == Int.MinValue) Int.MaxValue else r
    math.abs(n)
  }

  def getRandomPos: Int = {
    val potential = getRandomNat
    if (potential == 0) 1 else potential
  }
}
