import scala.reflect.runtime.universe._
import scala.reflect.macros.Context

object Macros {
  def cons_impl[A: c.WeakTypeTag](c: Context)(x: c.Expr[A], xs: c.Expr[List[A]]): c.Expr[List[A]] = c.universe.reify {
    println("A = " + c.literal(implicitly[c.WeakTypeTag[A]].toString).splice)
    x.splice :: xs.splice
  }

  def nil_impl[B: c.WeakTypeTag](c: Context): c.Expr[List[B]] = c.universe.reify {
    println("B = " + c.literal(implicitly[c.WeakTypeTag[B]].toString).splice)
    Nil
  }

  def cons[A](x: A, xs: List[A]): List[A] = macro cons_impl[A]

  def nil[B]: List[B] = macro nil_impl[B]
}