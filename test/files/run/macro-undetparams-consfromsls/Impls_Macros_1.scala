import scala.reflect.runtime.universe._
import scala.reflect.macros.BlackboxContext

object Macros {
  def cons_impl[A: c.WeakTypeTag](c: BlackboxContext)(x: c.Expr[A], xs: c.Expr[List[A]]): c.Expr[List[A]] = {
    import c.universe._
    reify {
      println("A = " + c.Expr[String](Literal(Constant(implicitly[c.WeakTypeTag[A]].toString))).splice)
      x.splice :: xs.splice
    }
  }

  def nil_impl[B: c.WeakTypeTag](c: BlackboxContext): c.Expr[List[B]] = {
    import c.universe._
    reify {
      println("B = " + c.Expr[String](Literal(Constant(implicitly[c.WeakTypeTag[B]].toString))).splice)
      Nil
    }
  }

  def cons[A](x: A, xs: List[A]): List[A] = macro cons_impl[A]

  def nil[B]: List[B] = macro nil_impl[B]
}