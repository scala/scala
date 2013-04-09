import scala.language.experimental.macros
import scala.reflect.macros._

trait A[T] {
  def min[U >: T](implicit ord: Numeric[U]): T = macro A.min[T, U]
}

object A {
  def min[T: c.WeakTypeTag, U >: T: c.WeakTypeTag](c: Context)(ord: c.Expr[Numeric[U]]): c.Expr[T] = {
    c.universe.reify {
      ord.splice.zero.asInstanceOf[T]
    }
  }
}

class B extends A[Int] {
  override def min[U >: Int](implicit ord: Numeric[U]): Int = macro B.min[U]
}

object B {
  def min[U >: Int: c.WeakTypeTag](c: Context)(ord: c.Expr[Numeric[U]]): c.Expr[Int] = {
    c.universe.reify {
      ord.splice.zero.asInstanceOf[Int]
    }
  }
}