import scala.reflect.runtime.universe._
import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo[T: c.WeakTypeTag, U: c.WeakTypeTag, V](c: Ctx)(implicit V: c.WeakTypeTag[V]): c.Expr[Unit] = {
    println(implicitly[c.WeakTypeTag[T]])
    println(implicitly[c.WeakTypeTag[U]])
    println(V)
    c.literalUnit
  }
}