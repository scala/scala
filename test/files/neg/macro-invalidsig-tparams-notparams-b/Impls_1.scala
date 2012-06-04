import scala.reflect.runtime.universe._
import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo[T: c.TypeTag, U: c.TypeTag, V](c: Ctx)(implicit V: c.TypeTag[V]): c.Expr[Unit] = {
    println(implicitly[c.TypeTag[T]])
    println(implicitly[c.TypeTag[U]])
    println(V)
    c.literalUnit
  }
}