import scala.reflect.runtime.universe._
import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo[T: c.AbsTypeTag, U: c.AbsTypeTag, V](c: Ctx)(implicit V: c.AbsTypeTag[V]): c.Expr[Unit] = {
    import c.universe._
    println(implicitly[c.AbsTypeTag[T]])
    println(implicitly[c.AbsTypeTag[U]])
    println(V)
    c.literalUnit
  }
}