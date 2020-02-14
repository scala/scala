import scala.language.experimental.macros

import scala.reflect.macros.whitebox

object util {
  def lazily[T](implicit t: => T): T = t
}

case class Rec[T](res: Rec[T])

class Functor[F[_]]

object Functor {
  def apply[F[_]](implicit f: => Functor[F]): Functor[F] = f

  implicit def hcons[F[_]](implicit ihc: IsHCons10[F]): Functor[F] = ???
}

trait IsHCons10[L[_]]

object IsHCons10 {
  implicit def apply[L[_]]: IsHCons10[L] = macro mkImpl[L]

  def mkImpl[L[_]](c: whitebox.Context)
    (implicit lTag: c.WeakTypeTag[L[_]]): c.Tree = {
    import c.universe._

    val tpe = lTag.tpe.etaExpand

    q"""
      new IsHCons10[$tpe] {
        def foo = util.lazily[Functor[Rec]]
      }
    """
  }
}
