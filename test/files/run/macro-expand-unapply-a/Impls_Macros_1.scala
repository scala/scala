import scala.reflect.macros.WhiteboxContext

object Helper {
  def unapplySeq[T](x: List[T]): Option[Seq[T]] = List.unapplySeq[T](x)
}

object Macros {
  def impl[T: c.WeakTypeTag](c: WhiteboxContext)(x: c.Expr[List[T]]) = {
    c.universe.reify(Helper.unapplySeq(x.splice))
  }

  object UnapplyMacro {
    def unapplySeq[T](x: List[T]): Option[Seq[T]] = macro impl[T]
  }
}