import scala.reflect.macros.whitebox.Context

object Helper {
  def unapplySeq[T](x: List[T]): Option[Seq[T]] = List.unapplySeq[T](x)
}

object Macros {
  def impl[T: c.WeakTypeTag](c: Context)(x: c.Expr[List[T]]) = {
    c.universe.reify(Helper.unapplySeq(x.splice))
  }

  object UnapplyMacro {
    def unapplySeq[T](x: List[T]): Option[Seq[T]] = macro impl[T]
  }
}