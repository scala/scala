import scala.reflect.macros.whitebox.Context

object Macros {
  def impl[T](c: Context)(implicit T: c.WeakTypeTag[T]) = {
    import c.universe._
    import definitions._
    val fields = T.tpe.decls.toList.collect{ case x: TermSymbol if x.isVal && x.isCaseAccessor => x }
    val Repr = appliedType(TupleClass(fields.length).asType.toType, fields.map(_.info))
    q"new Generic[$T]{ type Repr = $Repr }"
  }
}