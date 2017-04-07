import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  def impl[T](c: Context)(implicit ttag: c.WeakTypeTag[T]): c.Expr[List[String]] = {
    import c.universe._;
    val ttpe = ttag.tpe
    val tsym = ttpe.typeSymbol.asClass
    val subclasses = tsym.knownDirectSubclasses.toList.map(_.name.toString)

    c.Expr[List[String]](q"$subclasses")
  }

  def knownDirectSubclasses[T]: List[String] = macro impl[T]
}
