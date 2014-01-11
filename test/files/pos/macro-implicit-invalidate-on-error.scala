import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

trait LegacyLiftable[T] {
  def apply(universe: scala.reflect.api.Universe, value: T): universe.Tree
}

object LegacyLiftable {
  implicit def liftCaseClass[T <: Product]: LegacyLiftable[T] = macro liftCaseClassImpl[T]

  def liftCaseClassImpl[T: c.WeakTypeTag](c: Context): c.Expr[LegacyLiftable[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    if (!tpe.typeSymbol.asClass.isCaseClass) c.abort(c.enclosingPosition, "denied")
    val p = List(q"Literal(Constant(1))")
    c.Expr[LegacyLiftable[T]] { q"""
      new LegacyLiftable[$tpe] {
        def apply(universe: scala.reflect.api.Universe, value: $tpe): universe.Tree = {
          import universe._
          Apply(Select(Ident(TermName("C")), TermName("apply")), List(..$p))
        }
      }
    """ }
  }
}
