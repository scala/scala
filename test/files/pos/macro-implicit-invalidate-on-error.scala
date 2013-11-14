package scala.reflect
package api

import scala.language.experimental.macros
import scala.reflect.macros.Context

trait Liftable[T] {
  def apply(universe: api.Universe, value: T): universe.Tree
}

object Liftable {
  implicit def liftCaseClass[T <: Product]: Liftable[T] = macro liftCaseClassImpl[T]

  def liftCaseClassImpl[T: c.WeakTypeTag](c: Context): c.Expr[Liftable[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    if (!tpe.typeSymbol.asClass.isCaseClass) c.abort(c.enclosingPosition, "denied")
    val p = List(q"Literal(Constant(1))")
    c.Expr[Liftable[T]] { q"""
      new scala.reflect.api.Liftable[$tpe] {
        def apply(universe: scala.reflect.api.Universe, value: $tpe): universe.Tree = {
          import universe._
          Apply(Select(Ident(TermName("C")), TermName("apply")), List(..$p))
        }
      }
    """ }
  }
}
