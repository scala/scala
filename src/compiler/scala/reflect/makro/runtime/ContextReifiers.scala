package scala.reflect.makro
package runtime

abstract class ContextReifiers { self =>
  val c: Context

  import c.universe._
  import definitions._
  import treeBuild._

  import scala.reflect.reify.Taggers
  import language.implicitConversions
  private implicit def context2taggers(c0: Context) : Taggers { val c: c0.type } = new { val c: c0.type = c0 } with Taggers

  private def forMacroContext[T](prefix: Tree)(op: (Tree, Tree) => T): T = {
    val universe = gen.mkAttributedSelect(prefix.duplicate, MacroContextUniverse) setType SingleType(prefix.tpe, MacroContextUniverse)
    val mirror = TypeApply(Select(Select(prefix.duplicate, nme.mirror), nme.asInstanceOf_), List(Select(Ident(nme.UNIVERSE_SHORT), tpnme.Mirror)))
    op(universe, mirror)
  }

  def materializeExprForMacroContext(prefix: Tree, expr: Tree): Tree =
    forMacroContext(prefix)((universe, mirror) => c.materializeExpr(universe, mirror, expr))

  def materializeTypeTagForMacroContext(prefix: Tree, tpe: Type, concrete: Boolean): Tree =
    forMacroContext(prefix)((universe, mirror) => c.materializeTypeTag(universe, mirror, tpe, concrete))
}