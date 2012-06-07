package scala.tools
package reflect

import scala.reflect.makro.runtime.ContextReifiers
import scala.reflect.reify.Taggers
import scala.tools.nsc.typechecker.{Analyzer, Macros}

/** Optimizes system macro expansions by hardwiring them directly to their implementations
 *  bypassing standard reflective load and invoke to avoid the overhead of Java/Scala reflection.
 */
trait FastTrack {
  self: Macros with Analyzer =>

  import global._
  import definitions._

  import language.implicitConversions
  private implicit def context2taggers(c0: MacroContext) : Taggers { val c: c0.type } = new { val c: c0.type = c0 } with Taggers
  private implicit def context2contextreifiers(c0: MacroContext) : ContextReifiers { val c: c0.type } = new { val c: c0.type = c0 } with ContextReifiers

  implicit def fastTrackEntry2MacroRuntime(entry: FastTrackEntry): MacroRuntime = args => entry.run(args)
  type FastTrackExpander = PartialFunction[(MacroContext, Tree), Tree]
  case class FastTrackEntry(sym: Symbol, expander: FastTrackExpander) {
    def validate(argss: List[List[Any]]): Boolean = {
      val c = argss.flatten.apply(0).asInstanceOf[MacroContext]
      val isValid = expander isDefinedAt (c, c.expandee)
      isValid
    }
    def run(args: List[Any]): Any = {
      val c = args(0).asInstanceOf[MacroContext]
      val result = expander((c, c.expandee))
      c.Expr[Nothing](result)(c.TypeTag.Nothing)
    }
  }

  lazy val fastTrack: Map[Symbol, FastTrackEntry] = {
    var registry = Map[Symbol, FastTrackEntry]()
    implicit class BindTo(sym: Symbol) { def bindTo(expander: FastTrackExpander): Unit = if (sym != NoSymbol) registry += sym -> FastTrackEntry(sym, expander) }
    MacroInternal_materializeClassTag bindTo { case (c, Apply(TypeApply(_, List(tt)), List(u))) => c.materializeClassTag(u, tt.tpe) }
    MacroInternal_materializeAbsTypeTag bindTo { case (c, Apply(TypeApply(_, List(tt)), List(u))) => c.materializeTypeTag(u, EmptyTree, tt.tpe, concrete = false) }
    MacroInternal_materializeTypeTag bindTo { case (c, Apply(TypeApply(_, List(tt)), List(u))) => c.materializeTypeTag(u, EmptyTree, tt.tpe, concrete = true) }
    ApiUniverseReify bindTo { case (c, Apply(TypeApply(_, List(tt)), List(expr))) => c.materializeExpr(c.prefix.tree, EmptyTree, expr) }
    MacroContextReify bindTo { case (c, Apply(TypeApply(_, List(tt)), List(expr))) => c.materializeExprForMacroContext(c.prefix.tree, expr) }
    ReflectRuntimeCurrentMirror bindTo { case (c, _) => scala.reflect.runtime.Macros.currentMirror(c).tree }
    registry
  }
}