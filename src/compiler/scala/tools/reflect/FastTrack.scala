package scala.tools
package reflect

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
  private implicit def context2taggers(c0: MacroContext): Taggers { val c: c0.type } = new { val c: c0.type = c0 } with Taggers
  private implicit def context2macroimplementations(c0: MacroContext): MacroImplementations { val c: c0.type } = new { val c: c0.type = c0 } with MacroImplementations

  implicit def fastTrackEntry2MacroRuntime(entry: FastTrackEntry): MacroRuntime = args => entry.run(args.c)
  type FastTrackExpander = PartialFunction[(MacroContext, Tree), Tree]
  case class FastTrackEntry(sym: Symbol, expander: FastTrackExpander) {
    def validate(c: MacroContext): Boolean = expander.isDefinedAt((c, c.expandee))
    def run(c: MacroContext): Any = {
      val result = expander((c, c.expandee))
      c.Expr[Nothing](result)(c.AbsTypeTag.Nothing)
    }
  }

  lazy val fastTrack: Map[Symbol, FastTrackEntry] = {
    var registry = Map[Symbol, FastTrackEntry]()
    implicit class BindTo(sym: Symbol) { def bindTo(expander: FastTrackExpander): Unit = if (sym != NoSymbol) registry += sym -> FastTrackEntry(sym, expander) }
    MacroInternal_materializeClassTag bindTo { case (c, Apply(TypeApply(_, List(tt)), List(u))) => c.materializeClassTag(u, tt.tpe) }
    MacroInternal_materializeAbsTypeTag bindTo { case (c, Apply(TypeApply(_, List(tt)), List(u))) => c.materializeTypeTag(u, EmptyTree, tt.tpe, concrete = false) }
    MacroInternal_materializeTypeTag bindTo { case (c, Apply(TypeApply(_, List(tt)), List(u))) => c.materializeTypeTag(u, EmptyTree, tt.tpe, concrete = true) }
    BaseUniverseReify bindTo { case (c, Apply(TypeApply(_, List(tt)), List(expr))) => c.materializeExpr(c.prefix.tree, EmptyTree, expr) }
    ReflectRuntimeCurrentMirror bindTo { case (c, _) => scala.reflect.runtime.Macros.currentMirror(c).tree }
    StringContext_f bindTo { case (c, app@Apply(Select(Apply(_, parts), _), args)) => c.macro_StringInterpolation_f(parts, args, app.pos) }
    registry
  }
}