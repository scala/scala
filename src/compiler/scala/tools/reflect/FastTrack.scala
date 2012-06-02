package scala.tools
package reflect

import scala.reflect.base.{Universe => BaseUniverse}
import scala.reflect.api.{Universe => ApiUniverse}
import scala.reflect.makro.runtime.{Context => MacroContext}
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

  lazy val fastTrack: Map[Symbol, MacroRuntime] = {
    // challenge: how can we factor out the common code? Does not seem to be easy.
    var registry = Map[Symbol, MacroRuntime]()
    implicit class RichSymbol(sym: Symbol) { def bindTo(rt: MacroRuntime): Unit = { if (sym != NoSymbol) registry += sym -> rt; } }
    MacroInternal_materializeArrayTag bindTo (args => {
      assert(args.length == 3, args)
      val c = args(0).asInstanceOf[MacroContext]
      val u = args(1).asInstanceOf[c.Expr[BaseUniverse]]
      val tt = args(2).asInstanceOf[c.TypeTag[_]]
      c.Expr(c.materializeArrayTag(u.tree, tt.tpe))(c.TypeTag.Nothing)
    })
    MacroInternal_materializeClassTag bindTo (args => {
      assert(args.length == 3, args)
      val c = args(0).asInstanceOf[MacroContext]
      val u = args(1).asInstanceOf[c.Expr[BaseUniverse]]
      val tt = args(2).asInstanceOf[c.TypeTag[_]]
      c.Expr(c.materializeClassTag(u.tree, tt.tpe))(c.TypeTag.Nothing)
    })
    MacroInternal_materializeTypeTag bindTo (args => {
      assert(args.length == 3, args)
      val c = args(0).asInstanceOf[MacroContext]
      val u = args(1).asInstanceOf[c.Expr[BaseUniverse]]
      val tt = args(2).asInstanceOf[c.TypeTag[_]]
      c.Expr(c.materializeTypeTag(u.tree, EmptyTree, tt.tpe, concrete = false))(c.TypeTag.Nothing)
    })
    MacroInternal_materializeConcreteTypeTag bindTo (args => {
      assert(args.length == 3, args)
      val c = args(0).asInstanceOf[MacroContext]
      val u = args(1).asInstanceOf[c.Expr[BaseUniverse]]
      val tt = args(2).asInstanceOf[c.TypeTag[_]]
      c.Expr(c.materializeTypeTag(u.tree, EmptyTree, tt.tpe, concrete = true))(c.TypeTag.Nothing)
    })
    ApiUniverseReify bindTo (args => {
      assert(args.length == 2, args)
      val c = args(0).asInstanceOf[MacroContext]
      val expr = args(1).asInstanceOf[c.Expr[ApiUniverse]]
      c.Expr(c.materializeExpr(c.prefix.tree, EmptyTree, expr.tree))(c.TypeTag.Nothing)
    })
    MacroContextReify bindTo (args => {
      assert(args.length == 2, args)
      val c = args(0).asInstanceOf[MacroContext]
      val expr = args(1).asInstanceOf[c.Expr[_]]
      c.Expr(c.materializeExprForMacroContext(c.prefix.tree, expr.tree))(c.TypeTag.Nothing)
    })
    ReflectRuntimeCurrentMirror bindTo (args => {
      assert(args.length == 1, args)
      val c = args(0).asInstanceOf[MacroContext]
      scala.reflect.runtime.Macros.currentMirror(c)
    })
    registry
  }
}