package scala.tools
package reflect

import scala.reflect.reify.Taggers
import scala.tools.nsc.typechecker.{ Analyzer, Macros }
import scala.reflect.runtime.Macros.currentMirror
import scala.reflect.api.Universe
import scala.reflect.macros.compiler.DefaultMacroCompiler

/** Optimizes system macro expansions by hardwiring them directly to their implementations
 *  bypassing standard reflective load and invoke to avoid the overhead of Java/Scala reflection.
 */
trait FastTrack {
  self: Macros with Analyzer =>

  import global._
  import definitions._
  import scala.language.implicitConversions
  import treeInfo.Applied

  private implicit def context2taggers(c0: MacroContext): Taggers { val c: c0.type } =
    new { val c: c0.type = c0 } with Taggers
  private implicit def context2macroimplementations(c0: MacroContext): MacroImplementations { val c: c0.type } =
    new { val c: c0.type = c0 } with MacroImplementations
  private def make(sym: Symbol)(pf: PartialFunction[Applied, MacroContext => Tree]) =
    sym -> new FastTrackEntry(pf)

  final class FastTrackEntry(pf: PartialFunction[Applied, MacroContext => Tree]) extends (MacroArgs => Any) {
    def validate(tree: Tree) = pf isDefinedAt Applied(tree)
    def apply(margs: MacroArgs): margs.c.Expr[Nothing] = {
      val MacroArgs(c, _) = margs
      // Macros validated that the pf is defined here - and there's not much we could do if it weren't.
      c.Expr[Nothing](pf(Applied(c.expandee))(c))(c.WeakTypeTag.Nothing)
    }
  }

  /** A map from a set of pre-established macro symbols to their implementations. */
  lazy val fastTrack = Map[Symbol, FastTrackEntry](
    make(        materializeClassTag) { case Applied(_, ttag :: Nil, _)                 => _.materializeClassTag(ttag.tpe) },
    make(     materializeWeakTypeTag) { case Applied(_, ttag :: Nil, (u :: _) :: _)     => _.materializeTypeTag(u, EmptyTree, ttag.tpe, concrete = false) },
    make(         materializeTypeTag) { case Applied(_, ttag :: Nil, (u :: _) :: _)     => _.materializeTypeTag(u, EmptyTree, ttag.tpe, concrete = true) },
    make(           ApiUniverseReify) { case Applied(_, ttag :: Nil, (expr :: _) :: _)  => c => c.materializeExpr(c.prefix.tree, EmptyTree, expr) },
    make(            StringContext_f) { case Applied(Select(Apply(_, ps), _), _, args)  => c => c.macro_StringInterpolation_f(ps, args.flatten, c.expandee.pos) },
    make(ReflectRuntimeCurrentMirror) { case _                                          => c => currentMirror(c).tree }
  )
}
