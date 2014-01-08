package scala.tools
package reflect

import scala.reflect.reify.Taggers
import scala.tools.nsc.typechecker.{ Analyzer, Macros }
import scala.reflect.runtime.Macros.currentMirror
import scala.reflect.api.Universe
import scala.tools.reflect.quasiquotes.{ Quasiquotes => QuasiquoteImpls }

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
  private implicit def context2quasiquote(c0: MacroContext): QuasiquoteImpls { val c: c0.type } =
    new { val c: c0.type = c0 } with QuasiquoteImpls
  private def makeBlackbox(sym: Symbol)(pf: PartialFunction[Applied, MacroContext => Tree]) =
    sym -> new FastTrackEntry(pf, isBlackbox = true)
  private def makeWhitebox(sym: Symbol)(pf: PartialFunction[Applied, MacroContext => Tree]) =
    sym -> new FastTrackEntry(pf, isBlackbox = false)

  final class FastTrackEntry(pf: PartialFunction[Applied, MacroContext => Tree], val isBlackbox: Boolean) extends (MacroArgs => Any) {
    def validate(tree: Tree) = pf isDefinedAt Applied(tree)
    def apply(margs: MacroArgs): margs.c.Expr[Nothing] = {
      val MacroArgs(c, _) = margs
      // Macros validated that the pf is defined here - and there's not much we could do if it weren't.
      c.Expr[Nothing](pf(Applied(c.expandee))(c))(c.WeakTypeTag.Nothing)
    }
  }

  /** A map from a set of pre-established macro symbols to their implementations. */
  def fastTrack: Map[Symbol, FastTrackEntry] = fastTrackCache()
  private val fastTrackCache = perRunCaches.newGeneric[Map[Symbol, FastTrackEntry]] {
    val runDefinitions = currentRun.runDefinitions
    import runDefinitions._
    Map[Symbol, FastTrackEntry](
      makeBlackbox(        materializeClassTag) { case Applied(_, ttag :: Nil, _)                 => _.materializeClassTag(ttag.tpe) },
      makeBlackbox(     materializeWeakTypeTag) { case Applied(_, ttag :: Nil, (u :: _) :: _)     => _.materializeTypeTag(u, EmptyTree, ttag.tpe, concrete = false) },
      makeBlackbox(         materializeTypeTag) { case Applied(_, ttag :: Nil, (u :: _) :: _)     => _.materializeTypeTag(u, EmptyTree, ttag.tpe, concrete = true) },
      makeBlackbox(             Predef_classOf) { case Applied(_, ttag :: Nil, _)                 => c => gen.mkClassOf(ttag.tpe) },
      makeBlackbox(           ApiUniverseReify) { case Applied(_, ttag :: Nil, (expr :: _) :: _)  => c => c.materializeExpr(c.prefix.tree, EmptyTree, expr) },
      makeBlackbox(            StringContext_f) { case Applied(Select(Apply(_, ps), _), _, args)  => c => c.macro_StringInterpolation_f(ps, args.flatten, c.expandee.pos) },
      makeBlackbox(ReflectRuntimeCurrentMirror) { case _                                          => c => currentMirror(c).tree },
      makeWhitebox(  QuasiquoteClass_api_apply) { case _                                          => _.expandQuasiquote },
      makeWhitebox(QuasiquoteClass_api_unapply) { case _                                          => _.expandQuasiquote }
    )
  }
}
