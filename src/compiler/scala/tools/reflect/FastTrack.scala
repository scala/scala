package scala.tools
package reflect

import scala.reflect.reify.Taggers
import scala.tools.nsc.typechecker.{ Analyzer, Macros }
import scala.reflect.runtime.Macros.currentMirror

/** Optimizes system macro expansions by hardwiring them directly to their implementations
 *  bypassing standard reflective load and invoke to avoid the overhead of Java/Scala reflection.
 */
trait FastTrack {
  self: Macros with Analyzer =>

  import global._
  import definitions._
  import scala.language.implicitConversions

  private implicit def context2taggers(c0: MacroContext): Taggers { val c: c0.type } =
    new { val c: c0.type = c0 } with Taggers
  private implicit def context2macroimplementations(c0: MacroContext): MacroImplementations { val c: c0.type } =
    new { val c: c0.type = c0 } with MacroImplementations
  implicit def fastTrackEntry2MacroRuntime(entry: FastTrackEntry): MacroRuntime =
    args => entry.run(args.c)

  /** TODO - reuse. */
  private object Args {
    def unapply(t: Tree): Some[List[Tree]] = t match {
      case Apply(_, args) => Some(args)
      case _              => Some(Nil)
    }
  }
  private object PolyArgs {
    def unapply(t: Tree): Some[(List[Tree], List[Tree])] = t match {
      case Apply(TypeApply(_, targs), args) => Some((targs, args))
      case Apply(_, args)                   => Some((Nil, args))
      case TypeApply(_, targs)              => Some((targs, Nil))
      case _                                => Some((Nil, Nil))
    }
  }
  private object Call {
    def unapply(t: Tree): Some[(Tree, List[Tree])] = t match {
      case Apply(fn, args) => Some((fn, args))
      case _               => Some((t, Nil))
    }
  }
  private object PolyCall {
    def unapply(t: Tree): Some[(Tree, List[Tree], List[Tree])] = t match {
      case Apply(TypeApply(fn, targs), args) => Some((fn, targs, args))
      case Apply(fn, args)                   => Some((fn, Nil, args))
      case _                                 => Some((t, Nil, Nil))
    }
  }

  final class FastTrackEntry(val sym: Symbol, pf: PartialFunction[MacroContext, Tree]) {
    def validate(c: MacroContext) = pf isDefinedAt c
    def run(c: MacroContext): Any = c.Expr[Nothing](pf(c))(c.WeakTypeTag.Nothing)
  }

  /** A map from a set of pre-established macro symbols to their implementations. */
  lazy val fastTrack: Map[Symbol, FastTrackEntry] = {
    def make(sym: Symbol, fn: MacroContext => PartialFunction[Tree, Tree]): FastTrackEntry =
      new FastTrackEntry(sym, { case c: MacroContext if fn(c) isDefinedAt c.expandee => fn(c)(c.expandee) })

    val entries = List[FastTrackEntry](
      make(materializeClassTag, c         => { case PolyArgs(ttag :: Nil, Nil)          => c.materializeClassTag(ttag.tpe) }),
      make(materializeWeakTypeTag, c      => { case PolyArgs(ttag :: Nil, u :: Nil)     => c.materializeTypeTag(u, EmptyTree, ttag.tpe, concrete = false) }),
      make(materializeTypeTag, c          => { case PolyArgs(ttag :: Nil, u :: Nil)     => c.materializeTypeTag(u, EmptyTree, ttag.tpe, concrete = true) }),
      make(ApiUniverseReify, c            => { case PolyArgs(ttag :: Nil, expr :: Nil)  => c.materializeExpr(c.prefix.tree, EmptyTree, expr) }),
      make(ReflectRuntimeCurrentMirror, c => { case _                                   => currentMirror(c).tree }),
      make(StringContext_f, c             => { case Call(Select(Args(parts), _), args)  => c.macro_StringInterpolation_f(parts, args, c.expandee.pos) }),
      make(Predef_classOf, c              => { case PolyArgs(ttag :: Nil, Nil)          => gen.mkClassOf(ttag.tpe) })
    )
    entries filterNot (_.sym == NoSymbol) map (x => x.sym -> x) toMap
  }
}
