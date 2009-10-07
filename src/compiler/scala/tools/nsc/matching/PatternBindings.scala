/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * Author: Paul Phillips
 */

package scala.tools.nsc
package matching

import transform.ExplicitOuter
import collection.immutable.TreeMap

trait PatternBindings extends ast.TreeDSL
{
  self: ExplicitOuter with ParallelMatching =>

  import global.{ typer => _, _ }
  import definitions.{ EqualsPatternClass }
  import CODE._
  import Debug._

  /** EqualsPattern **/
  def isEquals(tpe: Type)           = cond(tpe) { case TypeRef(_, EqualsPatternClass, _) => true }
  def mkEqualsRef(tpe: Type)        = typeRef(NoPrefix, EqualsPatternClass, List(tpe))
  def decodedEqualsType(tpe: Type)  = condOpt(tpe) { case TypeRef(_, EqualsPatternClass, List(arg)) => arg } getOrElse (tpe)

  // used as argument to `EqualsPatternClass'
  case class PseudoType(o: Tree) extends SimpleTypeProxy {
    override def underlying: Type = o.tpe
    override def safeToString: String = "PseudoType("+o+")"
  }

  // If the given pattern contains alternatives, return it as a list of patterns.
  // Makes typed copies of any bindings found so all alternatives point to final state.
  def extractBindings(p: Tree, prevBindings: Tree => Tree = identity[Tree] _): List[Tree] = {
    def newPrev(b: Bind) = (x: Tree) => treeCopy.Bind(b, b.name, x) setType x.tpe

    p match {
      case b @ Bind(_, body)  => extractBindings(body, newPrev(b))
      case Alternative(ps)    => ps map prevBindings
    }
  }
  def makeBind(vs: List[Symbol], pat: Tree): Tree = vs match {
    case Nil      => pat
    case x :: xs  => Bind(x, makeBind(xs, pat)) setType pat.tpe
  }

  trait PatternBindingLogic {
    self: Pattern =>

    // This is for traversing the pattern tree - pattern types which might have
    // bound variables beneath them return a list of said patterns for flatMapping.
    def subpatternsForVars: List[Pattern] = Nil

    // This is what calls subpatternsForVars.
    def definedVars: List[Symbol] =
      (boundVariables ::: (subpatternsForVars flatMap (_.definedVars))).reverse // XXX reverse?

    lazy val boundVariables = strip(boundTree)

    // XXX only a var for short-term experimentation.
    private var _boundTree: Bind = null
    def boundTree = if (_boundTree == null) tree else _boundTree
    def withBoundTree(x: Bind): this.type = {
      _boundTree = x
      this
    }

    // If a tree has bindings, boundTree looks something like
    //   Bind(v3, Bind(v2, Bind(v1, tree)))
    // This takes the given tree and creates a new pattern
    //   using the same bindings.
    def rebindTo(t: Tree): Pattern =
      Pattern(wrapBindings(boundVariables, t))

    // Wrap this pattern's bindings around (_: Type)
    def rebindToType(tpe: Type, annotatedType: Type = null): Pattern = {
      val aType = if (annotatedType == null) tpe else annotatedType
      rebindTo(Typed(WILD(tpe), TypeTree(aType)) setType tpe)
    }

    // Wrap them around _
    def rebindToEmpty(tpe: Type): Pattern =
      rebindTo(Typed(EmptyTree, TypeTree(tpe)) setType tpe)

    // Wrap them around a singleton type for an EqualsPattern check.
    def rebindToEqualsCheck(): Pattern =
      rebindToType(equalsCheck)

    // Like rebindToEqualsCheck, but subtly different.  Not trying to be
    // mysterious -- I haven't sorted it all out yet.
    def rebindToObjectCheck(): Pattern = {
      val sType = mkSingleton
      rebindToType(mkEqualsRef(sType), sType)
    }

    /** Helpers **/

    private def wrapBindings(vs: List[Symbol], pat: Tree): Tree = vs match {
      case Nil      => pat
      case x :: xs  => Bind(x, wrapBindings(xs, pat)) setType pat.tpe
    }
    private def strip(t: Tree): List[Symbol] = t match {
      case b @ Bind(_, pat) => b.symbol :: strip(pat)
      case _                => Nil
    }
  }

  case class Binding(pvar: Symbol, tvar: Symbol) {
    // see bug #1843 for the consequences of not setting info.
    // there is surely a better way to do this, especially since
    // this looks to be the only usage of containsTp anywhere
    // in the compiler, but it suffices for now.
    if (tvar.info containsTp WildcardType)
      tvar setInfo pvar.info

    def toIdent       = Ident(tvar) setType pvar.tpe
    def castIfNeeded  =
      if (tvar.tpe <:< pvar.tpe) ID(tvar)
      else ID(tvar) AS_ANY pvar.tpe

    override def toString() = pp(pvar -> tvar)
  }

  case class BindingsInfo(xs: List[Binding]) {
    def patternVars = xs map (_.pvar)
    def temporaryVars = xs map (_.tvar)
    def idents = xs map (_.toIdent)

    def patternValDefs(implicit context: MatrixContext) =
      for (b @ Binding(pvar, tvar) <- xs) yield
        context.typedValDef(pvar, b.toIdent)
  }

  class Bindings(private val vlist: List[Binding]) extends Function1[Symbol, Option[Ident]] {
    traceCategory("Bindings", this.toString)
    def vmap(v: Symbol): Option[Binding] = vlist find (_.pvar eq v)

    // filters the given list down to those defined in these bindings
    def infoFor(vs: List[Symbol]) = BindingsInfo(vs map vmap flatten)
    def infoForAll                = BindingsInfo(vlist)

    def add(vs: Iterable[Symbol], tvar: Symbol): Bindings = {
      val newBindings = vs.toList map (v => Binding(v, tvar))
      new Bindings(newBindings ++ vlist)
    }
    def apply(v: Symbol): Option[Ident] = vmap(v) map (_.toIdent)

    override def toString() = pp(vlist)
  }

  val NoBinding: Bindings = new Bindings(Nil)
}