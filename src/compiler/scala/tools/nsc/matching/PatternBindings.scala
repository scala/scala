/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * Author: Paul Phillips
 */

package scala.tools.nsc
package matching

import transform.ExplicitOuter
import collection.immutable.TreeMap
import PartialFunction._

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
  def extractBindings(p: Pattern): List[Pattern] =
    toPats(_extractBindings(p.boundTree, identity))

  private def _extractBindings(p: Tree, prevBindings: Tree => Tree): List[Tree] = {
    def newPrev(b: Bind) = (x: Tree) => treeCopy.Bind(b, b.name, x) setType x.tpe

    p match {
      case b @ Bind(_, body)  => _extractBindings(body, newPrev(b))
      case Alternative(ps)    => ps map prevBindings
    }
  }

  trait PatternBindingLogic {
    self: Pattern =>

    // This is for traversing the pattern tree - pattern types which might have
    // bound variables beneath them return a list of said patterns for flatMapping.
    def subpatternsForVars: List[Pattern] = Nil

    private def shallowBoundVariables = strip(boundTree)
    private def otherBoundVariables = subpatternsForVars flatMap (_.deepBoundVariables)

    def deepBoundVariables: List[Symbol] = shallowBoundVariables ::: otherBoundVariables
    // An indiscriminate deep search would be:
    //
    // def deepBoundVariables = deepstrip(boundTree)

    lazy val boundVariables = {
      val res = shallowBoundVariables
      val deep = deepBoundVariables

      if (res.size != deep.size)
        TRACE("deep variable list %s is larger than bound %s", deep, res)

      res
    }

    // XXX only a var for short-term experimentation.
    private var _boundTree: Bind = null
    def boundTree = if (_boundTree == null) tree else _boundTree
    def withBoundTree(x: Bind): this.type = {
      _boundTree = x
      tracing[this.type]("Bound", this)
    }

    // If a tree has bindings, boundTree looks something like
    //   Bind(v3, Bind(v2, Bind(v1, tree)))
    // This takes the given tree and creates a new pattern
    //   using the same bindings.
    def rebindTo(t: Tree): Pattern = {
      if (boundVariables.size < deepBoundVariables.size)
        TRACE("ALERT: rebinding %s is losing %s", this, otherBoundVariables)

      Pattern(wrapBindings(boundVariables, t))
    }

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
    def rebindToObjectCheck(): Pattern = rebindToType(mkEqualsRef(atomicTpe), atomicTpe)

    /** Helpers **/
    private def wrapBindings(vs: List[Symbol], pat: Tree): Tree = vs match {
      case Nil      => pat
      case x :: xs  => Bind(x, wrapBindings(xs, pat)) setType pat.tpe
    }
    private def strip(t: Tree): List[Symbol] = t match {
      case b @ Bind(_, pat) => b.symbol :: strip(pat)
      case _                => Nil
    }
    private def deepstrip(t: Tree): List[Symbol] =
      t filter { case _: Bind => true ; case _ => false } map (_.symbol)
  }

  case class Binding(pvar: Symbol, tvar: Symbol) {
    // see bug #1843 for the consequences of not setting info.
    // there is surely a better way to do this, especially since
    // this looks to be the only usage of containsTp anywhere
    // in the compiler, but it suffices for now.
    if (tvar.info containsTp WildcardType)
      tvar setInfo pvar.info

    override def toString() = pp(pvar -> tvar)
  }

  class Bindings(private val vlist: List[Binding]) {
    if (!vlist.isEmpty)
      traceCategory("Bindings", this.toString)

    def get() = vlist

    def add(vs: Iterable[Symbol], tvar: Symbol): Bindings = {
      val newBindings = vs.toList map (v => Binding(v, tvar))
      new Bindings(newBindings ++ vlist)
    }

    override def toString() =
      if (vlist.isEmpty) "No Bindings"
      else "%d Bindings(%s)".format(vlist.size, pp(vlist))
  }

  val NoBinding: Bindings = new Bindings(Nil)
}