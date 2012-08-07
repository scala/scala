/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * Author: Paul Phillips
 */

package scala.tools.nsc
package matching

import transform.ExplicitOuter
import PartialFunction._
import language.postfixOps

trait PatternBindings extends ast.TreeDSL
{
  self: ExplicitOuter with ParallelMatching =>

  import global.{ typer => _, _ }
  import definitions.{ EqualsPatternClass }
  import CODE._
  import Debug._

  /** EqualsPattern **/
  def isEquals(tpe: Type)             = tpe.typeSymbol == EqualsPatternClass
  def mkEqualsRef(tpe: Type)          = typeRef(NoPrefix, EqualsPatternClass, List(tpe))
  def decodedEqualsType(tpe: Type)    =
    if (tpe.typeSymbol == EqualsPatternClass) tpe.typeArgs.head else tpe

  // A subtype test which creates fresh existentials for type
  // parameters on the right hand side.
  def matches(arg1: Type, arg2: Type) = decodedEqualsType(arg1) matchesPattern decodedEqualsType(arg2)

  // For spotting duplicate unapplies
  def isEquivalentTree(t1: Tree, t2: Tree) = (t1.symbol == t2.symbol) && (t1 equalsStructure t2)

  // Reproduce the Bind trees wrapping oldTree around newTree
  def moveBindings(oldTree: Tree, newTree: Tree): Tree = oldTree match {
    case b @ Bind(x, body)  => Bind(b.symbol, moveBindings(body, newTree))
    case _                  => newTree
  }

  // used as argument to `EqualsPatternClass`
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

    // The outermost Bind(x1, Bind(x2, ...)) surrounding the tree.
    private var _boundTree: Tree = tree
    def boundTree = _boundTree
    def setBound(x: Bind): Pattern = {
      _boundTree = x
      this
    }
    def boundVariables = strip(boundTree)

    // If a tree has bindings, boundTree looks something like
    //   Bind(v3, Bind(v2, Bind(v1, tree)))
    // This takes the given tree and creates a new pattern
    //   using the same bindings.
    def rebindTo(t: Tree): Pattern = Pattern(moveBindings(boundTree, t))

    // Wrap this pattern's bindings around (_: Type)
    def rebindToType(tpe: Type, ascription: Type = null): Pattern = {
      val aType = if (ascription == null) tpe else ascription
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
    def rebindToObjectCheck(): Pattern =
      rebindToType(mkEqualsRef(sufficientType), sufficientType)

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
      treeCollect(t, { case x: Bind => x.symbol })
  }

  case class Binding(pvar: Symbol, tvar: Symbol) {
    override def toString() = pvar.name + " -> " + tvar.name
  }

  class Bindings(private val vlist: List[Binding]) {
    // if (!vlist.isEmpty)
    //   traceCategory("Bindings", this.toString)

    def get() = vlist
    def toMap = vlist map (x => (x.pvar, x.tvar)) toMap

    def add(vs: Iterable[Symbol], tvar: Symbol): Bindings = {
      val newBindings = vs.toList map (v => Binding(v, tvar))
      new Bindings(newBindings ++ vlist)
    }

    override def toString() =
      if (vlist.isEmpty) "<none>"
      else vlist.mkString(", ")
  }

  val NoBinding: Bindings = new Bindings(Nil)
}
