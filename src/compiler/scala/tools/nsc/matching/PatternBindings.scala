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

  case class Binding(pvar: Symbol, tvar: Symbol) {
    // see bug #1843 for the consequences of not setting info.
    // there is surely a better way to do this, especially since
    // this looks to be the only usage of containsTp anywhere
    // in the compiler, but it suffices for now.
    if (tvar.info containsTp WildcardType)
      tvar setInfo pvar.info

    def toIdent =
      Ident(tvar) setType pvar.tpe

    def castIfNeeded =
      if (tvar.tpe <:< pvar.tpe) ID(tvar)
      else ID(tvar) AS_ANY pvar.tpe
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
    def this() = this(Nil)

    def vmap(v: Symbol): Option[Binding] = vlist find (_.pvar eq v)

    // filters the given list down to those defined in these bindings
    def infoFor(vs: List[Symbol]) = BindingsInfo(vs map vmap flatten)
    def infoForAll                = BindingsInfo(vlist)

    def add(vs: Iterable[Symbol], tvar: Symbol): Bindings = {
      val newBindings = vs.toList map (v => Binding(v, tvar))
      new Bindings(newBindings ++ vlist)
    }

    def apply(v: Symbol): Option[Ident] = vmap(v) map (_.toIdent)

    override def toString() = " Bound(%s)".format(vlist)
  }

  val NoBinding: Bindings = new Bindings()
}