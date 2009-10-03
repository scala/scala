/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * Author: Paul Phillips
 */

package scala.tools.nsc
package matching

import transform.ExplicitOuter

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
    override def toString() = "%s: %s @ %s: %s".format(pvar.name, pvar.tpe, tvar.name, tvar.tpe)
  }

  case class BindingsInfo(xs: List[BindingInfo]) {
    def idents = xs map (_.ident)
    def vsyms = xs map (_.vsym)

    def vdefs(implicit context: MatrixContext) =
      xs map (x => context.typedValDef(x.vsym, x.ident))
  }
  case class BindingInfo(vsym: Symbol, ident: Ident)

  case class Bindings(bindings: Binding*) extends Function1[Symbol, Option[Ident]] {
    private def castIfNeeded(pvar: Symbol, tvar: Symbol) =
      if (tvar.tpe <:< pvar.tpe) ID(tvar)
      else ID(tvar) AS_ANY pvar.tpe

    // filters the given list down to those defined in these bindings
    def infoFor(vs: List[Symbol]): BindingsInfo = BindingsInfo(
      for (v <- vs ; substv <- apply(v)) yield
        BindingInfo(v, substv)
    )

    def add(vs: Iterable[Symbol], tvar: Symbol): Bindings = {
      def newBinding(v: Symbol) = {
        // see bug #1843 for the consequences of not setting info.
        // there is surely a better way to do this, especially since
        // this looks to be the only usage of containsTp anywhere
        // in the compiler, but it suffices for now.
        if (tvar.info containsTp WildcardType)
          tvar setInfo v.info

        Binding(v, tvar)
      }
      val newBindings = vs.toList map newBinding
      Bindings(newBindings ++ bindings: _*)
    }

    def apply(v: Symbol): Option[Ident] =
      bindings find (_.pvar eq v) map (x => Ident(x.tvar) setType v.tpe)

    override def toString() =
      if (bindings.isEmpty) "" else bindings.mkString(" Bound(", ", ", ")")

    /** The corresponding list of value definitions. */
    final def targetParams(implicit typer: analyzer.Typer): List[ValDef] =
      for (Binding(v, t) <- bindings.toList) yield
        VAL(v) === (typer typed castIfNeeded(v, t))
  }

  val NoBinding: Bindings = Bindings()
}