/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.ast

import scala.tools.nsc.util.Position
import symtab.Flags._

abstract class TreeGen {

  val global: Global

  import global._
  import definitions._
  import posAssigner.atPos

  /** Builds a reference to value whose type is given stable prefix.
   */
  def mkAttributedQualifier(tpe: Type): Tree = tpe match {
    case NoPrefix =>
      EmptyTree
    case ThisType(clazz) =>
      if (clazz.isRoot || clazz.isEmptyPackageClass) EmptyTree
      else mkAttributedThis(clazz)
    case SingleType(pre, sym) =>
      if (sym.isThisSkolem) {
        mkAttributedQualifier(ThisType(sym.deSkolemize))
      } else {
        val qual = mkAttributedStableRef(pre, sym)
        qual.tpe match {
          case MethodType(List(), restpe) =>
            Apply(qual, List()) setType restpe
          case _ =>
            qual
        }
      }
    case TypeRef(pre, sym, args) =>
      assert(phase.erasedTypes)
      if (sym.isModuleClass && !sym.isRoot) {
        val qual = mkAttributedSelect(mkAttributedQualifier(sym.owner.tpe), sym.sourceModule);
        qual.tpe match {
          case MethodType(List(), restpe) =>
            Apply(qual, List()) setType restpe
          case _ =>
            qual
        }
      } else mkAttributedThis(sym)
    case _ =>
      throw new Error("bad qualifier: " + tpe)
  }

  /** Builds a reference to given symbol with given stable prefix. */
  def mkAttributedRef(pre: Type, sym: Symbol): Tree = {
    val qual = mkAttributedQualifier(pre)
    if (qual == EmptyTree) mkAttributedIdent(sym)
    else mkAttributedSelect(qual, sym)
  }

  /** Builds a reference to given symbol. */
  def mkAttributedRef(sym: Symbol): Tree =
    if (sym.owner.isClass) mkAttributedRef(sym.owner.thisType, sym)
    else mkAttributedIdent(sym)

  /** Replaces tree type with a stable type if possible */
  def stabilize(tree: Tree): Tree = tree match {
    case Ident(_) =>
      if (tree.symbol.isStable) tree.setType(singleType(tree.symbol.owner.thisType, tree.symbol))
      else tree
    case Select(qual, _) =>
      assert(tree.symbol != null)
      assert(qual.tpe != null)
      if (tree.symbol.isStable && qual.tpe.isStable)
        tree.setType(singleType(qual.tpe, tree.symbol))
      else tree
    case _ =>
      tree
  }

  /** Cast `tree' to type `pt' */
  def mkAttributedCast(tree: Tree, pt: Type): Tree = {
    if (settings.debug.value) log("casting " + tree + ":" + tree.tpe + " to " + pt)
    assert(!tree.tpe.isInstanceOf[MethodType], tree)
    typer.typed {
      atPos(tree.pos) {
        Apply(TypeApply(mkAttributedSelect(tree, Object_asInstanceOf), List(TypeTree(pt))), List())
      }
    }
  }

  /** Builds a reference with stable type to given symbol */
  def mkAttributedStableRef(pre: Type, sym: Symbol): Tree =
    stabilize(mkAttributedRef(pre, sym))

  def mkAttributedStableRef(sym: Symbol): Tree =
    stabilize(mkAttributedRef(sym))

  def mkAttributedThis(sym: Symbol): Tree =
    This(sym.name) setSymbol sym setType sym.thisType

  def mkAttributedIdent(sym: Symbol): Tree = {
    assert(sym.isTerm)
    Ident(sym.name) setSymbol sym setType sym.tpe
  }

  def mkAttributedSelect(qual: Tree, sym: Symbol): Tree =
    if (qual.symbol != null &&
        (qual.symbol.name.toTermName == nme.ROOT ||
         qual.symbol.name.toTermName == nme.EMPTY_PACKAGE_NAME)) {
      mkAttributedIdent(sym)
    } else {
      assert(sym.isTerm)
      val result = Select(qual, sym.name) setSymbol sym
      if (qual.tpe != null) result setType qual.tpe.memberType(sym)
      result
    }

  /** Builds an instance test with given value and type. */
  def mkIsInstanceOf(value: Tree, tpe: Type, erased: Boolean): Tree = { // buraq: we ignore erase, no rtt
    val sym = definitions.Any_isInstanceOf
    /*
    val sym =
      if (erased) definitions.Any_isInstanceOfErased
      else definitions.Any_isInstanceOf
        */
    Apply(
      TypeApply(
        mkAttributedSelect(value, sym),
        List(TypeTree(tpe))),
      List())
  }

  def mkIsInstanceOf(value: Tree, tpe: Type): Tree = {
    mkIsInstanceOf(value, tpe, false/*global.phase.erasedTypes*/) // buraq: ignore which phase it is
  }

  /** Builds a cast with given value and type. */
  def mkAsInstanceOf(value: Tree, tpe: Type, erased: Boolean): Tree = {
    val sym =
      if (erased) definitions.Any_asInstanceOfErased
      else definitions.Any_asInstanceOf

    Apply(
      TypeApply(
        mkAttributedSelect(value, sym),
        List(TypeTree(tpe))),
      List())
  }

  def mkAsInstanceOf(value: Tree, tpe: Type): Tree =
    mkAsInstanceOf(value, tpe, global.phase.erasedTypes)

  /** Builds a list with given head and tail. */
  def mkNewCons(head: Tree, tail: Tree): Tree =
    New(Apply(mkAttributedRef(definitions.ConsClass), List(head, tail)))

  /** Builds a list with given head and tail. */
  def mkNil: Tree =
    mkAttributedRef(definitions.NilModule)

  /** Builds a pair */
  def mkNewPair(left: Tree, right: Tree) =
    New(Apply(mkAttributedRef(definitions.TupleClass(2)), List(left, right)))

  def mkCached(cvar: Symbol, expr: Tree): Tree = {
    val cvarRef = if (cvar.owner.isClass) Select(This(cvar.owner), cvar) else Ident(cvar)
    Block(
      List(
        If(Apply(Select(cvarRef, nme.eq), List(Literal(Constant(null)))),
           Assign(cvarRef, expr),
           EmptyTree)),
      cvarRef
    )
  }

  def mkRuntimeCall(meth: Name, args: List[Tree]): Tree =
    Apply(Select(mkAttributedRef(ScalaRunTimeModule), meth), args)
}
