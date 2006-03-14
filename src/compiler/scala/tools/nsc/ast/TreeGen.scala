/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.ast;

import scala.tools.nsc.util.Position;
import symtab.Flags._;

abstract class TreeGen {

  val global: Global;

  import global._;
  import definitions._;
  import posAssigner.atPos;

  /** Builds a reference to value whose type is given stable prefix.
   */
  def mkQualifier(tpe: Type): Tree = tpe match {
    case NoPrefix =>
      EmptyTree
    case ThisType(clazz) =>
      if (clazz.isRoot || clazz.isEmptyPackageClass) EmptyTree else This(clazz)
    case SingleType(pre, sym) =>
      if (sym.isThisSkolem) {
        mkQualifier(ThisType(sym.deSkolemize))
      } else {
        val qual = mkStableRef(pre, sym);
        qual.tpe match {
          case MethodType(List(), restpe) =>
            Apply(qual, List()) setType restpe
          case _ =>
            qual
        }
      }
    case TypeRef(pre, sym, args) =>
      assert(phase.erasedTypes);
      if (sym.isModuleClass && !sym.isRoot) {
        val qual = Select(mkQualifier(sym.owner.tpe), sym.sourceModule);
        qual.tpe match {
	  case MethodType(List(), restpe) =>
	    Apply(qual, List()) setType restpe
          case _ =>
            qual
        }
      } else This(sym)
    case _ =>
      throw new Error("bad qualifier: " + tpe)
  }

  /** Builds a reference to given symbol with given stable prefix. */
  def mkRef(pre: Type, sym: Symbol): Tree  = {
    val qual = mkQualifier(pre);
    if (qual == EmptyTree) Ident(sym) else Select(qual, sym)
  }

  /** Builds a reference to given symbol. */
  def mkRef(sym: Symbol): Tree =
    if (sym.owner.isClass) mkRef(sym.owner.thisType, sym) else Ident(sym);

  /** Replaces tree type with a stable type if possible */
  def stabilize(tree: Tree): Tree = tree match {
    case Ident(_) =>
      if (tree.symbol.isStable) tree.setType(singleType(tree.symbol.owner.thisType, tree.symbol))
      else tree
    case Select(qual, _) =>
      if (tree.symbol.isStable && qual.tpe.isStable) tree.setType(singleType(qual.tpe, tree.symbol))
      else tree
    case _ =>
      tree
  }

  /** Cast `tree' to type `pt' */
  def cast(tree: Tree, pt: Type): Tree = {
    if (settings.debug.value) log("casting " + tree + ":" + tree.tpe + " to " + pt);
    assert(!tree.tpe.isInstanceOf[MethodType], tree);
    typer.typed {
      atPos(tree.pos) {
	Apply(TypeApply(Select(tree, Object_asInstanceOf), List(TypeTree(pt))), List())
      }
    }
  }

  /** Builds a reference with stable type to given symbol */
  def mkStableRef(pre: Type, sym: Symbol): Tree  = stabilize(mkRef(pre, sym));
  def mkStableRef(sym: Symbol): Tree  = stabilize(mkRef(sym));

  def This(sym: Symbol): Tree =
    global.This(sym.name) setSymbol sym setType sym.thisType;

  def Ident(sym: Symbol): Tree = {
    assert(sym.isTerm);
    global.Ident(sym.name) setSymbol sym setType sym.tpe;
  }

  def Select(qual: Tree, sym: Symbol): Tree =
    if (qual.symbol != null &&
        (qual.symbol.name.toTermName == nme.ROOT ||
         qual.symbol.name.toTermName == nme.EMPTY_PACKAGE_NAME)) {
      this.Ident(sym)
    } else {
      assert(sym.isTerm);
      val result = global.Select(qual, sym.name) setSymbol sym;
      if (qual.tpe != null) result setType qual.tpe.memberType(sym);
      result
    }

  /** Builds an instance test with given value and type. */
  def mkIsInstanceOf(value: Tree, tpe: Type, erased: Boolean): Tree = { // buraq: we ignore erase, no rtt
    val sym = definitions.Any_isInstanceOf;
    /*
    val sym =
      if(erased)
        definitions.Any_isInstanceOfErased
      else
        definitions.Any_isInstanceOf;
        */
    Apply(
      TypeApply(
        Select(value, sym),
        List(TypeTree(tpe))),
      List())
  }

  def mkIsInstanceOf(value: Tree, tpe: Type): Tree = {
    mkIsInstanceOf(value, tpe, false/*global.phase.erasedTypes*/); // buraq: ignore which phase it is
  }

  /** Builds a cast with given value and type. */
  def mkAsInstanceOf(value: Tree, tpe: Type, erased: Boolean): Tree = {
    val sym =
      if(erased)
        definitions.Any_asInstanceOfErased
      else
        definitions.Any_asInstanceOf;

    Apply(
      TypeApply(
        Select(value, sym),
        List(TypeTree(tpe))),
      List())
  }

  def mkAsInstanceOf(value: Tree, tpe: Type): Tree = {
    mkAsInstanceOf(value, tpe, global.phase.erasedTypes);
  }


  /** Builds a list with given head and tail. */
  def mkNewCons(head: Tree, tail: Tree):  Tree =
    New(Apply(mkRef(definitions.ConsClass), List(head,tail)));

  /** Builds a list with given head and tail. */
  def mkNil: Tree =
    mkRef(definitions.NilModule);

  /** Builds a pair */
  def mkNewPair(left: Tree, right: Tree) =
    New(Apply(mkRef(definitions.TupleClass(2)), List(left,right)));

}
