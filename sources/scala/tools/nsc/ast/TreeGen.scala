/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.ast;

import scala.tools.util.Position;
import symtab.Flags._;

abstract class TreeGen {

  val global: Global;

  import global._;
  import definitions._;
  import posAssigner.atPos;

  /** Builds a reference to value whose type is given stable prefix.
   */
  def mkQualifier(stable: Type): Tree = stable match {
    case NoPrefix =>
      EmptyTree
    case ThisType(clazz) =>
      if (clazz.isRoot || clazz.isEmptyPackageClass) EmptyTree else This(clazz)
    case SingleType(pre, sym) =>
      val qual = mkStableRef(pre, sym);
      qual.tpe match {
	case MethodType(List(), restpe) =>
	  Apply(qual, List()) setType restpe
        case _ =>
          qual
      }
  }

  /** Builds a reference to given symbol with given stable prefix. */
  def mkRef(pre: Type, sym: Symbol): Tree  = {
    val qual = mkQualifier(pre);
    if (qual == EmptyTree) Ident(sym) else Select(qual, sym)
  }

  /** Builds a reference to given symbol. */
  def mkRef(sym: Symbol): Tree = mkRef(sym.owner.thisType, sym);

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

  /** Builds a reference with stable type to given symbol */
  def mkStableRef(pre: Type, sym: Symbol): Tree  = stabilize(mkRef(pre, sym));
  def mkStableRef(sym: Symbol): Tree  = stabilize(mkRef(sym));

  def This(sym: Symbol): Tree =
    global.This(sym.name) setSymbol sym setType atPhase(phase.next)(sym.thisType);

  def Ident(sym: Symbol) = {
    assert(sym.isTerm);
    sym.setFlag(ACCESSED);
    global.Ident(sym.name) setSymbol sym setType atPhase(phase.next)(sym.tpe);
  }

  def Select(qual: Tree, sym: Symbol) = {
    assert(sym.isTerm);
    sym.setFlag(ACCESSED);
    val result = global.Select(qual, sym.name) setSymbol sym;
    if (qual.tpe != null) result setType atPhase(phase.next)(qual.tpe.memberType(sym));
    result
  }
}
