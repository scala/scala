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
	case MethodType(params, _) =>
	  assert(params.isEmpty, qual.tpe);
	  Apply(qual, List());
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

  def TypeTree(tp: Type) = global.TypeTree() setType tp;

  def This(sym: Symbol) =
    global.This(sym.name) setSymbol sym setType sym.thisType;

  def Ident(sym: Symbol) = {
    assert(sym.isTerm);
    sym.setFlag(ACCESSED);
    global.Ident(sym.name) setSymbol sym setType sym.tpe;
  }

  def Select(qual: Tree, sym: Symbol) = {
    assert(sym.isTerm);
    sym.setFlag(ACCESSED);
    global.Select(qual, sym.name) setSymbol sym setType qual.tpe.memberType(sym);
  }

  def Apply(fun: Tree, args: List[Tree]) = fun.tpe match {
    case MethodType(formals, restpe) =>
      global.Apply(fun, args) setType restpe
  }

  def Assign(lhs: Tree, rhs: Tree) =
    global.Assign(lhs, rhs) setType UnitClass.tpe;

  def ValDef(sym: Symbol, rhs: Tree): ValDef = atPos(sym.pos) {
    global.ValDef(flags2mods(sym.flags), sym.name, TypeTree(sym.tpe), rhs)
      setSymbol sym setType NoType
  }
  def ValDef(sym: Symbol): ValDef = ValDef(sym, EmptyTree);

  def AbsTypeDef(sym: Symbol) = atPos(sym.pos) {
    global.AbsTypeDef(flags2mods(sym.flags), sym.name,
                      TypeTree(sym.info.bounds.lo), TypeTree(sym.info.bounds.hi))
      setSymbol sym setType NoType
  }

  def DefDef(sym: Symbol, rhs: List[List[Symbol]] => Tree) = atPos(sym.pos) {
    var cnt = 0;
    def freshName = { cnt = cnt + 1; newTermName("x$" + cnt) }
    def mk(tparams: List[Symbol], vparamss: List[List[Symbol]], tpe: Type): DefDef = tpe match {
      case PolyType(tparams, restpe) =>
        mk(tparams, List(), restpe)
      case MethodType(formals, restpe) =>
        val vparams: List[Symbol] = formals map sym.newValueParameter(sym.pos, freshName).setInfo;
        mk(tparams, vparamss ::: List(vparams), restpe)
      case _ =>
        global.DefDef(
          flags2mods(sym.flags),
          sym.name,
          tparams.map(AbsTypeDef),
          vparamss.map(.map(ValDef)),
          TypeTree(tpe),
          rhs(vparamss))
	  setSymbol sym setType NoType
    }
    mk(List(), List(), sym.tpe)
  }
}
