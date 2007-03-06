/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.typechecker;

import symtab.Flags._;

/** Variances form a lattice, 0 <= COVARIANT <= Variances, 0 <= CONTRAVARIANT <= VARIANCES
 */
trait Variances {

  val global: Global;
  import global._;

  /** Convert variance to string */
  private def varianceString(variance: int): String =
    if (variance == COVARIANT) "covariant"
    else if (variance == CONTRAVARIANT) "contravariant"
    else "invariant";

  /** Flip between covariant and contravariant */
  private def flip(v: int): int = {
    if (v == COVARIANT) CONTRAVARIANT;
    else if (v == CONTRAVARIANT) COVARIANT;
    else v
  }

  private def compose(v1: int, v2: int) =
    if (v1 == 0) 0
    else if (v1 == CONTRAVARIANT) flip(v2)
    else v2;

  /** Map everything below VARIANCES to 0 */
  private def cut(v: int): int =
    if (v == VARIANCES) v else 0;

  /** Compute variance of type parameter `tparam' in types of all symbols `sym'. */
  def varianceInSyms(syms: List[Symbol])(tparam: Symbol): int =
    (VARIANCES /: syms) ((v, sym) => v & varianceInSym(sym)(tparam));

  /** Compute variance of type parameter `tparam' in type of symbol `sym'. */
  def varianceInSym(sym: Symbol)(tparam: Symbol): int =
    if (sym.isAliasType) cut(varianceInType(sym.info)(tparam))
    else varianceInType(sym.info)(tparam);

  /** Compute variance of type parameter `tparam' in all types `tps'. */
  def varianceInTypes(tps: List[Type])(tparam: Symbol): int =
    (VARIANCES /: tps) ((v, tp) => v & varianceInType(tp)(tparam));

  /** Compute variance of type parameter `tparam' in all type arguments
   *  `tps' which correspond to formal type parameters `tparams1'. */
  def varianceInArgs(tps: List[Type], tparams1: List[Symbol])(tparam: Symbol): int = {
    var v: int = VARIANCES;
    for (val (tp, tparam1) <- tps zip tparams1) {
      val v1 = varianceInType(tp)(tparam);
      v = v & (if (tparam1.isCovariant) v1
	       else if (tparam1.isContravariant) flip(v1)
	       else cut(v1))
    }
    v
  }

  /** Compute variance of type parameter `tparam' in type `tp'. */
  def varianceInType(tp: Type)(tparam: Symbol): int = tp match {
    case ErrorType | WildcardType | NoType | NoPrefix | ThisType(_) | ConstantType(_) =>
      VARIANCES
    case SingleType(pre, sym) =>
      varianceInType(pre)(tparam)
    case TypeRef(pre, sym, args) =>
      if (sym == tparam) COVARIANT
      else varianceInType(pre)(tparam) & varianceInArgs(args, sym.typeParams)(tparam)
    case TypeBounds(lo, hi) =>
      flip(varianceInType(lo)(tparam)) & varianceInType(hi)(tparam)
    case RefinedType(parents, defs) =>
      varianceInTypes(parents)(tparam) & varianceInSyms(defs.toList)(tparam)
    case MethodType(formals, restpe) =>
      flip(varianceInTypes(formals)(tparam)) & varianceInType(restpe)(tparam)
    case PolyType(tparams, restpe) =>
      flip(varianceInSyms(tparams)(tparam)) & varianceInType(restpe)(tparam)
    case AnnotatedType(attribs, tp) =>
      varianceInType(tp)(tparam)
  }
}
