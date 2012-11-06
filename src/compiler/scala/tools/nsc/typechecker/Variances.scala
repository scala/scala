/* NSC -- new scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import symtab.Flags.{ VarianceFlags => VARIANCES, _ }

/** Variances form a lattice, 0 <= COVARIANT <= Variances, 0 <= CONTRAVARIANT <= VARIANCES
 */
trait Variances {

  val global: Global
  import global._

  /** Flip between covariant and contravariant */
  private def flip(v: Int): Int = {
    if (v == COVARIANT) CONTRAVARIANT
    else if (v == CONTRAVARIANT) COVARIANT
    else v
  }

  /** Map everything below VARIANCES to 0 */
  private def cut(v: Int): Int =
    if (v == VARIANCES) v else 0

  /** Compute variance of type parameter `tparam` in types of all symbols `sym`. */
  def varianceInSyms(syms: List[Symbol])(tparam: Symbol): Int =
    (VARIANCES /: syms) ((v, sym) => v & varianceInSym(sym)(tparam))

  /** Compute variance of type parameter `tparam` in type of symbol `sym`. */
  def varianceInSym(sym: Symbol)(tparam: Symbol): Int =
    if (sym.isAliasType) cut(varianceInType(sym.info)(tparam))
    else varianceInType(sym.info)(tparam)

  /** Compute variance of type parameter `tparam` in all types `tps`. */
  def varianceInTypes(tps: List[Type])(tparam: Symbol): Int =
    (VARIANCES /: tps) ((v, tp) => v & varianceInType(tp)(tparam))

  /** Compute variance of type parameter `tparam` in all type arguments
   *  <code>tps</code> which correspond to formal type parameters `tparams1`.
   */
  def varianceInArgs(tps: List[Type], tparams1: List[Symbol])(tparam: Symbol): Int = {
    var v: Int = VARIANCES;
    for ((tp, tparam1) <- tps zip tparams1) {
      val v1 = varianceInType(tp)(tparam)
      v = v & (if (tparam1.isCovariant) v1
	       else if (tparam1.isContravariant) flip(v1)
	       else cut(v1))
    }
    v
  }

  /** Compute variance of type parameter `tparam` in all type annotations `annots`. */
  def varianceInAttribs(annots: List[AnnotationInfo])(tparam: Symbol): Int = {
    (VARIANCES /: annots) ((v, annot) => v & varianceInAttrib(annot)(tparam))
  }

  /** Compute variance of type parameter `tparam` in type annotation `annot`. */
  def varianceInAttrib(annot: AnnotationInfo)(tparam: Symbol): Int = {
    varianceInType(annot.atp)(tparam)
  }

  /** Compute variance of type parameter <code>tparam</code> in type <code>tp</code>. */
  def varianceInType(tp: Type)(tparam: Symbol): Int = tp match {
    case ErrorType | WildcardType | NoType | NoPrefix | ThisType(_) | ConstantType(_) =>
      VARIANCES
    case BoundedWildcardType(bounds) =>
      varianceInType(bounds)(tparam)
    case SingleType(pre, sym) =>
      varianceInType(pre)(tparam)
    case TypeRef(pre, sym, args) =>
      if (sym == tparam) COVARIANT
      // tparam cannot occur in tp's args if tp is a type constructor (those don't have args)
      else if (tp.isHigherKinded) varianceInType(pre)(tparam)
      else varianceInType(pre)(tparam) & varianceInArgs(args, sym.typeParams)(tparam)
    case TypeBounds(lo, hi) =>
      flip(varianceInType(lo)(tparam)) & varianceInType(hi)(tparam)
    case RefinedType(parents, defs) =>
      varianceInTypes(parents)(tparam) & varianceInSyms(defs.toList)(tparam)
    case MethodType(params, restpe) =>
      flip(varianceInSyms(params)(tparam)) & varianceInType(restpe)(tparam)
    case NullaryMethodType(restpe) =>
      varianceInType(restpe)(tparam)
    case PolyType(tparams, restpe) =>
      flip(varianceInSyms(tparams)(tparam)) & varianceInType(restpe)(tparam)
    case ExistentialType(tparams, restpe) =>
      varianceInSyms(tparams)(tparam) & varianceInType(restpe)(tparam)
    case AnnotatedType(annots, tp, _) =>
      varianceInAttribs(annots)(tparam) & varianceInType(tp)(tparam)
  }
}
