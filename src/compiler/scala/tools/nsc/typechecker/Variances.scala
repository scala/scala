/* NSC -- new scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

// import scala.reflect.internal.Variance

/** See comments at scala.reflect.internal.Variance.
 */
trait Variances {
  val global: Global
  import global._
  import scala.reflect.internal.Variance._

  /** Compute variance of type parameter `tparam` in all types `tps`. */
  def varianceInTypes(tps: List[Type])(tparam: Symbol): Variance =
    fold(tps map (tp => varianceInType(tp)(tparam)))

  /** Compute variance of type parameter `tparam` in type `tp`. */
  def varianceInType(tp: Type)(tparam: Symbol): Variance = {
    def inArgs(sym: Symbol, args: List[Type]): Variance = fold(map2(args, sym.typeParams)((a, p) => inType(a) * p.variance))
    def inSyms(syms: List[Symbol]): Variance            = fold(syms map inSym)
    def inTypes(tps: List[Type]): Variance              = fold(tps map inType)

    def inSym(sym: Symbol): Variance = if (sym.isAliasType) inType(sym.info).cut else inType(sym.info)
    def inType(tp: Type): Variance   = tp match {
      case ErrorType | WildcardType | NoType | NoPrefix => Bivariant
      case ThisType(_) | ConstantType(_)                => Bivariant
      case TypeRef(_, `tparam`, _)                      => Covariant
      case BoundedWildcardType(bounds)                  => inType(bounds)
      case NullaryMethodType(restpe)                    => inType(restpe)
      case SingleType(pre, sym)                         => inType(pre)
      case TypeRef(pre, _, _) if tp.isHigherKinded      => inType(pre)                 // a type constructor cannot occur in tp's args
      case TypeRef(pre, sym, args)                      => inType(pre)                 & inArgs(sym, args)
      case TypeBounds(lo, hi)                           => inType(lo).flip             & inType(hi)
      case RefinedType(parents, defs)                   => inTypes(parents)            & inSyms(defs.toList)
      case MethodType(params, restpe)                   => inSyms(params).flip         & inType(restpe)
      case PolyType(tparams, restpe)                    => inSyms(tparams).flip        & inType(restpe)
      case ExistentialType(tparams, restpe)             => inSyms(tparams)             & inType(restpe)
      case AnnotatedType(annots, tp, _)                 => inTypes(annots map (_.atp)) & inType(tp)
    }

    inType(tp)
  }
}
