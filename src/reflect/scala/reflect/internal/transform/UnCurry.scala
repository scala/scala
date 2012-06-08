package scala.reflect
package internal
package transform

import Flags._

trait UnCurry {

  val global: SymbolTable
  import global._
  import definitions._

  private def expandAlias(tp: Type): Type = if (!tp.isHigherKinded) tp.normalize else tp

  val uncurry: TypeMap = new TypeMap {
    def apply(tp0: Type): Type = {
      val tp = expandAlias(tp0)
      tp match {
        case MethodType(params, MethodType(params1, restpe)) =>
          apply(MethodType(params ::: params1, restpe))
        case MethodType(params, ExistentialType(tparams, restpe @ MethodType(_, _))) =>
          assert(false, "unexpected curried method types with intervening existential")
          tp0
        case MethodType(h :: t, restpe) if h.isImplicit =>
          apply(MethodType(h.cloneSymbol.resetFlag(IMPLICIT) :: t, restpe))
        case NullaryMethodType(restpe) =>
          apply(MethodType(List(), restpe))
        case TypeRef(pre, ByNameParamClass, arg :: Nil) =>
          apply(functionType(List(), arg))
        case TypeRef(pre, RepeatedParamClass, arg :: Nil) =>
          apply(seqType(arg))
        case TypeRef(pre, JavaRepeatedParamClass, arg :: Nil) =>
          apply(arrayType(
            if (isUnboundedGeneric(arg)) ObjectClass.tpe else arg))
        case _ =>
          expandAlias(mapOver(tp))
      }
    }
  }

  private val uncurryType = new TypeMap {
    def apply(tp0: Type): Type = {
      val tp = expandAlias(tp0)
      tp match {
        case ClassInfoType(parents, decls, clazz) =>
          val parents1 = parents mapConserve uncurry
          if (parents1 eq parents) tp
          else ClassInfoType(parents1, decls, clazz) // @MAT normalize in decls??
        case PolyType(_, _) =>
          mapOver(tp)
        case _ =>
          tp
      }
    }
  }

  /** - return symbol's transformed type,
   *  - if symbol is a def parameter with transformed type T, return () => T
   *
   * @MAT: starting with this phase, the info of every symbol will be normalized
   */
  def transformInfo(sym: Symbol, tp: Type): Type =
    if (sym.isType) uncurryType(tp) else uncurry(tp)
}