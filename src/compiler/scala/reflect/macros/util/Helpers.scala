package scala.reflect.macros
package util

import scala.tools.nsc.typechecker.Analyzer

trait Helpers {
  self: Analyzer =>

  import global._
  import definitions._

  /** Transforms parameters lists of a macro impl.
   *  The `transform` function is invoked only for WeakTypeTag evidence parameters.
   *
   *  The transformer takes two arguments: a value parameter from the parameter list
   *  and a type parameter that is witnesses by the value parameter.
   *
   *  If the transformer returns a NoSymbol, the value parameter is not included from the result.
   *  If the transformer returns something else, this something else is included in the result instead of the value parameter.
   *
   *  Despite of being highly esoteric, this function significantly simplifies signature analysis.
   *  For example, it can be used to strip macroImpl.paramss from the evidences (necessary when checking def <-> impl correspondence)
   *  or to streamline creation of the list of macro arguments.
   */
  def transformTypeTagEvidenceParams(macroImplRef: Tree, transform: (Symbol, Symbol) => Symbol): List[List[Symbol]] = {
    val treeInfo.MacroImplReference(isBundle, _, macroImpl, _) = macroImplRef
    val paramss = macroImpl.paramss
    if (paramss.isEmpty || paramss.last.isEmpty) return paramss // no implicit parameters in the signature => nothing to do
    val rc =
      if (isBundle) macroImpl.owner.tpe.member(nme.c)
      else {
        def cparam = paramss.head.head
        if (paramss.head.isEmpty || !(cparam.tpe <:< MacroContextClass.tpe)) return paramss // no context parameter in the signature => nothing to do
        cparam
      }
    def transformTag(param: Symbol): Symbol = param.tpe.dealias match {
      case TypeRef(SingleType(SingleType(_, ac), universe), WeakTypeTagClass, targ :: Nil)
      if ac == rc && universe == MacroContextUniverse =>
        transform(param, targ.typeSymbol)
      case _ =>
        param
    }
    val transformed = paramss.last map transformTag filter (_ ne NoSymbol)
    if (transformed.isEmpty) paramss.init else paramss.init :+ transformed
  }

  /** Increases metalevel of the type, i.e. transforms:
   *    * T to c.Expr[T]
   *
   *  @see Metalevels.scala for more information and examples about metalevels
   */
  def increaseMetalevel(pre: Type, tp: Type): Type = transparentShallowTransform(RepeatedParamClass, tp) {
    case tp => typeRef(pre, MacroContextExprClass, List(tp))
  }

  /** Decreases metalevel of the type, i.e. transforms:
   *    * c.Expr[T] to T
   *    * Anything else to Any
   *
   *  @see Metalevels.scala for more information and examples about metalevels
   */
  def decreaseMetalevel(tp: Type): Type = transparentShallowTransform(RepeatedParamClass, tp) {
    case ExprClassOf(runtimeType) => runtimeType
    case _ => AnyTpe // so that macro impls with rhs = ??? don't screw up our inference
  }
}