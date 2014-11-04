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
    val runDefinitions = currentRun.runDefinitions
    import runDefinitions._

    val MacroContextUniverse = definitions.MacroContextUniverse
    val treeInfo.MacroImplReference(isBundle, _, _, macroImpl, _) = macroImplRef
    val paramss = macroImpl.paramss
    val ContextParam = paramss match {
      case Nil | _ :+ Nil                                       => NoSymbol // no implicit parameters in the signature => nothing to do
      case _ if isBundle                                        => macroImpl.owner.tpe member nme.c
      case (cparam :: _) :: _ if isMacroContextType(cparam.tpe) => cparam
      case _                                                    => NoSymbol // no context parameter in the signature => nothing to do
    }
    def transformTag(param: Symbol): Symbol = param.tpe.dealias match {
      case TypeRef(SingleType(SingleType(_, ContextParam), MacroContextUniverse), WeakTypeTagClass, targ :: Nil) => transform(param, targ.typeSymbol)
      case _                                                                                                     => param
    }
    ContextParam match {
      case NoSymbol => paramss
      case _        =>
        paramss.last map transformTag filter (_.exists) match {
          case Nil         => paramss.init
          case transformed => paramss.init :+ transformed
        }
    }
  }

  /** Increases metalevel of the type, i.e. transforms:
   *    * T to c.Expr[T]
   *
   *  @see Metalevels.scala for more information and examples about metalevels
   */
  def increaseMetalevel(pre: Type, tp: Type): Type =
    transparentShallowTransform(RepeatedParamClass, tp) {
      case tp => typeRef(pre, MacroContextExprClass, List(tp))
    }

  /** Transforms c.Expr[T] types into c.Tree and leaves the rest unchanged.
   */
  def untypeMetalevel(tp: Type): Type = {
    val runDefinitions = currentRun.runDefinitions
    import runDefinitions._

    transparentShallowTransform(RepeatedParamClass, tp) {
      case ExprClassOf(_) => typeRef(tp.prefix, TreesTreeType, Nil)
      case tp => tp
    }
  }

  /** Decreases metalevel of the type, i.e. transforms:
   *    * c.Expr[T] to T
   *    * Nothing to Nothing
   *    * Anything else to NoType
   *
   *  @see Metalevels.scala for more information and examples about metalevels
   */
  def decreaseMetalevel(tp: Type): Type = {
    val runDefinitions = currentRun.runDefinitions
    import runDefinitions._
    transparentShallowTransform(RepeatedParamClass, tp) {
      case ExprClassOf(runtimeType) => runtimeType
      // special-casing Nothing here is a useful convention
      // that enables no-hassle prototyping with `macro ???` and `macro { ...; ??? }`
      case nothing if nothing =:= NothingTpe => NothingTpe
      case _ => NoType
    }
  }
}
