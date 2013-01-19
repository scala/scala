package scala.reflect
package macros

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  A slice of [[scala.reflect.macros.Context the Scala macros context]] that provides
 *  facilities to drive type inference.
 */
trait Infer {
  self: Context =>

  /** This is the control handle provided to the macro in the type inference callback
   *  declared in `scala.reflect.macros.Macro.onInfer`.
   *  Modelled closely after `inferExprInstance` and `inferMethodInstance`.
   *  Refer to http://docs.scala-lang.org/overviews/macros/inference.html for documentation.
   */
  type TypeInferenceContext >: Null <: TypeInferenceContextApi

  trait TypeInferenceContextApi {
    def tree: Tree
    def unknowns: List[Symbol]
    def expectedType: Type
    def actualType: Type

    // TODO: can we get rid of this couple?
    def keepNothings: Boolean
    def useWeaklyCompatible: Boolean

    def infer(sym: Symbol, tpe: Type): Unit

    // TODO: would be lovely to have a different signature here, namely:
    // def inferDefault(sym: Symbol): Type
    // so that the macro can partially rely on out-of-the-box inference
    // and infer the rest afterwards
    def inferDefault(): Unit
  }
}