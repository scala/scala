package scala.reflect.macros
package runtime

trait Infer {
  self: Context =>

  import universe._

  trait TypeInferenceContext extends TypeInferenceContextApi

  class InferExprInstanceContext(val tree: Tree, var unknowns: List[Symbol], val expectedType: Type, val actualType: Type,
                                 val keepNothings: Boolean, val useWeaklyCompatible: Boolean)
                                 extends TypeInferenceContext {
    def infer(sym: Symbol, tpe: Type): Unit = {
      callsiteTyper.infer.substExpr(tree, List(sym), List(tpe), expectedType)
      unknowns = unknowns diff List(sym)
    }
    def inferDefault(): Unit = {
      val stillUnknown = callsiteTyper.infer.inferExprInstance(tree, unknowns, expectedType, actualType, keepNothings, useWeaklyCompatible, allowMacroHelpers = false)
      unknowns = stillUnknown
    }
  }

  class InferMethodInstanceContext(val tree: Tree, var unknowns: List[Symbol], val expectedType: Type)
                                   extends TypeInferenceContext {
    val Apply(fn, args) = tree
    def actualType = fn.tpe
    def keepNothings = true
    def useWeaklyCompatible = false
    def infer(sym: Symbol, tpe: Type): Unit = ??? // TODO: implement this
    def inferDefault(): Unit = {
      // TODO: don't forget to call `notifyUndetparamsInferred`
      val stillUnknown = callsiteTyper.infer.inferMethodInstance(fn, unknowns, args, expectedType, allowMacroHelpers = false)
      unknowns = stillUnknown
    }
  }
}