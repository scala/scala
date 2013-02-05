package scala.reflect.macros
package runtime

import scala.collection.mutable.ArrayBuffer

trait Infer {
  self: Context =>

  import universe._

  abstract class TypeInferenceContext(val tree: Tree, val unknowns: List[Symbol], val expectedType: Type, val actualType: Type) extends TypeInferenceContextApi {
    val _inferences = ArrayBuffer[Option[Type]]()
    def inferences = _inferences.toList
    inferDefault()

    def infer(sym: Symbol, tpe: Type): Unit = {
      val i = unknowns.indexOf(sym)
      assert(i != -1, s"unknowns = $unknowns, sym = $sym")
      _inferences(i) = Some(tpe)
    }

    def inferDefault(): Unit = {
      _inferences.clear()
      _inferences ++= unknowns.map(_ => None)
    }
  }

  class InferExprInstanceContext(tree: Tree, unknowns: List[Symbol], expectedType: Type, actualType: Type,
                                 val keepNothings: Boolean, val useWeaklyCompatible: Boolean)
                                 extends TypeInferenceContext(tree, unknowns, expectedType, actualType)

  class InferMethodInstanceContext(tree: Tree, unknowns: List[Symbol], expectedType: Type)
                                   extends TypeInferenceContext(tree, unknowns, expectedType, tree.asInstanceOf[Apply].fun.tpe) {
    def keepNothings = false
    def useWeaklyCompatible = false
  }
}