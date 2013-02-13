/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin odersky
 */
package scala.tools.nsc
package transform

/** This phase maps ErasedValueTypes to the underlying unboxed representation and
 *  performs peephole optimizations.
 */
trait PostErasure extends InfoTransform with TypingTransformers {
  val global: Global

  import global._
  import treeInfo._
  import treeInfo.{ ValueClass => VC }
  import definitions.{ Object_==, Object_!= }

  val phaseName: String = "posterasure"

  def newTransformer(unit: CompilationUnit): Transformer = new PostErasureTransformer(unit)
  override def changesBaseClasses = false

  object elimErasedValueType extends TypeMap {
    def apply(tp: Type) = tp match {
      case ConstantType(Constant(tp: Type)) => ConstantType(Constant(apply(tp)))
      case ErasedValueType(tref)            => enteringErasure(erasure.erasedValueClassArg(tref))
      case _                                => mapOver(tp)
    }
  }

  def transformInfo(sym: Symbol, tp: Type) = elimErasedValueType(tp)

  class PostErasureTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree: Tree) = {
      def finish(res: Tree) = logResult(s"Value class simplification\n  Old: $tree\n  New")(res)
      def compare(lhs: Tree, cmp: Symbol, rhs: Tree) =
        finish(localTyper typed (Apply(Select(lhs, cmp) setPos tree.pos, rhs :: Nil) setPos tree.pos))

      super.transform(tree) setType elimErasedValueType(tree.tpe) match {
        case VC.Underlying(arg)                                   => finish(arg)            //      new C(arg).underlying  ==>  arg
        case AsInstanceOf(arg, tpe) if arg.tpe <:< tpe            => finish(arg)            //        arg.asInstanceOf[T]  ==>  arg
        case VC.BinaryOp(lhs, cmp @ (Object_== | Object_!=), rhs) => compare(lhs, cmp, rhs) // new C(arg1) == new C(arg2)  ==>  arg1 == arg2
        case tree                                                 => tree
      }
    }
  }
}
