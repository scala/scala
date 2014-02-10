/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin odersky
 */
package scala.tools.nsc
package transform

/** This phase maps ErasedValueTypes to the underlying unboxed representation and
 *  performs peephole optimizations.
 */
trait PostErasure extends InfoTransform with TypingTransformers with scala.reflect.internal.transform.PostErasure {
  val global: Global

  import global._
  import treeInfo._

  val phaseName: String = "posterasure"

  def newTransformer(unit: CompilationUnit): Transformer = new PostErasureTransformer(unit)
  override def changesBaseClasses = false

  class PostErasureTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree: Tree) = {
      def finish(res: Tree) = logResult(s"Posterasure reduction\n  Old: $tree\n  New")(res)

      /* We use the name of the operation being performed and not the symbol
       * itself because the symbol hails from the boxed class, and this transformation
       * exists to operate directly on the values. So we are for instance looking
       * up == on an lhs of type Int, whereas the symbol which has been passed in
       * is from java.lang.Integer.
       */
      def binop(lhs: Tree, op: Symbol, rhs: Tree) =
        finish(localTyper typed (Apply(Select(lhs, op.name) setPos tree.pos, rhs :: Nil) setPos tree.pos))

      super.transform(tree) setType elimErasedValueType(tree.tpe) match {
        case AsInstanceOf(v, tpe) if v.tpe <:< tpe => finish(v)          // x.asInstanceOf[X]       ==> x
        case ValueClass.BoxAndUnbox(v)             => finish(v)          // (new B(v)).unbox        ==> v
        case ValueClass.BoxAndCompare(v1, op, v2)  => binop(v1, op, v2)  // new B(v1) == new B(v2)  ==> v1 == v2
        case tree                                  => tree
      }
    }
  }
}
