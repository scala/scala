package scala.tools.nsc
package transform

abstract class Delambdafy extends Transform with TypingTransformers with ast.TreeDSL {
  import global._

  /** the following two members override abstract members in Transform */
  val phaseName: String = "delambdafy"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new DelambdafyTransformer(unit)

  class DelambdafyTransformer(unit: CompilationUnit) extends TypingTransformer(unit) { 
    override def transform(tree: Tree): Tree = tree
  }
}
