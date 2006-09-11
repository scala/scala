/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$

package scala.tools.nsc.transform

/** A base class for transforms.
 *  A transform contains a compiler phase which applies a tree transformer.
 */
trait TypingTransformers {

  val global: Global
  import global._

  abstract class TypingTransformer(unit: CompilationUnit) extends Transformer {
    var localTyper: analyzer.Typer = analyzer.newTyper(
      analyzer.rootContext(unit, EmptyTree, true))
    private var curTree: Tree = _

    override def atOwner[A](owner: Symbol)(trans: => A): A = atOwner(curTree, owner)(trans)

    def atOwner[A](tree: Tree, owner: Symbol)(trans: => A): A = {
      val savedLocalTyper = localTyper
      localTyper = localTyper.atOwner(tree, owner)
      val result = super.atOwner(owner)(trans)
      localTyper = savedLocalTyper
      result
    }

    override def transform(tree: Tree): Tree = {
      curTree = tree
      tree match {
        case Template(_, _) =>
          // enter template into context chain
          atOwner(currentOwner) { super.transform(tree) }
        case _ =>
          super.transform(tree)
      }
    }
  }
}

