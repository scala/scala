/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

import scala.collection.{ mutable, immutable }

/** A base class for transforms.
 *  A transform contains a compiler phase which applies a tree transformer.
 */
trait TypingTransformers {

  val global: Global
  import global._

  abstract class TypingTransformer(unit: CompilationUnit) extends Transformer {
    var localTyper: analyzer.Typer =
      if (phase.erasedTypes)
        erasure.newTyper(erasure.rootContext(unit, EmptyTree, true)).asInstanceOf[analyzer.Typer]
      else
        analyzer.newTyper(analyzer.rootContext(unit, EmptyTree, true))
    protected var curTree: Tree = _
    protected def typedPos(pos: Position)(tree: Tree) = localTyper typed { atPos(pos)(tree) }

    /** a typer for each enclosing class */
    val typers: mutable.Map[Symbol, analyzer.Typer] = new mutable.HashMap

    override def atOwner[A](owner: Symbol)(trans: => A): A = atOwner(curTree, owner)(trans)

    def atOwner[A](tree: Tree, owner: Symbol)(trans: => A): A = {
      val savedLocalTyper = localTyper
//      println("transformer atOwner: " + owner + " isPackage? " + owner.isPackage)
      localTyper = localTyper.atOwner(tree, if (owner.isModule) owner.moduleClass else owner)
      typers += Pair(owner, localTyper)
      val result = super.atOwner(owner)(trans)
      localTyper = savedLocalTyper
      typers -= owner
      result
    }

    override def transform(tree: Tree): Tree = {
      curTree = tree
      tree match {
        case Template(_, _, _) =>
          // enter template into context chain
          atOwner(currentOwner) { super.transform(tree) }
        case PackageDef(_, _) =>
          atOwner(tree.symbol) { super.transform(tree) }
        case _ =>
          super.transform(tree)
      }
    }
  }
}

