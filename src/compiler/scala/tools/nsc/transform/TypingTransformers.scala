/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$

package scala.tools.nsc.transform

import scala.collection.mutable.{Map, HashMap}

/** A base class for transforms.
 *  A transform contains a compiler phase which applies a tree transformer.
 */
trait TypingTransformers {

  val global: Global
  import global._

  abstract class TypingTransformer(unit: CompilationUnit) extends Transformer {
    var localTyper: analyzer.Typer = analyzer.newTyper(
      analyzer.rootContext(unit, EmptyTree, true))
    protected var curTree: Tree = _

    /** a typer for each enclosing class */
    var typers: Map[Symbol, analyzer.Typer] = new HashMap

    override def atOwner[A](owner: Symbol)(trans: => A): A = atOwner(curTree, owner)(trans)

    def atOwner[A](tree: Tree, owner: Symbol)(trans: => A): A = {
      val savedLocalTyper = localTyper
//      println("ttransformer atOwner: " + owner + " isPackage? " + owner.isPackage)
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

