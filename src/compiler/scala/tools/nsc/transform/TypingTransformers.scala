/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package transform

/** A base class for transforms.
 *  A transform contains a compiler phase which applies a tree transformer.
 */
trait TypingTransformers {

  val global: Global
  import global._

  abstract class TypingTransformer(unit: CompilationUnit) extends Transformer {
    var localTyper: analyzer.Typer =
      if (phase.erasedTypes)
        erasure.newTyper(erasure.rootContextPostTyper(unit, EmptyTree)).asInstanceOf[analyzer.Typer]
      else // TODO: AM: should some phases use a regular rootContext instead of a post-typer one??
        analyzer.newTyper(analyzer.rootContextPostTyper(unit, EmptyTree))
    protected var curTree: Tree = _

    override final def atOwner[A](owner: Symbol)(trans: => A): A = atOwner(curTree, owner)(trans)

    def atOwner[A](tree: Tree, owner: Symbol)(trans: => A): A = {
      val savedContext = localTyper.context
      localTyper.context = localTyper.context.make(tree, if (owner.isModuleNotMethod) owner.moduleClass else owner)
      val result = super.atOwner(owner)(trans)
      localTyper.context = savedContext
      result
    }

    override def transform(tree: Tree): Tree = {
      curTree = tree
      tree match {
        case Template(_, _, _) =>
          // enter template into context chain
          atOwner(currentOwner) { tree.transform(this) }
        case PackageDef(_, _) =>
          atOwner(tree.symbol) { tree.transform(this) }
        case _ =>
          tree.transform(this)
      }
    }
  }
}

