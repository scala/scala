/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package transform

import scala.collection.mutable


/** A base class for transforms.
 *  A transform contains a compiler phase which applies a tree transformer.
 */
trait TypingTransformers {

  val global: Global
  import global._

  protected def newRootLocalTyper(unit: CompilationUnit): analyzer.Typer = if (phase.erasedTypes)
    erasure.newTyper(erasure.rootContextPostTyper(unit, EmptyTree)).asInstanceOf[analyzer.Typer]
  else // TODO: AM: should some phases use a regular rootContext instead of a post-typer one??
    analyzer.newTyper(analyzer.rootContextPostTyper(unit, EmptyTree))

  abstract class TypingTransformer(initLocalTyper: global.analyzer.Typer) extends AstTransformer {
    def this(unit: CompilationUnit) = this(newRootLocalTyper(unit))
    var localTyper: analyzer.Typer = initLocalTyper
    currentOwner = localTyper.context.owner
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
    def transformAtOwner(owner: Symbol, tree: Tree): Tree = atOwner(tree, owner) { transform(tree) }
  }

  private object ThicketAttachment
  /** A base class for typing transformers that need to perform "thicket expansion". A thicket is the output of a
   *  transformation that is flattened into the enclosing block.
   */
  abstract class ThicketTransformer(initLocalTyper: analyzer.Typer) extends TypingTransformer(initLocalTyper) {
    private def expandThicket(t: Tree): List[Tree] = t match {
      case Block(stats, expr) if t.attachments.containsElement(ThicketAttachment) =>
        stats :+ expr
      case _ => t :: Nil
    }

    def apply(tree: Tree): List[Tree] = expandThicket(transform(tree))

    protected def Thicket(stats: List[Tree], expr: Tree): Tree = {
      Block(stats, expr).updateAttachment(ThicketAttachment)
    }
    protected def Thicket(block: Block): Tree = {
      block.updateAttachment(ThicketAttachment)
    }

    override def transform(tree: Tree): Tree = tree match {
      case Block(stats, expr) =>
        val transformedStats = transformTrees(stats)
        val transformedExpr = transform(expr)
        if ((stats eq transformedStats) && (expr eq transformedExpr)) tree
        else {
          val expanded = new mutable.ListBuffer[Tree]
          def expandStats(): Unit = transformedStats.foreach {
            case EmptyTree =>
            case blk @ Block(stats, expr) if blk.attachments.containsElement(ThicketAttachment) =>
              stats.foreach { s => if (s != EmptyTree) expanded += s }
              if (expr != EmptyTree) expanded += expr
            case t =>
              expanded += t
          }
          def expandExpr(): Tree = transformedExpr match {
            case blk @ Block(stats, expr) if blk.attachments.containsElement(ThicketAttachment) =>
              stats.foreach { s => if (s != EmptyTree) expanded += s }
              expr
            case t =>
              t
          }
          expandStats()
          val expr1 = expandExpr()
          treeCopy.Block(tree, expanded.toList, expr1)
        }
      case _ =>
        super.transform(tree)
    }
  }
}

