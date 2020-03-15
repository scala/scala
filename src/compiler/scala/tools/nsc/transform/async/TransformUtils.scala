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

package scala.tools.nsc.transform.async

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.internal.Mode
import scala.tools.nsc.transform.TypingTransformers

/**
 * Utilities used in both `ExprBuilder` and `AnfTransform`.
 */
private[async] trait TransformUtils extends TypingTransformers {
  import global._

  def currentTransformState: AsyncTransformState[global.type]
  val asyncNames: AsyncNames[global.type]
  object name extends asyncNames.AsyncName {
    def fresh(name: TermName): TermName = freshenIfNeeded(name)
    def fresh(name: String): String = currentFreshNameCreator.newName(name) // TODO ok? was c.freshName
  }

  def typedPos(pos: Position)(tree: Tree): Tree = currentTransformState.localTyper.typedPos(pos)(tree: Tree)
  def typedPos(pos: Position, mode: Mode, pt: Type)(tree: Tree): Tree = currentTransformState.localTyper.typedPos(pos, mode, pt)(tree)
  def typed(tree: Tree): Tree = typedPos(currentTransformState.applySym.pos)(tree)

  def maybeTry(emitTryCatch: Boolean)(block: Tree, catches: List[CaseDef], finalizer: Tree): Tree =
    if (emitTryCatch) Try(block, catches, finalizer) else block

  lazy val IllegalStateExceptionClass: Symbol = rootMirror.staticClass("java.lang.IllegalStateException")

  def isAsync(fun: Tree): Boolean = fun.symbol == currentTransformState.ops.Async_async
  def isAwait(fun: Tree): Boolean = fun.symbol == currentTransformState.ops.Async_await

  def isBooleanShortCircuit(sym: Symbol): Boolean =
    sym.owner == definitions.BooleanClass && (sym == definitions.Boolean_and || sym == definitions.Boolean_or)

  def isLabel(sym: Symbol): Boolean = sym != null && sym.isLabel
  def isCaseLabel(sym: Symbol): Boolean = sym != null && sym.isLabel && sym.name.startsWith("case")
  def isMatchEndLabel(sym: Symbol): Boolean = sym != null && sym.isLabel && sym.name.startsWith("matchEnd")

  def substituteTrees(t: Tree, from: List[Symbol], to: List[Tree]): Tree =
    (new TreeSubstituter(from, to)).transform(t)

  def statsAndExpr(tree: Tree): (List[Tree], Tree) = tree match {
    case Block(stats, expr) => (stats, expr)
    case _                  =>
      if (tree.tpe <:< definitions.UnitTpe) (Nil, tree)
      else (List(tree), literalUnit)
  }

  def listToBlock(trees: List[Tree]): Block = trees match {
    case trees @ (init :+ last) =>
      val pos = trees.map(_.pos).reduceLeft(_ union _)
      Block(init, last).setType(last.tpe).setPos(pos)
    case Nil =>
      throw new MatchError(trees)
  }

  def assignUnitType(t: Tree): t.type = t.setType(definitions.UnitTpe)

  def isUnitType(tp: Type): Boolean = tp.typeSymbol == definitions.UnitClass || tp =:= definitions.BoxedUnitTpe

  def literalUnit: Tree = Literal(Constant(())).setType(definitions.UnitTpe) // a def to avoid sharing trees
  def literalBoxedUnit: Tree = gen.mkAttributedRef(definitions.BoxedUnit_UNIT)

  def isLiteralUnit(t: Tree): Boolean = t match {
    case Literal(Constant(())) => true
    case t if t.symbol == definitions.BoxedUnit_UNIT => true // important to find match labels (which are potential states)
    case _ => false
  }

  def adaptToUnit(rhs: List[Tree]): Block = {
    def filterUnit(ts: List[Tree]): List[Tree] = ts.filterNot(isLiteralUnit(_))
    rhs match {
      case (rhs: Block) :: Nil if rhs.tpe <:< definitions.UnitTpe || rhs.tpe <:< definitions.BoxedUnitTpe =>
        rhs
      case init :+ last if last.tpe <:< definitions.UnitTpe || last.tpe <:< definitions.BoxedUnitTpe=>
        Block(filterUnit(init), last).setType(definitions.UnitTpe)
      case _                                                          =>
        Block(filterUnit(rhs), literalUnit).setType(definitions.UnitTpe)
    }
  }

  private object ThicketAttachment
  class ThicketTransformer(initLocalTyper: analyzer.Typer) extends TypingTransformer(initLocalTyper) {
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
        def expandStats(expanded: mutable.ListBuffer[Tree]) = transformedStats.foreach {
          case blk @ Block(stats, expr) if blk.attachments.containsElement(ThicketAttachment) =>
            expanded ++= stats
            expanded += expr
          case t =>
            expanded += t
        }
        def expandExpr(expanded: mutable.ListBuffer[Tree]): Tree = transformedExpr match {
          case blk @ Block(stats, expr) if blk.attachments.containsElement(ThicketAttachment) =>
            expanded ++= stats
            expr
          case t =>
            t
        }

        if (stats eq transformedStats) {
          if (expr eq transformedExpr) tree
          else {
            val expanded = new mutable.ListBuffer[Tree]
            expanded ++= transformedStats
            val expr1 = expandExpr(expanded)
            if (expanded.isEmpty)
              treeCopy.Block(tree, transformedStats, expr1)
            else
              treeCopy.Block(tree, expanded.toList, expr1)
          }
        } else {
          val expanded = new mutable.ListBuffer[Tree]
          if (expr eq transformedExpr) {
            expandStats(expanded)
            treeCopy.Block(tree, expanded.toList, expr)
          } else {
            expandStats(expanded)
            val expr1 = expandExpr(expanded)
            treeCopy.Block(tree, expanded.toList, expr1)
          }
        }
      case _ =>
        super.transform(tree)
    }
  }

  /** Descends into the regions of the tree that are subject to the
    * translation to a state machine by `async`. When a nested template,
    * function, or by-name argument is encountered, the descent stops,
    * and `nestedClass` etc are invoked.
    */
  trait AsyncTraverser extends Traverser {
    def nestedClass(classDef: ClassDef): Unit = {
    }

    def nestedModule(module: ModuleDef): Unit = {
    }

    def nestedMethod(defdef: DefDef): Unit = {
    }

    def byNameArgument(arg: Tree): Unit = {
    }

    def function(function: Function): Unit = {
    }

    def patMatFunction(tree: Match): Unit = {
    }

    override def traverse(tree: Tree): Unit = {
      tree match {
        case cd: ClassDef          => nestedClass(cd)
        case md: ModuleDef         => nestedModule(md)
        case dd: DefDef            => nestedMethod(dd)
        case fun: Function         => function(fun)
        case m@Match(EmptyTree, _) => patMatFunction(m) // Pattern matching anonymous function under -Xoldpatmat of after `restorePatternMatchingFunctions`
        case Apply(fun, arg1 :: arg2 :: Nil) if isBooleanShortCircuit(fun.symbol) =>
          traverse(fun)
          traverse(arg1)
          byNameArgument(arg2)
        case _                     => super.traverse(tree)
      }
    }
  }

  def toMultiMap[A, B](abs: Iterable[(A, B)]): mutable.LinkedHashMap[A, List[B]] = {
    // LinkedHashMap for stable order of results.
    val result = new mutable.LinkedHashMap[A, ListBuffer[B]]()
    for ((a, b) <- abs) {
      val buffer = result.getOrElseUpdate(a, new ListBuffer[B])
      buffer += b
    }
    result.map { case (a, b) => (a, b.toList) }
  }

  /**
    * Efficiently decorate each subtree within `t` with the result of `t exists isAwait`,
    * and return a function that can be used on derived trees to efficiently test the
    * same condition.
    *
    * If the derived tree contains synthetic wrapper trees, these will be recursed into
    * in search of a sub tree that was decorated with the cached answer.
    *
    * Requires markContainsAwaitTraverser has previously traversed `t`.
    **/
  final def containsAwait(t: Tree): Boolean = {
    object traverser extends Traverser {
      var containsAwait = false
      override def traverse(tree: Tree): Unit =
        if (tree.attachments.containsElement(NoAwait)) {} // safe to skip
        else if (!containsAwait) {
          if (tree.attachments.containsElement(ContainsAwait)) containsAwait = true
          else if (markContainsAwaitTraverser.shouldAttach(t)) super.traverse(tree)
        }
    }
    traverser.traverse(t)
    traverser.containsAwait
  }

  def markContainsAwait(t: Tree) = markContainsAwaitTraverser.traverse(t)

  private object markContainsAwaitTraverser extends Traverser {
    def shouldAttach(t: Tree) = !treeCannotContainAwait(t)
    private def treeCannotContainAwait(t: Tree) = t match {
      case _: CannotHaveAttrs => true
      case _: Ident | _: TypeTree | _: Literal => true
      case _ => false
    }
    private def attachContainsAwait(t: Tree): Unit = if (shouldAttach(t)) {
      t.setAttachments(t.attachments.addElement(ContainsAwait))
      t.setAttachments(t.attachments.removeElement(NoAwait))
    }
    private def attachNoAwait(t: Tree): Unit = if (shouldAttach(t)) {
      t.setAttachments(t.attachments.addElement(NoAwait))
    }

    var stack = mutable.ArrayStack[Tree]()

    override def traverse(tree: Tree): Unit = {
      stack.push(tree)
      try {
        if (isAwait(tree))
          stack.foreach(attachContainsAwait)
        else
          attachNoAwait(tree)
        super.traverse(tree)
      } finally stack.pop()
    }
  }

  final def cleanupContainsAwaitAttachments(t: Tree): t.type = {
    t.foreach {
      case _: CannotHaveAttrs =>
      case t =>
        t.setAttachments(t.attachments.removeElement(ContainsAwait))
        t.setAttachments(t.attachments.removeElement(NoAwait))
    }
    t
  }

  val isMatchEnd: (Tree) => Boolean = t =>
    MatchEnd.unapply(t).isDefined
  object MatchEnd {
    def unapply(t: Tree): Option[LabelDef] = t match {
      case ValDef(_, _, _, t) => unapply(t)
      case ld: LabelDef if isMatchEndLabel(ld.symbol) => Some(ld)
      case _ => None
    }
  }

  final def flattenBlock(tree: Tree)(f: Tree => Unit): Unit = tree match {
    case Block(stats, expr) => stats.foreach(f); f(expr)
    case _ => f(tree)
  }

  // unwrap Block(t :: Nil, scala.runtime.BoxedUnit.UNIT) -- erasure will add the expr when await had type Unit
  object UnwrapBoxedUnit {
    def unapply(tree: Tree): Some[Tree] = tree match {
      case Block(t :: Nil, unit) if isLiteralUnit(unit) => Some(t) // is really only going to be BoxedUnit, but hey
      case t => Some(t)
    }
  }
}

private case object ContainsAwait
private case object NoAwait
case object StateTransitionTree
