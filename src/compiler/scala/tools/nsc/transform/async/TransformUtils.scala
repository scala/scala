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

package scala.tools.nsc.transform.async

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Utilities used in both `ExprBuilder` and `AnfTransform`.
 */
private[async] trait TransformUtils extends AsyncTransformStates {
  import global._

  private[async] val asyncNames: AsyncNames[global.type]

  def typedPos(pos: Position)(tree: Tree): Tree = currentTransformState.localTyper.typedPos(pos)(tree: Tree)
  def typedCurrentPos(tree: Tree): Tree = typedPos(currentTransformState.currentPos)(tree)
  def typedBasePos(tree: Tree): Tree = typedPos(currentTransformState.applySym.pos)(tree)

  lazy val IllegalStateExceptionClass: Symbol = rootMirror.staticClass("java.lang.IllegalStateException")
  lazy val IllegalStateExceptionClass_NEW_String: Symbol = IllegalStateExceptionClass.info.decl(nme.CONSTRUCTOR).suchThat(
    x => x.paramss.head.size == 1 && x.firstParam.info.typeSymbol == definitions.StringClass)

  def isAwait(fun: Tree): Boolean = fun.symbol == currentTransformState.Async_await

  def isBooleanAnd(sym: Symbol): Boolean =
    sym.owner == definitions.BooleanClass && sym == definitions.Boolean_and
  def isBooleanOr(sym: Symbol): Boolean =
    sym.owner == definitions.BooleanClass && sym == definitions.Boolean_or

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
    case _ =>
      throw new MatchError(trees)
  }

  def assignUnitType(t: Tree): t.type = t.setType(definitions.UnitTpe)

  // Avoid moving a Try into expression position with a potentially non-empty stack.
  // Uncurry has already run and is responsible for faking try expressions! See needsTryLift.
  final def deriveTree(tree: Tree, exprType: Type)(deriveExpr: Tree => Tree): Tree = tree match {
    case Try(block, catches, finalizer) =>
      val block1 = deriveTree(block, exprType)(deriveExpr)
      val catches1 = catches.mapConserve(cd => deriveCaseDef(cd)(body => deriveTree(body, exprType)(deriveExpr)))
      treeCopy.Try(tree, block1, catches1, finalizer).setType(exprType)
    case If(cond, thenp, elsep) =>
      treeCopy.If(tree, cond, deriveTree(thenp, exprType)(deriveExpr), deriveTree(elsep, exprType)(deriveExpr)).setType(exprType)
    case Match(scrut, cases) =>
      treeCopy.Match(tree, scrut, cases.map(cd => deriveCaseDef(cd)(body => deriveTree(body, exprType)(deriveExpr)))).setType(exprType)
    case Block(stats, expr) =>
      treeCopy.Block(tree, stats, deriveTree(expr, exprType)(deriveExpr)).setType(exprType)
    case MatchEnd(ld) =>
      ld.symbol.modifyInfo {
        case MethodType(params, _) => MethodType(params, exprType)
        case x                     => throw new MatchError(x)
      }
      treeCopy.LabelDef(ld, ld.name, ld.params, deriveTree(ld.rhs, exprType)(deriveExpr)).setType(exprType)
    case _ =>
      deriveExpr(tree).setType(exprType)
  }

  def isUnitType(tp: Type): Boolean = tp.typeSymbol == definitions.UnitClass || tp =:= definitions.BoxedUnitTpe

  def literalUnit: Tree = Literal(Constant(())).setType(definitions.UnitTpe) // a def to avoid sharing trees
  def literalBoxedUnit: Tree = gen.mkAttributedRef(definitions.BoxedUnit_UNIT)
  def literalBool(b: Boolean): Tree = Literal(Constant(b)).setType(definitions.BooleanTpe)

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

  /** Descends into the regions of the tree that are subject to the
    * translation to a state machine by `async`. When a nested template,
    * function, or by-name argument is encountered, the descent stops,
    * and `nestedClass` etc are invoked.
    */
  trait AsyncTraverser extends Traverser {
    def nestedClass(classDef: ClassDef): Unit = {
    }

    def nestedModuleClass(moduleClass: ClassDef): Unit = {
    }

    def nestedMethod(defdef: DefDef): Unit = {
    }

    def synchronizedCall(arg: Tree): Unit = {
    }

    def function(function: Function): Unit = {
    }

    def function(expandedFunction: ClassDef): Unit = {
    }

    override def traverse(tree: Tree): Unit = {
      tree match {
        case cd: ClassDef                                                              =>
          if (cd.symbol.isAnonymousClass) function(cd)
          else if (cd.symbol.isModuleClass) nestedModuleClass(cd)
          else nestedClass(cd)
        case dd: DefDef                                                                => nestedMethod(dd)
        case fun: Function                                                             => function(fun)
        case Apply(TypeApply(fun, _), arg :: Nil) if fun.symbol == definitions.Object_synchronized => synchronizedCall(arg)
        case _                                                                         => super.traverse(tree)
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
    */
  final def containsAwait(t: Tree): Boolean = containsAwaitTraverser.apply(t)

  private object containsAwaitTraverser extends Traverser {
    var containsAwait = false
    def apply(t: Tree): Boolean = {
      containsAwait = false
      traverse(t)
      containsAwait
    }
    override def traverse(tree: Tree): Unit =
      if (tree.attachments.containsElement(NoAwait)) {} // safe to skip
      else if (!containsAwait) {
        if (tree.attachments.containsElement(ContainsAwait)) containsAwait = true
        else if (markContainsAwaitTraverser.shouldAttach(tree)) super.traverse(tree)
      }
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

    val stack = mutable.Stack[Tree]()

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

  object cleanupContainsAwaitAttachments extends Traverser {
    def apply(tree: Tree): tree.type = {traverse(tree); tree}
    override def traverse(tree: Tree): Unit = tree match {
      case _: CannotHaveAttrs =>
      case t =>
        if (t.attachments.containsElement(NoAwait)) {
          t.setAttachments(t.attachments.removeElement(NoAwait))
        } else {
          t.setAttachments(t.attachments.removeElement(ContainsAwait))
          super.traverse(t)
        }
    }
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
