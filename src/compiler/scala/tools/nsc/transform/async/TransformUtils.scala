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
import scala.tools.nsc.NoPhase
import scala.language.existentials
import scala.reflect.internal.util.ListOfNil
import scala.tools.nsc.transform.TypingTransformers

// Logic sensitive to where we are in the pipeline
// (intend to move the transformation as late as possible, to avoid lugging all these trees around)
trait PhasedTransform extends TypingTransformers {
  import global._

  def isPastErasure: Boolean = {
    val erasurePhase = global.currentRun.erasurePhase
    erasurePhase != NoPhase && global.isPast(erasurePhase)
  }

  // We're not that granular, but keeping separate flag for semantics
  private def isPastUncurry = isPastErasure
  private def emptyParamss: List[List[ValDef]] = if (isPastUncurry) ListOfNil else Nil
  protected def applyNilAfterUncurry(t: Tree) = if (isPastUncurry) Apply(t, Nil) else t

  def literalNull = Literal(Constant(null))

  def typeEqualsUnit(tp: Type) = tp =:= definitions.UnitTpe || (isPastErasure && tp =:= definitions.BoxedUnitTpe)

  def assignUnitType(t: Tree): t.type =
    t.setType(definitions.UnitTpe)

  def setUnitMethodInfo(sym: Symbol): sym.type = sym.setInfo(MethodType(Nil, if (isPastErasure) definitions.BoxedUnitTpe else definitions.UnitTpe))

  def isUnitType(tp: Type) = tp.typeSymbol == definitions.UnitClass || (isPastErasure && tp =:= definitions.BoxedUnitTpe)
  def isNothingClass(sym: Symbol) = sym == definitions.NothingClass

  def literalUnit =
    Literal(Constant(())).setType(definitions.UnitTpe) // a def to avoid sharing trees
  def literalBoxedUnit =
    gen.mkAttributedRef(definitions.BoxedUnit_UNIT)

  def isLiteralUnit(t: Tree) = t match {
    case Literal(Constant(())) => true
    case t if t.symbol == definitions.BoxedUnit_UNIT => true // important to find match labels (which are potential states)
    case _ => false
  }


  def transformType(tp: Type) = if (isPastErasure) transformedType(tp) else tp

  def mkAsInstanceOf(qual: Tree, tp: Type) = gen.mkCast(qual, tp)

  private def tpeOf(t: Tree): Type = t match {
    case _ if t.tpe != null    => t.tpe
    case Try(body, Nil, _)     => tpeOf(body)
    case Block(_, expr)        => tpeOf(expr)
    case Literal(Constant(())) => definitions.UnitTpe
    case Return(_)             => definitions.NothingTpe
    case _                     => NoType
  }

  def adaptToUnit(rhs: List[Tree]): Block =
    rhs match {
      case (rhs: Block) :: Nil if { val tp = tpeOf(rhs); tp <:< definitions.UnitTpe || tp <:< definitions.BoxedUnitTpe } =>
        rhs
      case init :+ last if { val tp = tpeOf(last); tp <:< definitions.UnitTpe || tp <:< definitions.BoxedUnitTpe }        =>
        Block(init, last)
      case init :+ (last@Literal(Constant(())))                       =>
        Block(init, last)
      case init :+ (last@Block(_, Return(_) | Literal(Constant(())))) =>
        Block(init, last)
      case init :+ (last@Block(_, expr)) if expr.symbol == definitions.BoxedUnit_UNIT =>
        Block(init, last)
      case init :+ Block(stats, expr)                                 =>
        Block(init, Block(stats :+ expr, literalUnit))
      case _                                                          =>
        Block(rhs, literalUnit)
    }

  // TODO AM: why add the :Any type ascription to hide a tree of type Nothing? adaptToUnit doesn't seem to care
  def adaptToUnitIgnoringNothing(stats: List[Tree]): Block =
    stats match {
      case init :+ last if tpeOf(last) =:= definitions.NothingTpe =>
        adaptToUnit(init :+ Typed(last, TypeTree(definitions.AnyTpe)))
      case _                                                      =>
        adaptToUnit(stats)
    }

  private def derivedValueClassUnbox(cls: Symbol) =
    (cls.info.decls.find(sym => sym.isMethod && sym.asTerm.isParamAccessor) getOrElse NoSymbol)
}


/**
 * Utilities used in both `ExprBuilder` and `AnfTransform`.
 */
private[async] trait TransformUtils extends PhasedTransform {
  import global._

  def currentTransformState: AsyncTransformState[global.type]
  val asyncNames: AsyncNames[global.type]
  object name extends asyncNames.AsyncName {
    def fresh(name: TermName): TermName = freshenIfNeeded(name)
    def fresh(name: String): String = currentFreshNameCreator.newName(name) // TODO ok? was c.freshName
  }

  def maybeTry(emitTryCatch: Boolean)(block: Tree, catches: List[CaseDef], finalizer: Tree) =
    if (emitTryCatch) Try(block, catches, finalizer) else block

  lazy val IllegalStateExceptionClass = rootMirror.staticClass("java.lang.IllegalStateException")

  def isAsync(fun: Tree) = fun.symbol == currentTransformState.ops.Async_async
  def isAwait(fun: Tree) = fun.symbol == currentTransformState.ops.Async_await

  def isBooleanShortCircuit(sym: Symbol): Boolean =
    sym.owner == definitions.BooleanClass && (sym == definitions.Boolean_and || sym == definitions.Boolean_or)

  def isLabel(sym: Symbol): Boolean = sym.isLabel

  def substituteTrees(t: Tree, from: List[Symbol], to: List[Tree]): Tree =
    (new TreeSubstituter(from, to)).transform(t)

  def statsAndExpr(tree: Tree): (List[Tree], Tree) = tree match {
    case Block(stats, expr) => (stats, expr)
    case _                  => (List(tree), Literal(Constant(())))
  }

  def blockToList(tree: Tree): List[Tree] = tree match {
    case Block(stats, expr) => stats :+ expr
    case t                  => t :: Nil
  }

  def listToBlock(trees: List[Tree]): Block = trees match {
    case trees @ (init :+ last) =>
      val pos = trees.map(_.pos).reduceLeft(_ union _)
      Block(init, last).setType(last.tpe).setPos(pos)
    case Nil =>
      throw new MatchError(trees)
  }

  def emptyConstructor: DefDef = {
    val emptySuperCall = Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), Nil)
    DefDef(NoMods, nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(emptySuperCall), Literal(Constant(()))))
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

  def thisType(sym: Symbol): Type = {
    if (sym.isClass) sym.asClass.thisPrefix
    else NoPrefix
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
        if (tree.hasAttachment[NoAwait.type]) {} // safe to skip
        else if (!containsAwait) {
          if (tree.hasAttachment[ContainsAwait.type]) containsAwait = true
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

    var stack: List[Tree] = Nil

    override def traverse(tree: Tree): Unit = {
      stack ::= tree
      try {
        if (isAwait(tree))
          stack.foreach(attachContainsAwait)
        else
          attachNoAwait(tree)
        super.traverse(tree)
      } finally stack = stack.tail
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
      case ld: LabelDef if ld.name.toString.startsWith("matchEnd") => Some(ld)
      case _ => None
    }
  }

  final def flattenBlock(tree: Tree)(f: Tree => Unit): Unit = tree match {
    case Block(stats, expr) => stats.foreach(f); f(expr)
    case _ => f(tree)
  }
}

private case object ContainsAwait
private case object NoAwait
