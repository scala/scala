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
import scala.reflect.internal.{Flags, SymbolTable}
import user.AsyncBase
import scala.tools.nsc.{Global, NoPhase}
import scala.language.existentials

private[async] trait AsyncContext {
  val asyncBase: AsyncBase
  val u: Global
}

// Logic sensitive to where we are in the pipeline
// (intend to move the transformation as late as possible, to avoid lugging all these trees around)
trait PhasedTransform extends AsyncContext {
  import u._

  // macro context interface -- the rest is meant to be independent of our being a macro (planning to move async into the compiler)
  def abort(pos: Position, msg: String): Nothing
  def error(pos: Position, msg: String): Unit
  def typecheck(tree: Tree): Tree

  // We're not that granular, but keeping separate flag for semantics
  private lazy val isPastUncurry = isPastErasure
  private lazy val emptyParamss: List[Nil.type] = if (isPastUncurry) List(Nil) else Nil
  protected def applyNilAfterUncurry(t: Tree) = if (isPastUncurry) Apply(t, Nil) else t

  lazy val isPastErasure = {
    val erasurePhase = u.currentRun.erasurePhase
    erasurePhase != NoPhase && u.isPast(erasurePhase)
  }

  def literalNull = Literal(Constant(null))

  def typeEqualsNothing(tp: Type) = tp =:= definitions.NothingTpe

  def typeEqualsUnit(tp: Type) = tp =:= definitions.UnitTpe || (isPastErasure && tp =:= definitions.BoxedUnitTpe)

  def assignUnitType(t: Tree): t.type =
    t.setType(definitions.UnitTpe)

  def setUnitMethodInfo(sym: Symbol): sym.type = sym.setInfo(MethodType(Nil, if (isPastErasure) definitions.BoxedUnitTpe else definitions.UnitTpe))

  def isUnitType(tp: Type) = tp.typeSymbol == definitions.UnitClass || (isPastErasure && tp =:= definitions.BoxedUnitTpe)
  def isNothingClass(sym: Symbol) = sym == definitions.NothingClass

  def literalUnit =
    if (isPastErasure) gen.mkAttributedRef(definitions.BoxedUnit_UNIT)
    else Literal(Constant(())) // a def to avoid sharing trees

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

  def mkZero(tp: Type, pos: Position): Tree = {
    val tpSym = tp.typeSymbol
    if (tpSym.isClass && tpSym.asClass.isDerivedValueClass) {
      val argZero = mkZero(derivedValueClassUnbox(tpSym).infoIn(tp).resultType, pos)
      val baseType = tp.baseType(tpSym) // use base type here to dealias / strip phantom "tagged types" etc.

      // By explicitly attributing the types and symbols here, we subvert privacy.
      // Otherwise, ticket86PrivateValueClass would fail.

      // Approximately:
      // q"new ${valueClass}[$..targs](argZero)"
      val target: Tree = gen.mkAttributedSelect(typecheck(atPos(pos)(New(TypeTree(baseType)))), tpSym.asClass.primaryConstructor)

      val zero = gen.mkMethodCall(target, argZero :: Nil)
      // restore the original type which we might otherwise have weakened with `baseType` above
      typecheck(atPos(pos)(gen.mkCast(zero, tp)))
    } else {
      gen.mkZero(tp)
    }
  }

  final def uncheckedBounds(tp: Type): Type = if (isPastErasure) tp else u.uncheckedBounds(tp)

  def uncheckedBoundsIfNeeded(t: Type): Type = {
    var quantified: List[Symbol] = Nil
    var badSkolemRefs: List[Symbol] = Nil
    t.foreach {
      case et: ExistentialType =>
        quantified :::= et.quantified
      case TypeRef(pre, sym, args) =>
        val illScopedSkolems = args.map(_.typeSymbol).filter(arg => arg.isExistentialSkolem && !quantified.contains(arg))
        badSkolemRefs :::= illScopedSkolems
      case _ =>
    }
    if (badSkolemRefs.isEmpty) t
    else t.map {
      case tp @ TypeRef(pre, sym, args) if args.exists(a => badSkolemRefs.contains(a.typeSymbol)) =>
        uncheckedBounds(tp)
      case t => t
    }
  }


  final def mkMutableField(tpt: Type, name: TermName, init: Tree): List[Tree] = {
    if (isPastTyper) {
      import scala.reflect.internal.Flags._
      // If we are running after the typer phase (ie being called from a compiler plugin)
      // we have to create the trio of members manually.
      val field = ValDef(Modifiers(MUTABLE | PRIVATE | LOCAL), name.localName, TypeTree(tpt), init)
      val paramss = emptyParamss
      val getter = DefDef(Modifiers(ACCESSOR | STABLE), name.getterName, Nil, paramss, TypeTree(tpt), Select(This(tpnme.EMPTY), field.name))
      val setter = DefDef(Modifiers(ACCESSOR), name.setterName, Nil, List(List(ValDef(NoMods, TermName("x"), TypeTree(tpt), EmptyTree))), TypeTree(definitions.UnitTpe), Assign(Select(This(tpnme.EMPTY), field.name), Ident(TermName("x"))))
      field :: getter :: setter :: Nil
    } else {
      val result = ValDef(NoMods, name, TypeTree(tpt), init)
      result :: Nil
    }
  }
  final def mkField(tpt: Type, name: TermName, init: Tree): List[Tree] = {
    if (isPastTyper) {
      import scala.reflect.internal.Flags._
      // If we are running after the typer phase (ie being called from a compiler plugin)
      // we have to create the trio of members manually.
      val field = ValDef(Modifiers(PRIVATE | LOCAL), name.localName, TypeTree(tpt), init)
      val paramss = emptyParamss
      val getter = DefDef(Modifiers(ACCESSOR | STABLE), name.getterName, Nil, paramss, TypeTree(tpt), Select(This(tpnme.EMPTY), field.name))
      field :: getter :: Nil
    } else {
      val result = ValDef(NoMods, name, TypeTree(tpt), init)
      result :: Nil
    }
  }

}


/**
 * Utilities used in both `ExprBuilder` and `AnfTransform`.
 */
private[async] trait TransformUtils extends PhasedTransform {
  import typingTransformers.{TypingTransformApi, typingTransform}
  import u._

  val asyncNames: AsyncNames[u.type]
  object name extends asyncNames.AsyncName {
    def fresh(name: TermName): TermName = freshenIfNeeded(name)
    def fresh(name: String): String = currentFreshNameCreator.newName(name) // TODO ok? was c.freshName
  }

  def emitTryCatch: Boolean = asyncBase.futureSystem.emitTryCatch

  def maybeTry(block: Tree, catches: List[CaseDef], finalizer: Tree) =
    if (emitTryCatch) Try(block, catches, finalizer) else block

  lazy val IllegalStateExceptionClass = rootMirror.staticClass("java.lang.IllegalStateException")

  val Async_async: Symbol
  val Async_await: Symbol

  def isAsync(fun: Tree) = fun.symbol == Async_async
  def isAwait(fun: Tree) = fun.symbol == Async_await


  private lazy val Boolean_ShortCircuits: Set[Symbol] = {
    import definitions.BooleanClass
    def BooleanTermMember(name: String) = BooleanClass.typeSignature.member(TermName(name).encodedName)
    val Boolean_&& = BooleanTermMember("&&")
    val Boolean_|| = BooleanTermMember("||")
    Set(Boolean_&&, Boolean_||)
  }

  private def isByName(fun: Tree): ((Int, Int) => Boolean) = {
    if (Boolean_ShortCircuits contains fun.symbol) (i, j) => true
    else if (fun.tpe == null) (x, y) => false
    else {
      val paramss = fun.tpe.paramss
      val byNamess = paramss.map(_.map(_.asTerm.isByNameParam))
      (i, j) => util.Try(byNamess(i)(j)).getOrElse(false)
    }
  }
  private def argName(fun: Tree): ((Int, Int) => TermName) = {
    val paramss = fun.tpe.paramss
    val namess = paramss.map(_.map(_.name.toTermName))
    (i, j) => util.Try(namess(i)(j)).getOrElse(TermName(s"arg_${i}_${j}"))
  }

  def isLabel(sym: Symbol): Boolean = sym.isLabel

  def substituteTrees(t: Tree, from: List[Symbol], to: List[Tree]): Tree =
    (new TreeSubstituter(from, to)).transform(t)

  /** Map a list of arguments to:
    * - A list of argument Trees
    * - A list of auxillary results.
    *
    * The function unwraps and rewraps the `arg :_*` construct.
    *
    * @param args The original argument trees
    * @param f  A function from argument (with '_*' unwrapped) and argument index to argument.
    * @tparam A The type of the auxillary result
    */
  private def mapArguments[A](args: List[Tree])(f: (Tree, Int) => (A, Tree)): (List[A], List[Tree]) = {
    args match {
      case args :+ Typed(tree, Ident(tpnme.WILDCARD_STAR)) =>
        val (a, argExprs :+ lastArgExpr) = (args :+ tree).zipWithIndex.map(f.tupled).unzip
        val exprs = argExprs :+ atPos(lastArgExpr.pos.makeTransparent)(Typed(lastArgExpr, Ident(tpnme.WILDCARD_STAR)))
        (a, exprs)
      case args                                            =>
        args.zipWithIndex.map(f.tupled).unzip
    }
  }

  case class Arg(expr: Tree, isByName: Boolean, argName: TermName)

  /**
   * Transform a list of argument lists, producing the transformed lists, and lists of auxillary
   * results.
   *
   * The function `f` need not concern itself with varargs arguments e.g (`xs : _*`). It will
   * receive `xs`, and it's result will be re-wrapped as `f(xs) : _*`.
   *
   * @param fun   The function being applied
   * @param argss The argument lists
   * @return      (auxillary results, mapped argument trees)
   */
  def mapArgumentss[A](fun: Tree, argss: List[List[Tree]])(f: Arg => (A, Tree)): (List[List[A]], List[List[Tree]]) = {
    val isByNamess: (Int, Int) => Boolean = isByName(fun)
    val argNamess: (Int, Int) => TermName = argName(fun)
    argss.zipWithIndex.map { case (args, i) =>
      mapArguments[A](args) {
        (tree, j) => f(Arg(tree, isByNamess(i, j), argNamess(i, j)))
      }
    }.unzip
  }


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
        case _ if isAsync(tree) =>
          // Under -Ymacro-expand:discard, used in the IDE, nested async blocks will be visible to the outer blocks
        case cd: ClassDef          => nestedClass(cd)
        case md: ModuleDef         => nestedModule(md)
        case dd: DefDef            => nestedMethod(dd)
        case fun: Function         => function(fun)
        case m@Match(EmptyTree, _) => patMatFunction(m) // Pattern matching anonymous function under -Xoldpatmat of after `restorePatternMatchingFunctions`
        case q"$fun[..$targs](...$argss)" if argss.nonEmpty =>
          val isInByName = isByName(fun)
          for ((args, i) <- argss.zipWithIndex) {
            for ((arg, j) <- args.zipWithIndex) {
              if (!isInByName(i, j)) traverse(arg)
              else byNameArgument(arg)
            }
          }
          traverse(fun)
        case _                     => super.traverse(tree)
      }
    }
  }

  def transformAt(tree: Tree)(f: PartialFunction[Tree, (TypingTransformApi => Tree)]) = {
    typingTransform(tree)((tree, api) => {
      if (f.isDefinedAt(tree)) f(tree)(api)
      else api.default(tree)
    })
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

  // Attributed version of `TreeGen#mkCastPreservingAnnotations`
  def mkAttributedCastPreservingAnnotations(tree: Tree, tp: Type): Tree = {
    atPos(tree.pos) {
      val casted = typecheck(gen.mkCast(tree, uncheckedBounds(tp.withoutAnnotations).dealias))
      Typed(casted, TypeTree(tp)).setType(tp)
    }
  }

  def withAnnotation(tp: Type, ann: Annotation): Type = withAnnotations(tp, List(ann))

  def withAnnotations(tp: Type, anns: List[Annotation]): Type = tp match {
    case AnnotatedType(existingAnns, underlying) => annotatedType(anns ::: existingAnns, underlying)
    case ExistentialType(quants, underlying) => existentialAbstraction(quants, withAnnotations(underlying, anns))
    case _ => annotatedType(anns, tp)
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
        else if (tree.hasAttachment[ContainsAwait.type]) containsAwait = true
        else if (markContainsAwaitTraverser.shouldAttach(t)) super.traverse(tree)
    }
    traverser.traverse(t)
    traverser.containsAwait
  }

  def markContainsAwait(t: Tree) = markContainsAwaitTraverser.traverse(t)

  private object markContainsAwaitTraverser extends Traverser {
    def shouldAttach(t: Tree) = !treeCannotContainAwait(t)
    private def treeCannotContainAwait(t: Tree) = t match {
      case _: Ident | _: TypeTree | _: Literal => true
      case _ => isAsync(t)
    }
    private def attachContainsAwait(t: Tree): Unit = if (shouldAttach(t)) {
      t.updateAttachment(ContainsAwait)
      t.removeAttachment[NoAwait.type]
    }
    private def attachNoAwait(t: Tree): Unit = if (shouldAttach(t)) {
      t.updateAttachment(NoAwait)
    }

    var stack: List[Tree] = Nil

    override def traverse(tree: Tree): Unit = {
      stack ::= tree
      try {
        if (isAsync(tree)) {
          ;
        } else {
          if (isAwait(tree))
            stack.foreach(attachContainsAwait)
          else
            attachNoAwait(tree)
          super.traverse(tree)
        }
      } finally stack = stack.tail
    }
  }

  final def cleanupContainsAwaitAttachments(t: Tree): t.type = {
    t.foreach {t =>
      t.removeAttachment[ContainsAwait.type]
      t.removeAttachment[NoAwait.type]
    }
    t
  }

  // First modification to translated patterns:
  //  - Set the type of label jumps to `Unit`
  //  - Propagate this change to trees known to directly enclose them:
  //    ``If` / `Block`) adjust types of enclosing
  final def adjustTypeOfTranslatedPatternMatches(t: Tree, owner: Symbol): Tree = {
    import definitions.UnitTpe

    typingTransform(t, owner) {
      (tree, api) =>
        tree match {
          case LabelDef(name, params, rhs) =>
            val rhs1 = api.recur(rhs)
            if (rhs1.tpe =:= UnitTpe) {
              tree.symbol.info = internal.methodType(tree.symbol.info.paramLists.head, UnitTpe)
              treeCopy.LabelDef(tree, name, params, rhs1)
            } else {
              treeCopy.LabelDef(tree, name, params, rhs1)
            }
          case Block(stats, expr) =>
            val stats1 = stats map api.recur
            val expr1 = api.recur(expr)
            if (expr1.tpe =:= UnitTpe)
              treeCopy.Block(tree, stats1, expr1).setType(UnitTpe)
            else
              treeCopy.Block(tree, stats1, expr1)
          case If(cond, thenp, elsep) =>
            val cond1 = api.recur(cond)
            val thenp1 = api.recur(thenp)
            val elsep1 = api.recur(elsep)
            if (thenp1.tpe =:= definitions.UnitTpe && elsep.tpe =:= UnitTpe)
              treeCopy.If(tree, cond1, thenp1, elsep1).setType(UnitTpe)
            else
              treeCopy.If(tree, cond1, thenp1, elsep1)
          case Apply(fun, args) if isLabel(fun.symbol) =>
            treeCopy.Apply(tree, api.recur(fun), args map api.recur).setType(UnitTpe)
          case vd @ ValDef(mods, name, tpt, rhs) if isCaseTempVal(vd.symbol) =>
            def addUncheckedBounds(t: Tree) = {
              typingTransform(t, owner) {
                (tree, api) =>
                  if (tree.tpe == null) tree else api.default(tree).setType(uncheckedBoundsIfNeeded(tree.tpe))
              }

            }
            val uncheckedRhs = addUncheckedBounds(api.recur(rhs))
            val uncheckedTpt = addUncheckedBounds(tpt)
            vd.symbol.info = uncheckedBoundsIfNeeded(vd.symbol.info)
            treeCopy.ValDef(vd, mods, name, uncheckedTpt, uncheckedRhs)
          case t => api.default(t)
        }
    }
  }

  private def isCaseTempVal(s: Symbol) = {
    s.isTerm && s.asTerm.isVal && s.isSynthetic && s.name.toString.startsWith("x")
  }


  def deriveLabelDef(ld: LabelDef, applyToRhs: Tree => Tree): LabelDef = {
    val rhs2 = applyToRhs(ld.rhs)
    val ld2 = treeCopy.LabelDef(ld, ld.name, ld.params, rhs2)
    if (ld eq ld2) ld
    else {
      val info2 = ld2.symbol.info match {
        case MethodType(params, p) => MethodType(params, rhs2.tpe)
        case t => t
      }
      ld2.symbol.info = info2
      ld2
    }
  }
  object MatchEnd {
    def unapply(t: Tree): Option[LabelDef] = t match {
      case ValDef(_, _, _, t) => unapply(t)
      case ld: LabelDef if ld.name.toString.startsWith("matchEnd") => Some(ld)
      case _ => None
    }
  }

  // TODO get rid of all of this and use compiler's facilities directly
  val typingTransformers: TypingTransformers { val global: u.type }

  abstract class TypingTransformers extends scala.tools.nsc.transform.TypingTransformers {
    val global: u.type = u
    def callsiteTyper: analyzer.Typer

    trait TransformApi {
      /** Calls the current transformer on the given tree.
        *  Current transformer = argument to the `transform` call.
        */
      def recur(tree: Tree): Tree

      /** Calls the default transformer on the given tree.
        *  Default transformer = recur into tree's children and assemble the results.
        */
      def default(tree: Tree): Tree
    }

    /** Functions that are available during [[typingTransform]].
      *  @see [[typingTransform]]
      */
    trait TypingTransformApi extends TransformApi {
      /** Temporarily pushes the given symbol onto the owner stack, creating a new local typer,
        *  invoke the given operation and then rollback the changes to the owner stack.
        */
      def atOwner[T](owner: Symbol)(op: => T): T

      /** Temporarily pushes the given tree onto the recursion stack, and then calls `atOwner(symbol)(trans)`.
        */
      def atOwner[T](tree: Tree, owner: Symbol)(op: => T): T

      /** Returns the symbol currently on the top of the owner stack.
        *  If we're not inside any `atOwner` call, then macro application's context owner will be used.
        */
      def currentOwner: Symbol

      /** Typechecks the given tree using the local typer currently on the top of the owner stack.
        *  If we're not inside any `atOwner` call, then macro application's callsite typer will be used.
        */
      def typecheck(tree: Tree): Tree
    }

    class HofTransformer(hof: (Tree, TransformApi) => Tree) extends Transformer {
      val api = new TransformApi {
        def recur(tree: Tree): Tree = hof(tree, this)
        def default(tree: Tree): Tree = superTransform(tree)
      }
      def superTransform(tree: Tree) = super.transform(tree)
      override def transform(tree: Tree): Tree = hof(tree, api)
    }

    //    def transform(tree: Tree)(transformer: (Tree, TransformApi) => Tree): Tree = new HofTransformer(transformer).transform(tree)

    class HofTypingTransformer(hof: (Tree, TypingTransformApi) => Tree) extends TypingTransformer(callsiteTyper.context.unit) { self =>
      currentOwner = callsiteTyper.context.owner
      curTree = EmptyTree

      localTyper = {
        val ctx: analyzer.Context = callsiteTyper.context.make(unit = callsiteTyper.context.unit)
        if (phase.erasedTypes) erasure.newTyper(ctx.asInstanceOf[erasure.Context]).asInstanceOf[analyzer.Typer] else analyzer.newTyper(ctx)
      }

      val api = new TypingTransformApi {
        def recur(tree: Tree): Tree = hof(tree, this)
        def default(tree: Tree): Tree = superTransform(tree)
        def atOwner[T](owner: Symbol)(op: => T): T = self.atOwner(owner)(op)
        def atOwner[T](tree: Tree, owner: Symbol)(op: => T): T = self.atOwner(tree, owner)(op)
        def currentOwner: Symbol = self.currentOwner
        def typecheck(tree: Tree): Tree = localTyper.typed(tree)
      }
      def superTransform(tree: Tree) = super.transform(tree)
      override def transform(tree: Tree): Tree = hof(tree, api)
    }

    def typingTransform(tree: Tree)(transformer: (Tree, TypingTransformApi) => Tree): Tree = new HofTypingTransformer(transformer).transform(tree)

    def typingTransform(tree: Tree, owner: Symbol)(transformer: (Tree, TypingTransformApi) => Tree): Tree = {
      val trans = new HofTypingTransformer(transformer)
      trans.atOwner(owner)(trans.transform(tree))
    }
  }

}

case object ContainsAwait
case object NoAwait
