/* NSC -- new scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Iulian Dragos
 */

package scala
package tools.nsc
package transform

import symtab.Flags
import Flags.SYNTHETIC

/** Perform tail recursive call elimination.
 *
 *  @author Iulian Dragos
 *  @version 1.0
 */
abstract class TailCalls extends Transform {
  import global._                     // the global environment
  import definitions._                // standard classes and methods
  import typer.typedPos               // methods to type trees

  val phaseName: String = "tailcalls"

  def newTransformer(unit: CompilationUnit): Transformer =
    new TailCallElimination(unit)

  /** Create a new phase which applies transformer */
  override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = new Phase(prev)

  /** The phase defined by this transform */
  class Phase(prev: scala.tools.nsc.Phase) extends StdPhase(prev) {
    def apply(unit: global.CompilationUnit) {
      if (!(settings.debuginfo.value == "notailcalls")) {
        newTransformer(unit).transformUnit(unit)
      }
    }
  }

  import treeInfo.hasSynthCaseSymbol

  /**
   * A Tail Call Transformer
   *
   * @author     Erik Stenman, Iulian Dragos
   * @version    1.1
   *
   * What it does:
   * <p>
   *   Finds method calls in tail-position and replaces them with jumps.
   *   A call is in a tail-position if it is the last instruction to be
   *   executed in the body of a method.  This is done by recursing over
   *   the trees that may contain calls in tail-position (trees that can't
   *   contain such calls are not transformed). However, they are not that
   *   many.
   * </p>
   * <p>
   *   Self-recursive calls in tail-position are replaced by jumps to a
   *   label at the beginning of the method. As the JVM provides no way to
   *   jump from a method to another one, non-recursive calls in
   *   tail-position are not optimized.
   * </p>
   * <p>
   *   A method call is self-recursive if it calls the current method and
   *   the method is final (otherwise, it could
   *   be a call to an overridden method in a subclass). Furthermore, If
   *   the method has type parameters, the call must contain these
   *   parameters as type arguments. Recursive calls on a different instance
   *   are optimized. Since 'this' is not a local variable, a dummy local val
   *   is added and used as a label parameter. The backend knows to load
   *   the corresponding argument in the 'this' (local at index 0). This dummy local
   *   is never used and should be cleaned up by dead code elimination (when enabled).
   * </p>
   * <p>
   *   This phase has been moved before pattern matching to catch more
   *   of the common cases of tail recursive functions. This means that
   *   more cases should be taken into account (like nested function, and
   *   pattern cases).
   * </p>
   * <p>
   *   If a method contains self-recursive calls, a label is added to at
   *   the beginning of its body and the calls are replaced by jumps to
   *   that label.
   * </p>
   * <p>
   *   Assumes: `Uncurry` has been run already, and no multiple
   *            parameter lists exit.
   * </p>
   */
  class TailCallElimination(unit: CompilationUnit) extends Transformer {
    private def defaultReason = "it contains a recursive call not in tail position"
    private val failPositions = perRunCaches.newMap[TailContext, Position]() withDefault (_.methodPos)
    private val failReasons   = perRunCaches.newMap[TailContext, String]() withDefaultValue defaultReason
    private def tailrecFailure(ctx: TailContext) {
      val method      = ctx.method
      val failReason  = failReasons(ctx)
      val failPos     = failPositions(ctx)

      reporter.error(failPos, s"could not optimize @tailrec annotated $method: $failReason")
    }

    /** Has the label been accessed? Then its symbol is in this set. */
    private val accessed = perRunCaches.newSet[Symbol]()
    // `accessed` was stored as boolean in the current context -- this is no longer tenable
    // with jumps to labels in tailpositions now considered in tailposition,
    // a downstream context may access the label, and the upstream one will be none the wiser
    // this is necessary because tail-calls may occur in places where syntactically they seem impossible
    // (since we now consider jumps to labels that are in tailposition, such as matchEnd(x) {x})

    sealed trait TailContext {
      def method: Symbol          // current method
      def tparams: List[Symbol]   // type parameters
      def methodPos: Position     // default position for failure reporting
      def tailPos: Boolean        // context is in tail position
      def label: Symbol           // new label, tail call target
      def tailLabels: Set[Symbol]

      def enclosingType = method.enclClass.typeOfThis
      def isEligible    = method.isEffectivelyFinalOrNotOverridden
      def isMandatory   = method.hasAnnotation(TailrecClass)
      def isTransformed = isEligible && accessed(label)

      def newThis(pos: Position) = {
        def msg = "Creating new `this` during tailcalls\n  method: %s\n  current class: %s".format(
          method.ownerChain.mkString(" -> "),
          currentClass.ownerChain.mkString(" -> ")
        )
        logResult(msg)(method.newValue(nme.THIS, pos, SYNTHETIC) setInfo currentClass.typeOfThis)
      }
      override def toString = s"${method.name} tparams=$tparams tailPos=$tailPos label=$label label info=${label.info}"

      final def noTailContext() = clonedTailContext(false)
      final def yesTailContext() = clonedTailContext(true)
      protected def clonedTailContext(tailPos: Boolean): TailContext = this match {
        case _ if this.tailPos == tailPos => this
        case clone: ClonedTailContext => clone.that.clonedTailContext(tailPos)
        case _ => new ClonedTailContext(this, tailPos)
      }
    }

    object EmptyTailContext extends TailContext {
      def method     = NoSymbol
      def tparams    = Nil
      def methodPos  = NoPosition
      def tailPos    = false
      def label      = NoSymbol
      def tailLabels = Set.empty[Symbol]
    }

    class DefDefTailContext(dd: DefDef) extends TailContext {
      def method    = dd.symbol
      def tparams   = dd.tparams map (_.symbol)
      def methodPos = dd.pos
      def tailPos   = true

      lazy val label      = mkLabel()
      lazy val tailLabels = {
        // labels are local to a method, so only traverse the rhs of a defdef
        val collector = new TailPosLabelsTraverser
        collector traverse dd.rhs
        collector.tailLabels.toSet
      }

      private def mkLabel() = {
        val label     = method.newLabel(newTermName("_" + method.name), method.pos)
        val thisParam = method.newSyntheticValueParam(currentClass.typeOfThis)
        label setInfo MethodType(thisParam :: method.tpe.params, method.tpe_*.finalResultType)
        if (isEligible)
          label substInfo (method.tpe.typeParams, tparams)

        label
      }
      private def isRecursiveCall(t: Tree) = {
        val receiver = t.symbol

        (    (receiver != null)
          && receiver.isMethod
          && (method.name == receiver.name)
          && (method.enclClass isSubClass receiver.enclClass)
        )
      }
      def containsRecursiveCall(t: Tree) = t exists isRecursiveCall
    }
    class ClonedTailContext(val that: TailContext, override val tailPos: Boolean) extends TailContext {
      def method     = that.method
      def tparams    = that.tparams
      def methodPos  = that.methodPos
      def tailLabels = that.tailLabels
      def label      = that.label
    }

    private var ctx: TailContext = EmptyTailContext

    override def transformUnit(unit: CompilationUnit): Unit = {
      try {
        super.transformUnit(unit)
      } finally {
        // OPT clear these after each compilation unit
        failPositions.clear()
        failReasons.clear()
        accessed.clear()
      }
    }

    /** Rewrite this tree to contain no tail recursive calls */
    def transform(tree: Tree, nctx: TailContext): Tree = {
      val saved = ctx
      ctx = nctx
      try transform(tree)
      finally this.ctx = saved
    }

    def yesTailTransform(tree: Tree): Tree = transform(tree, ctx.yesTailContext())
    def noTailTransform(tree: Tree): Tree = transform(tree, ctx.noTailContext())
    def noTailTransforms(trees: List[Tree]) = {
      val nctx = ctx.noTailContext()
      trees mapConserve (t => transform(t, nctx))
    }

    override def transform(tree: Tree): Tree = {
      /* A possibly polymorphic apply to be considered for tail call transformation. */
      def rewriteApply(target: Tree, fun: Tree, targs: List[Tree], args: List[Tree], mustTransformArgs: Boolean = true) = {
        val receiver: Tree = fun match {
          case Select(qual, _)  => qual
          case _                => EmptyTree
        }
        def receiverIsSame    = ctx.enclosingType.widen =:= receiver.tpe.widen
        def receiverIsSuper   = ctx.enclosingType.widen <:< receiver.tpe.widen
        def isRecursiveCall   = (ctx.method eq fun.symbol) && ctx.tailPos
        def transformArgs     = if (mustTransformArgs) noTailTransforms(args) else args
        def matchesTypeArgs   = ctx.tparams sameElements (targs map (_.tpe.typeSymbol))

        /* Records failure reason in Context for reporting.
         * Position is unchanged (by default, the method definition.)
         */
        def fail(reason: String) = {
          debuglog("Cannot rewrite recursive call at: " + fun.pos + " because: " + reason)
          if (ctx.isMandatory) failReasons(ctx) = reason
          treeCopy.Apply(tree, noTailTransform(target), transformArgs)
        }
        /* Position of failure is that of the tree being considered. */
        def failHere(reason: String) = {
          if (ctx.isMandatory) failPositions(ctx) = fun.pos
          fail(reason)
        }
        def rewriteTailCall(recv: Tree): Tree = {
          debuglog("Rewriting tail recursive call:  " + fun.pos.lineContent.trim)
          accessed += ctx.label
          typedPos(fun.pos) {
            val args = mapWithIndex(transformArgs)((arg, i) => mkAttributedCastHack(arg, ctx.label.info.params(i + 1).tpe))
            Apply(Ident(ctx.label), noTailTransform(recv) :: args)
          }
        }

        if (!ctx.isEligible)            fail("it is neither private nor final so can be overridden")
        else if (!isRecursiveCall) {
          if (ctx.isMandatory && receiverIsSuper) // OPT expensive check, avoid unless we will actually report the error
                                        failHere("it contains a recursive call targeting a supertype")
          else                          failHere(defaultReason)
        }
        else if (!matchesTypeArgs)      failHere("it is called recursively with different type arguments")
        else if (receiver == EmptyTree) rewriteTailCall(This(currentClass))
        else if (!receiverIsSame)       failHere("it changes type of 'this' on a polymorphic recursive call")
        else                            rewriteTailCall(receiver)
      }
      
      def isEligible(tree: DefDef) = {
        val sym = tree.symbol
        !(sym.hasAccessorFlag || sym.isConstructor)
      }

      // intentionally shadowing imports from definitions for performance
      val runDefinitions = currentRun.runDefinitions
      import runDefinitions.{Boolean_or, Boolean_and}

      tree match {
        case dd: DefDef if tree.symbol.isLazy && tree.symbol.hasAnnotation(TailrecClass) =>
          reporter.error(tree.pos, "lazy vals are not tailcall transformed")
          super.transform(tree)

        case dd @ DefDef(_, name, _, vparamss0, _, rhs0) if isEligible(dd) =>
          val newCtx = new DefDefTailContext(dd)
          if (newCtx.isMandatory && !(newCtx containsRecursiveCall rhs0))
            reporter.error(tree.pos, "@tailrec annotated method contains no recursive calls")

          debuglog(s"Considering $name for tailcalls, with labels in tailpos: ${newCtx.tailLabels}")
          val newRHS = transform(rhs0, newCtx)

          deriveDefDef(tree) { rhs =>
            if (newCtx.isTransformed) {
              /* We have rewritten the tree, but there may be nested recursive calls remaining.
               * If @tailrec is given we need to fail those now.
               */
              if (newCtx.isMandatory) {
                for (t @ Apply(fn, _) <- newRHS ; if fn.symbol == newCtx.method) {
                  failPositions(newCtx) = t.pos
                  tailrecFailure(newCtx)
                }
              }
              val newThis = newCtx.newThis(tree.pos)
              val vpSyms  = vparamss0.flatten map (_.symbol)

              typedPos(tree.pos)(Block(
                List(ValDef(newThis, This(currentClass))),
                LabelDef(newCtx.label, newThis :: vpSyms, mkAttributedCastHack(newRHS, newCtx.label.tpe.resultType))
              ))
            }
            else {
              if (newCtx.isMandatory && (newCtx containsRecursiveCall newRHS))
                tailrecFailure(newCtx)

              newRHS
            }
          }

        // a translated match
        case Block(stats, expr) if stats forall hasSynthCaseSymbol =>
          // the assumption is once we encounter a case, the remainder of the block will consist of cases
          // the prologue may be empty, usually it is the valdef that stores the scrut
          val (prologue, cases) = stats span (s => !s.isInstanceOf[LabelDef])
          val transformedPrologue = noTailTransforms(prologue)
          val transformedCases = transformTrees(cases)
          val transformedStats =
            if ((prologue eq transformedPrologue) && (cases eq transformedCases)) stats // allow reuse of `tree` if the subtransform was an identity
            else transformedPrologue ++ transformedCases
          treeCopy.Block(tree,
            transformedStats,
            transform(expr)
          )

        // a translated casedef
        case LabelDef(_, _, body) if hasSynthCaseSymbol(tree) =>
          deriveLabelDef(tree)(transform)

        case Block(stats, expr) =>
          treeCopy.Block(tree,
            noTailTransforms(stats),
            transform(expr)
          )

        case CaseDef(pat, guard, body) =>
          // CaseDefs are already translated and guards were moved into the body.
          // If this was not the case, guards would have to be transformed here as well.
          assert(guard.isEmpty)
          deriveCaseDef(tree)(transform)

        case If(cond, thenp, elsep) =>
          treeCopy.If(tree,
            noTailTransform(cond),
            transform(thenp),
            transform(elsep)
          )

        case Match(selector, cases) =>
          treeCopy.Match(tree,
            noTailTransform(selector),
            transformTrees(cases).asInstanceOf[List[CaseDef]]
          )

        case Try(block, catches, finalizer @ EmptyTree) =>
          // SI-1672 Catches are in tail position when there is no finalizer
          treeCopy.Try(tree,
            noTailTransform(block),
            transformTrees(catches).asInstanceOf[List[CaseDef]],
            EmptyTree
          )

        case Try(block, catches, finalizer) =>
           // no calls inside a try are in tail position if there is a finalizer, but keep recursing for nested functions
          treeCopy.Try(tree,
            noTailTransform(block),
            noTailTransforms(catches).asInstanceOf[List[CaseDef]],
            noTailTransform(finalizer)
          )

        case Apply(tapply @ TypeApply(fun, targs), vargs) =>
          rewriteApply(tapply, fun, targs, vargs)

        case Apply(fun, args) if fun.symbol == Boolean_or || fun.symbol == Boolean_and =>
          treeCopy.Apply(tree, noTailTransform(fun), transformTrees(args))

        // this is to detect tailcalls in translated matches
        // it's a one-argument call to a label that is in a tailposition and that looks like label(x) {x}
        // thus, the argument to the call is in tailposition
        case Apply(fun, args @ (arg :: Nil)) if fun.symbol.isLabel && ctx.tailLabels(fun.symbol) =>
          debuglog(s"in tailpos label: $arg")
          val res = yesTailTransform(arg)
          // we tail-called -- TODO: shield from false-positives where we rewrite but don't tail-call
          // must leave the jump to the original tailpos-label (fun)!
          // there might be *a* tailcall *in* res, but it doesn't mean res *always* tailcalls
          if (res ne arg)
            treeCopy.Apply(tree, fun, res :: Nil)
          else
            rewriteApply(fun, fun, Nil, args, mustTransformArgs = false)

        case Apply(fun, args) =>
          rewriteApply(fun, fun, Nil, args)
        case Alternative(_) | Star(_) | Bind(_, _) =>
          sys.error("We should've never gotten inside a pattern")
        case Select(qual, name) =>
          treeCopy.Select(tree, noTailTransform(qual), name)
        case EmptyTree | Super(_, _) | This(_) | Ident(_) | Literal(_) | Function(_, _) | TypeTree() =>
          tree
        case _ =>
          super.transform(tree)
      }
    }

    // Workaround for SI-6900. Uncurry installs an InfoTransformer and a tree Transformer.
    // These leave us with conflicting view on method signatures; the parameter symbols in
    // the MethodType can be clones of the ones originally found on the parameter ValDef, and
    // consequently appearing in the typechecked RHS of the method.
    private def mkAttributedCastHack(tree: Tree, tpe: Type) =
      gen.mkAttributedCast(tree, tpe)
  }

  // collect the LabelDefs (generated by the pattern matcher) in a DefDef that are in tail position
  // the labels all look like: matchEnd(x) {x}
  // then, in a forward jump `matchEnd(expr)`, `expr` is considered in tail position (and the matchEnd jump is replaced by the jump generated by expr)
  class TailPosLabelsTraverser extends Traverser {
    val tailLabels = new scala.collection.mutable.HashSet[Symbol]()

    private var maybeTail: Boolean = true // since we start in the rhs of a DefDef

    def traverse(tree: Tree, maybeTailNew: Boolean): Unit = {
      val saved = maybeTail
      maybeTail = maybeTailNew
      try traverse(tree)
      finally maybeTail = saved
    }

    def traverseNoTail(tree: Tree) = traverse(tree, maybeTailNew = false)
    def traverseTreesNoTail(trees: List[Tree]) = trees foreach traverseNoTail

    // intentionally shadowing imports from definitions for performance
    private val runDefinitions = currentRun.runDefinitions
    import runDefinitions.{Boolean_or, Boolean_and}

    override def traverse(tree: Tree) = tree match {
      // we're looking for label(x){x} in tail position, since that means `a` is in tail position in a call `label(a)`
      case LabelDef(_, List(arg), body@Ident(_)) if arg.symbol == body.symbol =>
        if (maybeTail) tailLabels += tree.symbol

      // jumps to matchEnd are transparent; need this case for nested matches
      // (and the translated match case below does things in reverse for this case's sake)
      case Apply(fun, arg :: Nil) if hasSynthCaseSymbol(fun) && tailLabels(fun.symbol) =>
        traverse(arg)

      case Apply(fun, args) if (fun.symbol == Boolean_or || fun.symbol == Boolean_and) =>
        traverseTrees(args)

      // a translated casedef
      case LabelDef(_, _, body) if hasSynthCaseSymbol(tree) =>
        traverse(body)

      // a translated match
      case Block(stats, expr) if stats forall hasSynthCaseSymbol =>
        // the assumption is once we encounter a case, the remainder of the block will consist of cases
        // the prologue may be empty, usually it is the valdef that stores the scrut
        val (prologue, cases) = stats span (s => !s.isInstanceOf[LabelDef])
        traverse(expr)
        traverseTrees(cases.reverse)  // reverse so that we enter the matchEnd LabelDef before we see jumps to it
        traverseTreesNoTail(prologue) // selector (may be absent)

      case CaseDef(pat, guard, body) =>
        traverse(body)

      case Match(selector, cases) =>
        traverseNoTail(selector)
        traverseTrees(cases)

      case dd @ DefDef(_, _, _, _, _, _) => // we are run per-method

      case Block(stats, expr) =>
        traverseTreesNoTail(stats)
        traverse(expr)

      case If(cond, thenp, elsep) =>
        traverse(thenp)
        traverse(elsep)

      case Try(block, catches, finalizer) =>
        traverseNoTail(block)
        traverseTreesNoTail(catches)
        traverseNoTail(finalizer)

      case Apply(_, _) | EmptyTree | Super(_, _) | This(_) | Select(_, _) | Ident(_) | Literal(_) | Function(_, _) | TypeTree() =>
      case _ => super.traverse(tree)
    }
  }
}
