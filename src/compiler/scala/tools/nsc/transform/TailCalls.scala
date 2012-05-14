/* NSC -- new scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Iulian Dragos
 */

package scala.tools.nsc
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
  import typer.{ typed, typedPos }    // methods to type trees

  val phaseName: String = "tailcalls"

  def newTransformer(unit: CompilationUnit): Transformer =
    new TailCallElimination(unit)

  /** Create a new phase which applies transformer */
  override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = new Phase(prev)

  /** The phase defined by this transform */
  class Phase(prev: scala.tools.nsc.Phase) extends StdPhase(prev) {
    def apply(unit: global.CompilationUnit) {
      if (!(settings.debuginfo.value == "notailcalls")) {
        newTransformer(unit).transformUnit(unit);
      }
    }
  }

  import gen.hasSynthCaseSymbol

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
   *   is never used and should be cleand up by dead code elimination (when enabled).
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
   *   Assumes: <code>Uncurry</code> has been run already, and no multiple
   *            parameter lists exit.
   * </p>
   */
  class TailCallElimination(unit: CompilationUnit) extends Transformer {
    private val defaultReason = "it contains a recursive call not in tail position"

    /** Has the label been accessed? Then its symbol is in this set. */
    private val accessed = new collection.mutable.HashSet[Symbol]()
    // `accessed` was stored as boolean in the current context -- this is no longer tenable
    // with jumps to labels in tailpositions now considered in tailposition,
    // a downstream context may access the label, and the upstream one will be none the wiser
    // this is necessary because tail-calls may occur in places where syntactically they seem impossible
    // (since we now consider jumps to labels that are in tailposition, such as matchEnd(x) {x})


    class Context() {
      /** The current method */
      var method: Symbol = NoSymbol

      // symbols of label defs in this method that are in tail position
      var tailLabels: Set[Symbol] = Set()

      /** The current tail-call label */
      var label: Symbol = NoSymbol

      /** The expected type arguments of self-recursive calls */
      var tparams: List[Symbol] = Nil

      /** Tells whether we are in a (possible) tail position */
      var tailPos = false

      /** The reason this method could not be optimized. */
      var failReason = defaultReason
      var failPos    = method.pos

      def this(that: Context) = {
        this()
        this.method   = that.method
        this.tparams  = that.tparams
        this.tailPos  = that.tailPos
        this.failPos  = that.failPos
        this.label    = that.label
        this.tailLabels = that.tailLabels
      }
      def this(dd: DefDef) {
        this()
        this.method   = dd.symbol
        this.tparams  = dd.tparams map (_.symbol)
        this.tailPos  = true
        this.failPos  = dd.pos

        /** Create a new method symbol for the current method and store it in
          * the label field.
          */
        this.label    = {
          val label     = method.newLabel(newTermName("_" + method.name), method.pos)
          val thisParam = method.newSyntheticValueParam(currentClass.typeOfThis)
          label setInfo MethodType(thisParam :: method.tpe.params, method.tpe.finalResultType)
        }
        if (isEligible)
          label substInfo (method.tpe.typeParams, tparams)
      }

      def enclosingType    = method.enclClass.typeOfThis
      def methodTypeParams = method.tpe.typeParams
      def isEligible       = method.isEffectivelyFinal
      // @tailrec annotation indicates mandatory transformation
      def isMandatory      = method.hasAnnotation(TailrecClass) && !forMSIL
      def isTransformed    = isEligible && accessed(label)
      def tailrecFailure() = unit.error(failPos, "could not optimize @tailrec annotated " + method + ": " + failReason)

      def newThis(pos: Position) = logResult("Creating new `this` during tailcalls\n  method: %s\n  current class: %s".format(
        method.ownerChain.mkString(" -> "), currentClass.ownerChain.mkString(" -> "))) {
          method.newValue(nme.THIS, pos, SYNTHETIC) setInfo currentClass.typeOfThis
      }

      override def toString(): String = (
        "" + method.name + " tparams: " + tparams + " tailPos: " + tailPos +
        " Label: " + label + " Label type: " + label.info
      )
    }

    private var ctx: Context = new Context()
    private def noTailContext() = {
      val t = new Context(ctx)
      t.tailPos = false
      t
    }

    /** Rewrite this tree to contain no tail recursive calls */
    def transform(tree: Tree, nctx: Context): Tree = {
      val saved = ctx
      ctx = nctx
      try transform(tree)
      finally this.ctx = saved
    }

    def noTailTransform(tree: Tree): Tree = transform(tree, noTailContext())
    def noTailTransforms(trees: List[Tree]) = {
      val nctx = noTailContext()
      trees map (t => transform(t, nctx))
    }

    override def transform(tree: Tree): Tree = {
      /** A possibly polymorphic apply to be considered for tail call transformation.
       */
      def rewriteApply(target: Tree, fun: Tree, targs: List[Tree], args: List[Tree]) = {
        val receiver: Tree = fun match {
          case Select(qual, _)  => qual
          case _                => EmptyTree
        }

        def receiverIsSame    = ctx.enclosingType.widen =:= receiver.tpe.widen
        def receiverIsSuper   = ctx.enclosingType.widen <:< receiver.tpe.widen
        def isRecursiveCall   = (ctx.method eq fun.symbol) && ctx.tailPos
        def transformArgs     = noTailTransforms(args)
        def matchesTypeArgs   = ctx.tparams sameElements (targs map (_.tpe.typeSymbol))

        /** Records failure reason in Context for reporting.
         *  Position is unchanged (by default, the method definition.)
         */
        def fail(reason: String) = {
          debuglog("Cannot rewrite recursive call at: " + fun.pos + " because: " + reason)

          ctx.failReason = reason
          treeCopy.Apply(tree, target, transformArgs)
        }
        /** Position of failure is that of the tree being considered.
         */
        def failHere(reason: String) = {
          ctx.failPos = fun.pos
          fail(reason)
        }
        def rewriteTailCall(recv: Tree): Tree = {
          debuglog("Rewriting tail recursive call:  " + fun.pos.lineContent.trim)

          accessed += ctx.label
          typedPos(fun.pos)(Apply(Ident(ctx.label), recv :: transformArgs))
        }

        if (!ctx.isEligible)            fail("it is neither private nor final so can be overridden")
        else if (!isRecursiveCall) {
          if (receiverIsSuper)          failHere("it contains a recursive call targeting supertype " + receiver.tpe)
          else                          failHere(defaultReason)
        }
        else if (!matchesTypeArgs)      failHere("it is called recursively with different type arguments")
        else if (receiver == EmptyTree) rewriteTailCall(This(currentClass))
        else if (forMSIL)               fail("it cannot be optimized on MSIL")
        else if (!receiverIsSame)       failHere("it changes type of 'this' on a polymorphic recursive call")
        else                            rewriteTailCall(receiver)
      }

      tree match {
        case ValDef(_, _, _, _) =>
          if (tree.symbol.isLazy && tree.symbol.hasAnnotation(TailrecClass))
            unit.error(tree.pos, "lazy vals are not tailcall transformed")

          super.transform(tree)

        case dd @ DefDef(_, _, _, vparamss0, _, rhs0) if !dd.symbol.hasAccessorFlag =>
          val newCtx = new Context(dd)
          def isRecursiveCall(t: Tree) = {
            val sym = t.symbol
            (sym != null) && {
              sym.isMethod && (dd.symbol.name == sym.name) && (dd.symbol.enclClass isSubClass sym.enclClass)
            }
          }
          if (newCtx.isMandatory) {
            if (!rhs0.exists(isRecursiveCall)) {
              unit.error(tree.pos, "@tailrec annotated method contains no recursive calls")
            }
          }

          // labels are local to a method, so only traverse the rhs of a defdef
          val collectTailPosLabels = new TailPosLabelsTraverser
          collectTailPosLabels traverse rhs0
          newCtx.tailLabels = collectTailPosLabels.tailLabels.toSet

          debuglog("Considering " + dd.name + " for tailcalls, with labels in tailpos: "+ newCtx.tailLabels)
          val newRHS = transform(rhs0, newCtx)

          deriveDefDef(tree){rhs =>
            if (newCtx.isTransformed) {
              /** We have rewritten the tree, but there may be nested recursive calls remaining.
               *  If @tailrec is given we need to fail those now.
               */
              if (newCtx.isMandatory) {
                for (t @ Apply(fn, _) <- newRHS ; if fn.symbol == newCtx.method) {
                  newCtx.failPos = t.pos
                  newCtx.tailrecFailure()
                }
              }
              val newThis = newCtx.newThis(tree.pos)
              val vpSyms  = vparamss0.flatten map (_.symbol)

              typedPos(tree.pos)(Block(
                List(ValDef(newThis, This(currentClass))),
                LabelDef(newCtx.label, newThis :: vpSyms, newRHS)
              ))
            }
            else {
              if (newCtx.isMandatory && newRHS.exists(isRecursiveCall))
                newCtx.tailrecFailure()

              newRHS
            }
          }

        // a translated match
        case Block(stats, expr) if stats forall hasSynthCaseSymbol =>
          // the assumption is once we encounter a case, the remainder of the block will consist of cases
          // the prologue may be empty, usually it is the valdef that stores the scrut
          val (prologue, cases) = stats span (s => !s.isInstanceOf[LabelDef])
          treeCopy.Block(tree,
            noTailTransforms(prologue) ++ transformTrees(cases),
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
          deriveCaseDef(tree)(transform)

        case If(cond, thenp, elsep) =>
          treeCopy.If(tree,
            cond,
            transform(thenp),
            transform(elsep)
          )

        case Match(selector, cases) =>
          treeCopy.Match(tree,
            noTailTransform(selector),
            transformTrees(cases).asInstanceOf[List[CaseDef]]
          )

        case Try(block, catches, finalizer) =>
           // no calls inside a try are in tail position, but keep recursing for nested functions
          treeCopy.Try(tree,
            noTailTransform(block),
            noTailTransforms(catches).asInstanceOf[List[CaseDef]],
            noTailTransform(finalizer)
          )

        case Apply(tapply @ TypeApply(fun, targs), vargs) =>
          rewriteApply(tapply, fun, targs, vargs)

        case Apply(fun, args) =>
          if (fun.symbol == Boolean_or || fun.symbol == Boolean_and)
            treeCopy.Apply(tree, fun, transformTrees(args))
          else if (fun.symbol.isLabel && args.nonEmpty && args.tail.isEmpty && ctx.tailLabels(fun.symbol)) {
            // this is to detect tailcalls in translated matches
            // it's a one-argument call to a label that is in a tailposition and that looks like label(x) {x}
            // thus, the argument to the call is in tailposition
            val saved = ctx.tailPos
            ctx.tailPos = true
            debuglog("in tailpos label: "+ args.head)
            val res = transform(args.head)
            ctx.tailPos = saved
            if (res ne args.head) {
              // we tail-called -- TODO: shield from false-positives where we rewrite but don't tail-call
              // must leave the jump to the original tailpos-label (fun)!
              // there might be *a* tailcall *in* res, but it doesn't mean res *always* tailcalls
              treeCopy.Apply(tree, fun, List(res))
            }
            else rewriteApply(fun, fun, Nil, args)
          } else rewriteApply(fun, fun, Nil, args)

        case Alternative(_) | Star(_) | Bind(_, _) =>
          sys.error("We should've never gotten inside a pattern")
        case EmptyTree | Super(_, _) | This(_) | Select(_, _) | Ident(_) | Literal(_) | Function(_, _) | TypeTree() =>
          tree
        case _ =>
          super.transform(tree)
      }
    }
  }

  // collect the LabelDefs (generated by the pattern matcher) in a DefDef that are in tail position
  // the labels all look like: matchEnd(x) {x}
  // then, in a forward jump `matchEnd(expr)`, `expr` is considered in tail position (and the matchEnd jump is replaced by the jump generated by expr)
  class TailPosLabelsTraverser extends Traverser {
    val tailLabels = new collection.mutable.ListBuffer[Symbol]()

    private var maybeTail: Boolean = true // since we start in the rhs of a DefDef

    def traverse(tree: Tree, maybeTailNew: Boolean): Unit = {
      val saved = maybeTail
      maybeTail = maybeTailNew
      try traverse(tree)
      finally maybeTail = saved
    }

    def traverseNoTail(tree: Tree) = traverse(tree, false)
    def traverseTreesNoTail(trees: List[Tree]) = trees foreach traverseNoTail

    override def traverse(tree: Tree) = tree match {
      case LabelDef(_, List(arg), body@Ident(_)) if arg.symbol == body.symbol => // we're looking for label(x){x} in tail position, since that means `a` is in tail position in a call `label(a)`
        if (maybeTail) tailLabels += tree.symbol

      // a translated casedef
      case LabelDef(_, _, body) if hasSynthCaseSymbol(tree) =>
        traverse(body)

      // a translated match
      case Block(stats, expr) if stats forall hasSynthCaseSymbol =>
        // the assumption is once we encounter a case, the remainder of the block will consist of cases
        // the prologue may be empty, usually it is the valdef that stores the scrut
        val (prologue, cases) = stats span (s => !s.isInstanceOf[LabelDef])
        traverseTreesNoTail(prologue) // selector (may be absent)
        traverseTrees(cases)
        traverse(expr)

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

      case EmptyTree | Super(_, _) | This(_) | Select(_, _) | Ident(_) | Literal(_) | Function(_, _) | TypeTree() =>
      case _ => super.traverse(tree)
    }
  }
}
