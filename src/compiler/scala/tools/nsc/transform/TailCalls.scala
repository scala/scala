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

    class Context() {
      /** The current method */
      var method: Symbol = NoSymbol

      /** The current tail-call label */
      var label: Symbol = NoSymbol

      /** The expected type arguments of self-recursive calls */
      var tparams: List[Symbol] = Nil

      /** Tells whether we are in a (possible) tail position */
      var tailPos = false

      /** The reason this method could not be optimized. */
      var failReason = defaultReason
      var failPos    = method.pos

      /** Has the label been accessed? */
      var accessed = false

      def this(that: Context) = {
        this()
        this.method   = that.method
        this.tparams  = that.tparams
        this.tailPos  = that.tailPos
        this.accessed = that.accessed
        this.failPos  = that.failPos
        this.label    = that.label
      }
      def this(dd: DefDef) {
        this()
        this.method   = dd.symbol
        this.tparams  = dd.tparams map (_.symbol)
        this.tailPos  = true
        this.accessed = false
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
      def isTransformed    = isEligible && accessed
      def tailrecFailure() = unit.error(failPos, "could not optimize @tailrec annotated " + method + ": " + failReason)

      def newThis(pos: Position) = method.newValue(nme.THIS, pos, SYNTHETIC) setInfo currentClass.typeOfThis

      override def toString(): String = (
        "" + method.name + " tparams: " + tparams + " tailPos: " + tailPos +
        " accessed: " + accessed + "\nLabel: " + label + "\nLabel type: " + label.info
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

          ctx.accessed = true
          typedPos(fun.pos)(Apply(Ident(ctx.label), recv :: transformArgs))
        }

        if (!ctx.isEligible)            fail("it is neither private nor final so can be overridden")
        else if (!isRecursiveCall) {
          if (receiverIsSuper)          failHere("it contains a recursive call targetting a supertype")
          else                          failHere(defaultReason)
        }
        else if (!matchesTypeArgs)      failHere("it is called recursively with different type arguments")
        else if (receiver == EmptyTree) rewriteTailCall(This(currentClass))
        else if (forMSIL)               fail("it cannot be optimized on MSIL")
        else if (!receiverIsSame)       failHere("it changes type of 'this' on a polymorphic recursive call")
        else                            rewriteTailCall(receiver)
      }

      tree match {
        case dd @ DefDef(_, _, _, vparamss0, _, rhs0) =>
          val newCtx = new Context(dd)
          debuglog("Considering " + dd.name + " for tailcalls")
          val newRHS = transform(rhs0, newCtx)

          deriveDefDef(tree)(rhs =>
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
              if (newCtx.isMandatory)
                newCtx.tailrecFailure()

              newRHS
            }
          )

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
          else
            rewriteApply(fun, fun, Nil, args)

        case Alternative(_) | Star(_) | Bind(_, _) =>
          sys.error("We should've never gotten inside a pattern")
        case EmptyTree | Super(_, _) | This(_) | Select(_, _) | Ident(_) | Literal(_) | Function(_, _) | TypeTree() =>
          tree
        case _ =>
          super.transform(tree)
      }
    }
  }
}
