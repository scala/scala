/* NSC -- new scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author Iulian Dragos
 */
// $Id$

package scala.tools.nsc.transform

import scala.tools.nsc.symtab.Flags

/** Perform tail recursive call elimination.
 *
 *  @author Iulian Dragos
 *  @version 1.0
 */
abstract class TailCalls extends Transform
                         /* with JavaLogging() */ {
  // inherits abstract value `global' and class `Phase' from Transform

  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import typer.{typed, atOwner}    // methods to type trees
  import posAssigner.atPos         // for filling in tree positions

  val phaseName: String = "tailcalls"

  def newTransformer(unit: CompilationUnit): Transformer =
    new TailCallElimination(unit)

  /** Create a new phase which applies transformer */
  override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = new Phase(prev)

  /** The phase defined by this transform */
  class Phase(prev: scala.tools.nsc.Phase) extends StdPhase(prev) {
    def apply(unit: global.CompilationUnit): unit =
      if (!(settings.debuginfo.value == "notc")) {
        newTransformer(unit).transformUnit(unit);
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
   *   A method call is self-recursive if it calls the current method on
   *   the current instance and the method is final (otherwise, it could
   *   be a call to an overridden method in a subclass). Furthermore, If
   *   the method has type parameters, the call must contain these
   *   parameters as type arguments.
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

    class Context {
      /** The current method */
      var currentMethod: Symbol = NoSymbol

      /** The current tail-call label */
      var label: Symbol = NoSymbol

      /** The expected type arguments of self-recursive calls */
      var tparams: List[Symbol] = Nil

      /** Tells whether we are in a (possible) tail position */
      var tailPos = false

      /** Is the label accessed? */
      var accessed = false

      def this(that: Context) = {
        this()
        this.currentMethod = that.currentMethod
        this.label         = that.label
        this.tparams       = that.tparams
        this.tailPos       = that.tailPos
        this.accessed      = that.accessed
      }

      /** Create a new method symbol for the current method and store it in
        * the label field.
        */
      def makeLabel(): Unit = {
        label = currentMethod.newLabel(currentMethod.pos, "_" + currentMethod.name)
        accessed = false
      }

      override def toString(): String = (
        "" + currentMethod.name + " tparams: " + tparams + " tailPos: " + tailPos +
        " accessed: " + accessed + "\nLabel: " + label + "\nLabel type: " + label.info
      )
    }

    private def mkContext(that: Context) = new Context(that)
    private def mkContext(that: Context, tp: Boolean): Context = {
      val t = mkContext(that)
      t.tailPos = tp
      t
    }

    private var ctx: Context = new Context()

    /** Rewrite this tree to contain no tail recursive calls */
    def transform(tree: Tree, nctx: Context): Tree = {
      val oldCtx = ctx
      ctx = nctx
      val t = transform(tree)
      this.ctx = oldCtx
      t
    }

    override def transform(tree: Tree): Tree = {
      tree match {

        case DefDef(mods, name, tparams, vparams, tpt, rhs) =>
          log("Entering DefDef: " + name)
          val newCtx = mkContext(ctx)
          newCtx.currentMethod = tree.symbol
          newCtx.makeLabel()
          newCtx.label.setInfo(tree.symbol.info)
          newCtx.tailPos = true

          val t1 = if (newCtx.currentMethod.isFinal ||
                       newCtx.currentMethod.enclClass.hasFlag(Flags.MODULE)) {
            newCtx.tparams = Nil
            log("  Considering " + name + " for tailcalls")
            tree.symbol.tpe match {
              case PolyType(tpes, restpe) =>
                newCtx.tparams = tparams map (.symbol)
                newCtx.label.setInfo(
                  restpe.substSym(tpes, tparams map (.symbol)))
              case _ => ()
            }

            var newRHS = transform(rhs, newCtx);
            if (newCtx.accessed) {
              log("Rewrote def " + newCtx.currentMethod)

              newRHS =
                  typed(atPos(tree.pos)(
                    LabelDef(newCtx.label,
                             List.flatten(vparams) map (.symbol),
                             newRHS)));
              copy.DefDef(tree, mods, name, tparams, vparams, tpt, newRHS);
            } else
              copy.DefDef(tree, mods, name, tparams, vparams, tpt, newRHS);
          } else {
            copy.DefDef(tree, mods, name, tparams, vparams, tpt, transform(rhs, newCtx))
          }
          log("Leaving DefDef: " + name)
          t1

        case EmptyTree => tree

        case PackageDef(name, stats) =>
          super.transform(tree)

        case ClassDef(_, name, _, _, _) =>
          log("Entering class " + name)
          val res = super.transform(tree)
          log("Leaving class " + name)
          res

        case ValDef(mods, name, tpt, rhs) => tree
        case AbsTypeDef(mods, name, lo, hi) => tree //  (eliminated by erasure)
        case AliasTypeDef(mods, name, tparams, rhs) => tree // (eliminated by erasure)
        case LabelDef(name, params, rhs) => super.transform(tree)

        case Template(parents, body) =>
          super.transform(tree)

        case Block(stats, expr) =>
          copy.Block(tree,
                     transformTrees(stats, mkContext(ctx, false)),
                     transform(expr))

        case CaseDef(pat, guard, body) =>
          copy.CaseDef(tree, pat, guard, transform(body))

        case Sequence(_) | Alternative(_) |
             Star(_)     | Bind(_, _) =>
          throw new RuntimeException("We should've never gotten inside a pattern")

        case Function(vparams, body) =>
          tree
          //throw new RuntimeException("Anonymous function should not exist at this point. at: " + unit.position(tree.pos));

        case Assign(lhs, rhs) =>
          super.transform(tree)

        case If(cond, thenp, elsep) =>
          copy.If(tree, cond, transform(thenp), transform(elsep))

        case Match(selector, cases) => //super.transform(tree);
          copy.Match(tree, transform(selector, mkContext(ctx, false)), transformTrees(cases).asInstanceOf[List[CaseDef]])

        case Return(expr) => super.transform(tree)
        case Try(block, catches, finalizer) =>
          if (finalizer == EmptyTree) super.transform(tree)
          else tree

        case Throw(expr) => super.transform(tree)
        case New(tpt) => super.transform(tree)
        case Typed(expr, tpt) => super.transform(tree)

        case Apply(tapply @ TypeApply(fun, targs), vargs) =>
          if ( ctx.currentMethod.isFinal &&
               ctx.tailPos &&
               isSameTypes(ctx.tparams, targs map (.tpe.symbol)) &&
               isRecursiveCall(fun))
                 rewriteTailCall(fun, transformTrees(vargs, mkContext(ctx, false)))
               else
                 copy.Apply(tree, tapply, transformTrees(vargs, mkContext(ctx, false)))

        case TypeApply(fun, args) =>
          super.transform(tree)

        case Apply(fun, args) if fun.symbol == definitions.Boolean_or =>
          copy.Apply(tree, fun, transformTrees(args))

        case Apply(fun, args) =>
          if (ctx.currentMethod.isFinal &&
              ctx.tailPos &&
              isRecursiveCall(fun))
            rewriteTailCall(fun, transformTrees(args, mkContext(ctx, false)))
          else
            copy.Apply(tree, fun, transformTrees(args, mkContext(ctx, false)))

        case Super(qual, mix) =>
          tree
        case This(qual) =>
          tree
        case Select(qualifier, selector) =>
          tree
        case Ident(name) =>
          tree
        case Literal(value) =>
          tree
        case TypeTree() =>
          tree
        case _ =>
          tree
      }
    }

    def transformTrees(trees: List[Tree], nctx: Context): List[Tree] =
      trees map ((tree) => transform(tree, nctx))

    private def rewriteTailCall(fun: Tree, args: List[Tree]): Tree = {
      log("Rewriting tail recursive method call at: " +
                      unit.position(fun.pos))
      ctx.accessed = true
      typed(atPos(fun.pos)(
        Apply(Ident(ctx.label), args)))
    }

    private def isSameTypes(ts1: List[Symbol], ts2: List[Symbol]): Boolean = {
      def isSameType(t1: Symbol, t2: Symbol) = {
        t1 == t2
      }
      List.forall2(ts1, ts2)(isSameType)
    }

    /** Returns <code>true</code> if the fun tree refers to the same method as
     *  the one saved in <code>ctx</code>. If it is a method call, we check
     *  that it is applied to <code>this</code>.
     *
     *  @param fun ...
     *  @return    <code>true</code> ...
     */
    private def isRecursiveCall(fun: Tree): Boolean =
      if (fun.symbol eq ctx.currentMethod)
        fun match {
          case Select(t @ This(_), _) =>
            assert(t.symbol == ctx.currentMethod.owner, "This refers to other class: " +
                   t.symbol + ": " + ctx.currentMethod.owner)
            true

          case Ident(_) => true
          case _ => false
        }
      else
        false
  }

}
