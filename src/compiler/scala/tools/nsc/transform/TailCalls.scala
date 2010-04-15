/* NSC -- new scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Iulian Dragos
 */
// $Id$

package scala.tools.nsc
package transform

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

  /** The @tailrec annotation indicates TCO is mandatory */
  private def tailrecRequired(defdef: DefDef) = defdef.symbol hasAnnotation TailrecClass

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

        case dd @ DefDef(mods, name, tparams, vparams, tpt, rhs) =>
          log("Entering DefDef: " + name)
          var isTransformed = false
          val newCtx = mkContext(ctx)
          newCtx.currentMethod = tree.symbol
          newCtx.makeLabel()
          val currentClassParam = tree.symbol.newSyntheticValueParam(currentClass.typeOfThis)
          newCtx.label.setInfo(MethodType(currentClassParam :: tree.symbol.tpe.params, tree.symbol.tpe.finalResultType))
          newCtx.tailPos = true

          val isEligible = newCtx.currentMethod.isEffectivelyFinal || (newCtx.currentMethod.enclClass hasFlag Flags.MODULE)

          if (isEligible) {
            newCtx.tparams = Nil
            log("  Considering " + name + " for tailcalls")
            tree.symbol.tpe match {
              case PolyType(tpes, restpe) =>
                newCtx.tparams = tparams map (_.symbol)
                newCtx.label.setInfo(
                newCtx.label.tpe.substSym(tpes, tparams map (_.symbol)))
              case _ =>
            }
          }

          val t1 = treeCopy.DefDef(tree, mods, name, tparams, vparams, tpt, {
            val transformed = transform(rhs, newCtx)

            transformed match {
              case newRHS if isEligible && newCtx.accessed =>
                log("Rewrote def " + newCtx.currentMethod)
                isTransformed = true
                val newThis = newCtx.currentMethod
                  . newValue (tree.pos, nme.THIS)
                  . setInfo (currentClass.typeOfThis)
                  . setFlag (Flags.SYNTHETIC)

                typed(atPos(tree.pos)(Block(
                  List(ValDef(newThis, This(currentClass))),
                  LabelDef(newCtx.label, newThis :: (vparams.flatten map (_.symbol)), newRHS)
                )))
              case rhs => rhs
            }
          })

          if (!forMSIL && !isTransformed && tailrecRequired(dd))
            unit.error(dd.pos, "could not optimize @tailrec annotated method")

          log("Leaving DefDef: " + name)
          t1

        case EmptyTree => tree

        case PackageDef(_, _) =>
          super.transform(tree)

        case ClassDef(_, name, _, _) =>
          log("Entering class " + name)
          val res = super.transform(tree)
          log("Leaving class " + name)
          res

        case ValDef(mods, name, tpt, rhs) => super.transform(tree)
        case LabelDef(name, params, rhs) => super.transform(tree)

        case Template(parents, self, body) =>
          super.transform(tree)

        case Block(stats, expr) =>
          treeCopy.Block(tree,
                         transformTrees(stats, mkContext(ctx, false)),
                         transform(expr))

        case CaseDef(pat, guard, body) =>
          treeCopy.CaseDef(tree, pat, guard, transform(body))

        case Alternative(_) | Star(_) | Bind(_, _) =>
          throw new RuntimeException("We should've never gotten inside a pattern")

        case Function(vparams, body) =>
          tree
          //throw new RuntimeException("Anonymous function should not exist at this point. at: " + unit.position(tree.pos));

        case Assign(lhs, rhs) =>
          super.transform(tree)

        case If(cond, thenp, elsep) =>
          treeCopy.If(tree, cond, transform(thenp), transform(elsep))

        case Match(selector, cases) => //super.transform(tree);
          treeCopy.Match(tree, transform(selector, mkContext(ctx, false)), transformTrees(cases).asInstanceOf[List[CaseDef]])

        case Return(expr) => super.transform(tree)
        case Try(block, catches, finalizer) =>
           // no calls inside a try are in tail position, but keep recursing for nested functions
          treeCopy.Try(tree, transform(block, mkContext(ctx, false)),
                       transformTrees(catches, mkContext(ctx, false)).asInstanceOf[List[CaseDef]],
                       transform(finalizer, mkContext(ctx, false)))

        case Throw(expr) => super.transform(tree)
        case New(tpt) => super.transform(tree)
        case Typed(expr, tpt) => super.transform(tree)

        case Apply(tapply @ TypeApply(fun, targs), vargs) =>
          lazy val defaultTree = treeCopy.Apply(tree, tapply, transformTrees(vargs, mkContext(ctx, false)))
          if ( ctx.currentMethod.isEffectivelyFinal &&
               ctx.tailPos &&
               isSameTypes(ctx.tparams, targs map (_.tpe.typeSymbol)) &&
               isRecursiveCall(fun)) {
            fun match {
              case Select(receiver, _) =>
                val recTpe = receiver.tpe.widen
                val enclTpe = ctx.currentMethod.enclClass.typeOfThis
                // make sure the type of 'this' doesn't change through this polymorphic recursive call
                if (!forMSIL &&
                    (receiver.tpe.typeParams.isEmpty ||
                      (receiver.tpe.widen == ctx.currentMethod.enclClass.typeOfThis)))
                  rewriteTailCall(fun, receiver :: transformTrees(vargs, mkContext(ctx, false)))
                else
                  defaultTree
              case _ => rewriteTailCall(fun, This(currentClass) :: transformTrees(vargs, mkContext(ctx, false)))
            }
          } else
            defaultTree

        case TypeApply(fun, args) =>
          super.transform(tree)

        case Apply(fun, args) if (fun.symbol == definitions.Boolean_or ||
                                  fun.symbol == definitions.Boolean_and) =>
          treeCopy.Apply(tree, fun, transformTrees(args))

        case Apply(fun, args) =>
          lazy val defaultTree = treeCopy.Apply(tree, fun, transformTrees(args, mkContext(ctx, false)))
          if (ctx.currentMethod.isEffectivelyFinal &&
              ctx.tailPos &&
              isRecursiveCall(fun)) {
            fun match {
              case Select(receiver, _) =>
                if (!forMSIL)
                  rewriteTailCall(fun, receiver :: transformTrees(args, mkContext(ctx, false)))
                else
                  defaultTree
              case _ => rewriteTailCall(fun, This(currentClass) :: transformTrees(args, mkContext(ctx, false)))
            }
          } else
            defaultTree


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
                      (fun.pos))
      ctx.accessed = true
      //println("fun: " + fun + " args: " + args)
      val t = atPos(fun.pos)(Apply(Ident(ctx.label), args))
      // println("TAIL: "+t)
      typed(t)
    }

    private def isSameTypes(ts1: List[Symbol], ts2: List[Symbol]) = ts1 sameElements ts2

    /** Returns <code>true</code> if the fun tree refers to the same method as
     *  the one saved in <code>ctx</code>.
     *
     *  @param fun the expression that is applied
     *  @return    <code>true</code> if the tree symbol refers to the innermost
     *             enclosing method
     */
    private def isRecursiveCall(fun: Tree): Boolean =
      (fun.symbol eq ctx.currentMethod)
  }

}
