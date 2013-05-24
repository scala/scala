package scala.reflect.reify
package phases

import scala.collection.{ mutable }

trait Metalevels {
  self: Reifier =>

  import global._

  /**
   *  Makes sense of cross-stage bindings.
   *
   *  ----------------
   *
   *  Analysis of cross-stage bindings becomes convenient if we introduce the notion of metalevels.
   *  Metalevel of a tree is a number that gets incremented every time you reify something and gets decremented when you splice something.
   *  Metalevel of a symbol is equal to the metalevel of its definition.
   *
   *  Example 1. Consider the following snippet:
   *
   *    reify {
   *      val x = 2             // metalevel of symbol x is 1, because it's declared inside reify
   *      val y = reify{x}      // metalevel of symbol y is 1, because it's declared inside reify
   *                            // metalevel of Ident(x) is 2, because it's inside two reifies
   *      y.splice              // metalevel of Ident(y) is 0, because it's inside a designator of a splice
   *    }
   *
   *  Cross-stage bindings are introduced when symbol.metalevel != curr_metalevel.
   *  Both bindings introduced in Example 1 are cross-stage.
   *
   *  Depending on what side of the inequality is greater, the following situations might occur:
   *
   *  1) symbol.metalevel < curr_metalevel. In this case reifier will generate a free variable
   *  that captures both the name of the symbol (to be compiled successfully) and its value (to be run successfully).
   *  For example, x in Example 1 will be reified as follows: Ident(newFreeVar("x", IntTpe, x))
   *
   *  2) symbol.metalevel > curr_metalevel. This leads to a metalevel breach that violates intuitive perception of splicing.
   *  As defined in macro spec, splicing takes a tree and inserts it into another tree - as simple as that.
   *  However, how exactly do we do that in the case of y.splice? In this very scenario we can use dataflow analysis and inline it,
   *  but what if y were a var, and what if it were calculated randomly at runtime?
   *
   *  This question has a genuinely simple answer. Sure, we cannot resolve such splices statically (i.e. during macro expansion of `reify`),
   *  but now we have runtime toolboxes, so noone stops us from picking up that reified tree and evaluating it at runtime
   *  (in fact, this is something that `Expr.splice` does transparently).
   *
   *  This is akin to early vs late binding dilemma.
   *  The prior is faster, plus, the latter (implemented with reflection) might not work because of visibility issues or might be not available on all platforms.
   *  But the latter still has its uses, so I'm allowing metalevel breaches, but introducing the -Xlog-runtime-evals to log them.
   *
   *  upd. We no longer do that. In case of a runaway `splice` inside a `reify`, one will get a static error.
   *  Why? Unfortunately, the cute idea of transparently converting between static and dynamic splices has failed.
   *  1) Runtime eval that services dynamic splices requires scala-compiler.jar, which might not be on library classpath
   *  2) Runtime eval incurs a severe performance penalty, so it'd better to be explicit about it
   *
   *  ----------------
   *
   *  As we can see, the only problem is the fact that lhs'es of `splice` can be code blocks that can capture variables from the outside.
   *  Code inside the lhs of an `splice` is not reified, while the code from the enclosing reify is.
   *
   *  Hence some bindings become cross-stage, which is not bad per se (in fact, some cross-stage bindings have sane semantics, as in the example above).
   *  However this affects freevars, since they are delicate inter-dimensional beings that refer to both current and next planes of existence.
   *  When splicing tears the fabric of the reality apart, some freevars have to go single-dimensional to retain their sanity.
   *
   *  Example 2. Consider the following snippet:
   *
   *    reify {
   *      val x = 2
   *      reify{x}.splice
   *    }
   *
   *  Since the result of the inner reify is wrapped in a splice, it won't be reified
   *  together with the other parts of the outer reify, but will be inserted into that result verbatim.
   *
   *  The inner reify produces an Expr[Int] that wraps Ident(freeVar("x", IntTpe, x)).
   *  However the freevar the reification points to will vanish when the compiler processes the outer reify.
   *  That's why we need to replace that freevar with a regular symbol that will point to reified x.
   *
   *  Example 3. Consider the following fragment:
   *
   *    reify {
   *      val x = 2
   *      val y = reify{x}
   *      y.splice
   *    }
   *
   *  In this case the inner reify doesn't appear next to splice, so it will be reified together with x.
   *  This means that no special processing is needed here.
   *
   *  Example 4. Consider the following fragment:
   *
   *    reify {
   *      val x = 2
   *      {
   *        val y = 2
   *        val z = reify{reify{x + y}}
   *        z.splice
   *      }.splice
   *    }
   *
   *  The reasoning from Example 2 still holds here - we do need to inline the freevar that refers to x.
   *  However, we must not touch anything inside the splice'd block, because it's not getting reified.
   */
  val metalevels = new Transformer {
    var insideSplice = false
    val inlineableBindings = mutable.Map[TermName, Tree]()

    def withinSplice[T](op: => T) = {
      val old = insideSplice
      insideSplice = true
      try op
      finally insideSplice = old
    }

    // Q: here we deal with all sorts of reified trees. what about ReifiedType(_, _, _, _, _, _)?
    // A: nothing. reified trees give us problems because they sometimes create dimensional rifts as described above
    //    to the contrast, reified types (i.e. synthetic typetags materialized by Implicits.scala) always stay on the same metalevel as their enclosing code
    override def transform(tree: Tree): Tree = tree match {
      case TreeSplice(ReifiedTree(universe, mirror, symtab, rtree, tpe, rtpe, concrete)) =>
        if (reifyDebug) println("entering inlineable splice: " + tree)
        val inlinees = symtab.syms filter (_.isLocalToReifee)
        inlinees foreach (inlinee => symtab.symAliases(inlinee) foreach (alias => inlineableBindings(alias) = symtab.symBinding(inlinee)))
        val symtab1 = symtab -- inlinees
        if (reifyDebug) println("trimmed %s inlineable free defs from its symbol table: %s".format(inlinees.length, inlinees map (inlinee => symtab.symName(inlinee)) mkString(", ")))
        withinSplice { super.transform(TreeSplice(ReifiedTree(universe, mirror, symtab1, rtree, tpe, rtpe, concrete))) }
      case TreeSplice(splicee) =>
        if (reifyDebug) println("entering splice: " + splicee)
        val breaches = splicee filter (sub => sub.hasSymbolField && sub.symbol != NoSymbol && sub.symbol.metalevel > 0)
        if (!insideSplice && breaches.nonEmpty) {
          // we used to convert dynamic splices into runtime evals transparently, but we no longer do that
          // why? see comments above
          // if (settings.logRuntimeSplices.value) reporter.echo(tree.pos, "this splice cannot be resolved statically")
          // withinSplice { super.transform(tree) }
          if (reifyDebug) println("metalevel breach in %s: %s".format(tree, (breaches map (_.symbol)).distinct mkString ", "))
          CannotReifyRuntimeSplice(tree)
        } else {
          withinSplice { super.transform(tree) }
        }
      // todo. also inline usages of `inlineableBindings` in the symtab itself
      // e.g. a free$Foo can well use free$x, if Foo is path-dependent w.r.t x
      // FreeRef(_, _) check won't work, because metalevels of symbol table and body are different, hence, freerefs in symbol table look different from freerefs in body
      case FreeRef(_, name) if inlineableBindings contains name =>
        if (reifyDebug) println("inlineable free ref: %s in %s".format(name, showRaw(tree)))
        val inlined = reify(inlineableBindings(name))
        if (reifyDebug) println("verdict: inlined as %s".format(showRaw(inlined)))
        inlined
      case _ =>
        super.transform(tree)
    }
  }
}
