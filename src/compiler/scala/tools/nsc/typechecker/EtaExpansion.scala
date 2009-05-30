/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.typechecker

import scala.collection.mutable.ListBuffer
import symtab.Flags._

/** This trait ...
 *
 *  @author  Martin Odersky
 *  @version 1.0
 */
trait EtaExpansion { self: Analyzer =>

  import global._

  object etaExpansion {
    def unapply(tree: Tree): Option[(List[ValDef], Tree, List[Tree])] = tree match {
      case Function(vparams, Apply(fn, args))
      if (List.forall2(vparams, args) {
        case (vparam, Ident(name)) => vparam.name == name
        case _ => false
      }) =>
        Some((vparams, fn, args))
      case _ =>
        None
    }
  }

  /** <p>
   *    Expand partial function applications of type <code>type</code>.
   *  </p><pre>
   *  p.f(es_1)...(es_n)
   *     ==>  {
   *            <b>private synthetic val</b> eta$f   = p.f   // if p is not stable
   *            ...
   *            <b>private synthetic val</b> eta$e_i = e_i    // if e_i is not stable
   *            ...
   *            (ps_1 => ... => ps_m => eta$f([es_1])...([es_m])(ps_1)...(ps_m))
   *          }</pre>
   *  <p>
   *    tree is already attributed
   *  </p>
   */
  def etaExpand(unit : CompilationUnit, tree: Tree): Tree = {
    val tpe = tree.tpe
    val symbolHash = ""
    var cnt = 0 // for NoPosition
    def freshName(pos : util.Position, n : Int) = {
      cnt += 1
      newTermName(unit.fresh.newName(pos, "eta$" + (cnt - 1) + "$"))
      // Note - the comment below made more sense before I ripped inIDE out -
      // I leave it in to give context to the todo: at the bottom.
      // Martin to Sean: I removed the
      // else if (n == 0) branch and changed `n' in the line above to `(cnt - 1)'
      // this was necessary because otherwise curried eta-expansions would get the same
      // symbol. An example which failes test/files/run/Course-2002-02.scala
      // todo: review and get rid of the `n' argument (which is unused right now).
    }
    // { cnt = cnt + 1; newTermName("eta$" + cnt) }
    val defs = new ListBuffer[Tree]

    /** Append to <code>defs</code> value definitions for all non-stable
     *  subexpressions of the function application <code>tree</code>.
     *
     *  @param tree ...
     *  @return     ...
     */
    def liftoutPrefix(tree: Tree): Tree = {
      def liftout(tree: Tree): Tree =
        if (treeInfo.isPureExpr(tree)) tree
        else {
          val vname: Name = freshName(tree.pos, 0)
          defs += atPos(tree.pos)(ValDef(Modifiers(SYNTHETIC), vname, TypeTree(), tree))
          Ident(vname) setPos tree.pos
        }
      tree match {
        case Apply(fn, args) =>
          treeCopy.Apply(tree, liftoutPrefix(fn), List.mapConserve(args)(liftout)) setType null
        case TypeApply(fn, args) =>
          treeCopy.TypeApply(tree, liftoutPrefix(fn), args) setType null
        case Select(qual, name) =>
          treeCopy.Select(tree, liftout(qual), name) setSymbol NoSymbol setType null
        case Ident(name) =>
          tree
      }
    }

    /** Eta-expand lifted tree.
     *
     *  @param tree ...
     *  @param tpe  ...
     *  @return     ...
     */
    def expand(tree: Tree, tpe: Type): Tree = tpe match {
      case mt: ImplicitMethodType =>
        tree
      case MethodType(paramSyms, restpe) =>
        var cnt0 = 0
        def cnt = {
          cnt0 += 1
          cnt0 - 1
        }
        val params = paramSyms map (sym =>
          ValDef(Modifiers(SYNTHETIC | PARAM), sym.name, TypeTree(sym.tpe), EmptyTree))
        atPos(tree.pos)(Function(params, expand(Apply(tree, params map gen.paramToArg), restpe)))
        //atPos(tree.pos)(Function(params, expand(Apply(tree, args), restpe)))
      case _ =>
        tree
    }

    val tree1 = liftoutPrefix(tree)
    atPos(tree.pos)(Block(defs.toList, expand(tree1, tpe)))
  }
}
