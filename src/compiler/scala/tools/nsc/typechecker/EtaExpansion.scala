/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
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
trait EtaExpansion requires Analyzer {

  import global._
  import posAssigner.atPos

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
  def etaExpand(tree: Tree): Tree = {
    val tpe = tree.tpe
    var cnt = 0
    def freshName() = { cnt = cnt + 1; newTermName("eta$" + cnt) }
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
          val vname: Name = freshName()
          defs += atPos(tree.pos)(ValDef(Modifiers(SYNTHETIC), vname, TypeTree(), tree))
          Ident(vname) setPos tree.pos
        }
      tree match {
        case Apply(fn, args) =>
          copy.Apply(tree, liftoutPrefix(fn), List.mapConserve(args)(liftout)) setType null
        case TypeApply(fn, args) =>
          copy.TypeApply(tree, liftoutPrefix(fn), args) setType null
        case Select(qual, name) =>
          copy.Select(tree, liftout(qual), name) setSymbol NoSymbol setType null
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
      case MethodType(formals, restpe) =>
        val params = formals map (formal =>
          ValDef(Modifiers(SYNTHETIC | PARAM), freshName(), TypeTree()
            .setType(formal), EmptyTree))
        val args = params map (param => Ident(param.name))
        atPos(tree.pos)(Function(params, expand(Apply(tree, args), restpe)))
      case _ =>
        tree
    }

    val tree1 = liftoutPrefix(tree)
    atPos(tree.pos)(Block(defs.toList, expand(tree1, tpe)))
  }
}
