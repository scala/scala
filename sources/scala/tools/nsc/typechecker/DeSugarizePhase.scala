/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.typechecker;

import symtab.Flags._;
import scala.collection.mutable.ListBuffer;

/** Performs the following context-free rewritings:
 *  (1) Places all pattern variables in Bind nodes. In a pattern, for identifiers `x':
 *                 x  => x @ _
 *               x:T  => x @ (_ : T)
 *
 *  (2) Removes pattern definitions (PatDef's) as follows:
 *      If pattern is a simple (typed) identifier:
 *        val x = e     ==>  val x = e
 *        val x: T = e  ==>  val x: T = e
 *
 *      if there are no variables in pattern
 *        val p = e  ==>  e.match (case p => ())
 *
 *      if there is exactly one variable in pattern
 *        val x_1 = e.match (case p => (x_1))
 *
 *      if there is more than one variable in pattern
 *        val p = e  ==>  private synthetic val t$ = e.match (case p => (x_1, ..., x_N))
 *                        val x_1 = t$._1
 *                        ...
 *                        val x_N = t$._N
 *
 *  (3) Removes function types as follows:
 *        (argtpes) => restpe   ==>   scala.Function_n[argtpes, restpe]
 *
 *  (4) Wraps naked case definitions in a match as follows:
 *        { cases }   ==>   (x => x.match {cases}), except when already argument to match
 */
abstract class DeSugarizePhase(prev: Phase) extends StdPhase(prev) {
  import global._;
  import posAssigner.atPos;

  def name = "desugarize";
  def apply(unit: CompilationUnit): unit =
    unit.body = new DeSugarizer(unit).transformTrees(unit.body);

  class DeSugarizer(unit: CompilationUnit) extends Transformer {

    /** introduce fresh variable of the form "ds$56"
     */
    private def getvar: Name = unit.fresh.newName("ds$");

    /**  in patterns x  => x @ _
     *               x:T  => x @ (_ : T)
     */
    object patvarTransformer extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
	case Ident(name) if (treeInfo.isVariableName(name) && name != nme.WILDCARD) =>
	  atPos(tree.pos)(Bind(name, Ident(nme.WILDCARD)))
	case Typed(id @ Ident(name), tpt) =>
	  Bind(name, atPos(tree.pos)(Typed(Ident(nme.WILDCARD), tpt))) setPos id.pos
	case Apply(fn @ Apply(_, _), args) =>
	  copy.Apply(tree, transform(fn), transformTrees(args))
	case Apply(fn, args) =>
	  copy.Apply(tree, fn, transformTrees(args))
	case Sequence(_) | Alternative(_) | Bind(_, _) | Typed(_, _) =>
	  super.transform(tree)
	case _ =>
	  tree
      }
    }

    /** Traverse pattern and collect all variable names in buffer */
    object getvarTraverser extends Traverser {
      val buf = new ListBuffer[Name];
      def init: Traverser = { buf.clear; this }
      override def traverse(tree: Tree): unit = tree match {
	case Bind(name, tpe) =>
	  if (buf.elements forall (name !=)) buf += name;
	  traverse(tpe)
	case _ => super.traverse(tree)
      }
    }

    /** Returns list of all pattern variables without duplicates */
    private def getVariables(tree: Tree): List[Name] = {
      getvarTraverser.init.traverse(tree);
      getvarTraverser.buf.toList
    }

    /** Expand patdefs */
    private def transformStat(tree: Tree): List[Tree] = {
      def rebox(defs: List[Tree], boxf: Tree => Tree): List[Tree] = defs match {
	case List(stat) => List(if (stat.isDef) boxf(stat) else stat);
	case stat :: stats => stat :: (stats map boxf)
      }
      def mkTuple(trees: List[Tree]): Tree = trees match {
	case List() => Literal(())
	case List(tree) => tree
	case _ => Apply(Select(Ident(nme.scala), newTermName("Tuple" + trees.length)), trees)
      }
      tree match {
	// pattern definitions:
	//  val x = e     ==>  val x = e
	case PatDef(mods, Ident(name), rhs) =>
	  List(atPos(tree.pos)(ValDef(mods, name, EmptyTypeTree(), rhs)))

	//  val (x @ _) = e     ==>  val x = e
	case PatDef(mods, Bind(name, Ident(nme.WILDCARD)), rhs) =>
	  List(atPos(tree.pos)(ValDef(mods, name, EmptyTypeTree(), rhs)))

	//  val x: T = e  ==>  val x: T = e
	case PatDef(mods, Typed(Ident(name), tpt), rhs) =>
	  List(atPos(tree.pos)(ValDef(mods, name, tpt, rhs)))

	//  val x @ (_: T) = e  ==>  val x: T = e
	case PatDef(mods, Bind(name, Typed(Ident(nme.WILDCARD), tpt)), rhs) =>
	  List(atPos(tree.pos)(ValDef(mods, name, tpt, rhs)))

	//  in case there are no variables in pattern
	//  val p = e  ==>  e.match (case p => ())
	//
	//  in case there is exactly one variable in pattern
	//  val x_1 = e.match (case p => (x_1))
	//
	//  in case there are more variables in pattern
	//  val p = e  ==>  private synthetic val t$ = e.match (case p => (x_1, ..., x_N))
	//                  val x_1 = t$._1
	//                  ...
	//                  val x_N = t$._N
	case PatDef(mods, pat, rhs) =>
	  val vars = getVariables(pat);
	  val matchExpr = atPos(pat.pos) {
	    Apply(
	      Select(rhs, nme._match),
	      List(Visitor(List(CaseDef(pat, EmptyTree, mkTuple(vars map Ident))))))
	  }
	  vars match {
	    case List() =>
	      List(matchExpr)
	    case List(vname) =>
	      List(ValDef(mods, vname, EmptyTypeTree(), matchExpr))
	    case _ =>
	      val tmp = getvar;
	      val firstDef = ValDef(PRIVATE | LOCAL | SYNTHETIC,
				  tmp, EmptyTypeTree(), matchExpr);
	      var cnt = 0;
	      val restDefs = for (val v <- vars) yield {
	        cnt = cnt + 1;
		val selector = newTermName("_" + cnt);
		ValDef(mods, v, EmptyTypeTree(), Select(Ident(tmp), selector))
	      }
	      firstDef :: restDefs
	  } map atPos(tree.pos)

	case DocDef(comment, defn) =>
	  rebox(transformStat(defn), t => DocDef(comment, t))

	case Attributed(attribute, defn) =>
	  rebox(transformStat(defn), t => Attributed(attribute, t))

	case _ =>
	  List(tree)
      }
    }

    /** Expand patdefs in list of statements */
    private def transformStats(trees: List[Tree]): List[Tree] = {
      def isPatDef(tree: Tree): boolean = tree match {
	case PatDef(_, _, _) => true
	case DocDef(_, defn) => isPatDef(defn)
	case Attributed(_, defn) => isPatDef(defn)
	case _ => false
      }
      if (trees exists isPatDef) trees flatMap transformStat else trees
    }

    /** desugarize tree */
    override def transform(tree: Tree): Tree = tree match {
      // (argtpes) => restpe   ==>   scala.Function_n[argtpes, restpe]
      case FunctionTypeTree(argtpes, restpe) =>
	atPos(tree.pos) (
	  AppliedTypeTree(
	    Select(Ident(nme.scala), newTypeName("Function" + argtpes.length)),
	    transformTrees(argtpes) ::: List(transform(restpe))))

      // { cases }   ==>   (x => x.match {cases}), except when already argument to match
      case Apply(s @ Select(x, nme._match), List(v @ Visitor(cases))) =>
	copy.Apply(tree,
          copy.Select(s, transform(x), nme._match),
          List(copy.Visitor(v, transformCaseDefs(cases))))

      case Apply(Ident(nme._match), List(v @ Visitor(cases))) =>
	copy.Apply(tree,
          Select(This(nme.EMPTY.toTypeName), nme._match),
          List(copy.Visitor(v, transformCaseDefs(cases))))

      case Visitor(cases) =>
	val x = getvar;
	atPos(tree.pos)(
	  Function(
	    List(ValDef(PARAM, x, EmptyTypeTree(), EmptyTree)),
	    Apply(Select(Ident(x), nme._match), List(super.transform(tree)))))

      case CaseDef(pat, guard, body) =>
	copy.CaseDef(tree, transform(patvarTransformer.transform(pat)),
		     transform(guard), transform(body))

      case PatDef(mods, pat, rhs) =>
	copy.PatDef(tree, mods, transform(patvarTransformer.transform(pat)), transform(rhs))

      case Template(parents, body) =>
	copy.Template(tree, transformTrees(parents), transformStats(transformTrees(body)))

      case Block(stats, expr) =>
	copy.Block(tree, transformStats(transformTrees(stats)), transform(expr))
    }
  }
}
