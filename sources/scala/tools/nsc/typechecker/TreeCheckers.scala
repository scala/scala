/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.typechecker;

import scala.tools.util.Position;

abstract class TreeCheckers extends Analyzer {

  import global._;

  val tpeOfTree = new scala.collection.mutable.HashMap[Tree, Type];

  def checkTrees: unit = {
    System.out.println("[consistency check at start of phase " + phase + "]");
    for (val unit <- units) check(unit);
  }

  def check(unit: CompilationUnit): unit = {
    val curPrompt = reporter.prompt();
    reporter.prompt(true);
    val context = startContext.make(unit);
    context.checking = true;
    tpeOfTree.clear;
    val checker = new TreeChecker(context);
    checker.precheck.traverse(unit.body);
    checker.typed(unit.body);
    checker.postcheck.traverse(unit.body);
    reporter.prompt(curPrompt);
  }

  override def newTyper(context: Context): Typer = new TreeChecker(context);

  class TreeChecker(context0: Context) extends Typer(context0) {

    import infer._;

    override def typed(tree: Tree, mode: int, pt: Type): Tree = {
      //System.out.println("**** checking " + tree);//debug
      tree match {
	case EmptyTree | TypeTree() =>
	  ;
	case _ =>
	  if (!tpeOfTree.contains(tree)) {
	    tpeOfTree.update(tree, tree.tpe);
            tree.tpe = null
	  }
          val newtree = super.typed(tree, mode, pt);
          if (newtree ne tree)
            error(tree.pos, "trees differ\n old: " + tree + " [" + tree.getClass() + "]\n new: " +
		  newtree + " [" + newtree.getClass() + "]");
      }
      tree
    }
    override def typed(tree: Tree) = super.typed(tree); // doto remove for new compiler

    object precheck extends Traverser {
      override def traverse(tree: Tree): unit = {
	if (tree.pos == Position.NOPOS)
	  error(tree.pos, "tree without position: " + tree)
	else if (tree.tpe == null && phase.id >= typerPhase.id)
	  error(tree.pos, "tree without type: " + tree)
      }
    }

    object postcheck extends Traverser {
      override def traverse(tree: Tree): unit = tree match {
	case EmptyTree | TypeTree() =>
	  ;
	case _ =>
	  tpeOfTree.get(tree) match {
	    case Some(oldtpe) =>
	      if (!(oldtpe =:= tree.tpe))
		error(tree.pos, "types differ\n old: " + oldtpe + "\n new: " + tree.tpe +
		      "\n tree: " + tree);
	      tree.tpe = oldtpe;
	      super.traverse(tree)
	    case None =>
	  }
      }
    }
  }
}
