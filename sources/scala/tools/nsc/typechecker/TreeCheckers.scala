/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.typechecker;

import scala.tools.util.Position;

abstract class TreeCheckers extends Analyzer {

  import global._;

  def checkTrees: unit = {
    System.out.println("[consistency check at start of phase " + phase + "]");
    for (val unit <- units) check(unit);
  }

  def check(unit: CompilationUnit): unit = {
    val curPrompt = reporter.prompt();
    reporter.prompt(true);
    val context = startContext.make(unit);
    context.checking = true;
    new TreeChecker(context).transformExpr(unit.body);
    reporter.prompt(curPrompt);
  }

  override def newTyper(context: Context): Typer = new TreeChecker(context);

  class TreeChecker(context0: Context) extends Typer(context0) {

    import infer._;

    override def transform(tree: Tree, mode: int, pt: Type): Tree = {
      System.out.println("**** checking " + tree);//debug
      if (tree.pos == Position.NOPOS)
	error(tree.pos, "tree without position: " + tree)
      else if (phase.id > typeCheckPhase.id)
        if (tree.tpe == null)
	  error(tree.pos, "tree without type: " + tree);
        else {
          val oldtpe = tree.tpe;
          tree.tpe = null;
          val newtree = super.transform(tree, mode, pt);
          if (newtree ne tree)
            error(tree.pos, "trees differ\n old: " + tree + "\n new: " + newtree);
          else if (!(oldtpe =:= newtree.tpe))
            error(tree.pos, "types differ\n old: " + oldtpe + "\n new: " + newtree.tpe);
          tree.tpe = oldtpe
        }
      tree
    }
  }
}
