/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.typechecker;

import scala.tools.nsc.util.Position;
import scala.tools.nsc.reporters.AbstractReporter;
import symtab.Flags._;

abstract class TreeCheckers extends Analyzer {

  import global._;

  val tpeOfTree = new scala.collection.mutable.HashMap[Tree, Type];

  def checkTrees: unit = {
    System.out.println("[consistency check at start of phase " + phase + "]");
    for (val unit <- currentRun.units) check(unit);
  }

  def check(unit: CompilationUnit): unit = {
    val areporter = if (reporter.isInstanceOf[AbstractReporter]) reporter.asInstanceOf[AbstractReporter] else null;

    val curPrompt = if (areporter != null) {
      val ret = areporter.prompt;
      areporter.prompt = true;
      ret;
    } else false;

    val context = rootContext(unit);
    context.checking = true;
    tpeOfTree.clear;
    val checker = new TreeChecker(context);
    checker.precheck.traverse(unit.body);
    checker.typed(unit.body);
    checker.postcheck.traverse(unit.body);
    if (areporter != null)
      areporter.prompt = curPrompt;
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
          if ((newtree ne tree) && !newtree.isInstanceOf[Literal])
            error(tree.pos, "trees differ\n old: " + tree + " [" + tree.getClass() + "]\n new: " +
		  newtree + " [" + newtree.getClass() + "]");
      }
      tree
    }
    override def typed(tree: Tree) = super.typed(tree); // doto remove for new compiler

    object precheck extends Traverser {
      override def traverse(tree: Tree): unit =
        try {
          tree match {
            case DefDef(_, _, _, _, _, _) =>
              if (tree.symbol.hasFlag(ACCESSOR) &&
		  !tree.symbol.hasFlag(DEFERRED) &&
		  !tree.symbol.tpe.resultType.isInstanceOf[ConstantType]) {
                assert(tree.symbol.accessed != NoSymbol);
                assert(tree.symbol.accessed.getter(tree.symbol.owner) == tree.symbol ||
                       tree.symbol.accessed.setter(tree.symbol.owner) == tree.symbol);
              }
            case ValDef(_, _, _, _) =>
              if (tree.symbol.hasGetter) {
                assert(tree.symbol.getter(tree.symbol.owner) != NoSymbol)
              }
            case Apply(_, args) =>
              assert(args forall (EmptyTree !=))
            case Select(_, _) =>
              assert(tree.symbol != NoSymbol, tree);
	    case This(_) =>
	      if (!(tree.symbol.isStatic && (tree.symbol hasFlag MODULE))) {
		var o = currentOwner;
		while (o != tree.symbol) {
		  o = o.owner;
		  assert(o != NoSymbol, tree)
		}
	      }
            case _ =>
          }
	  if (tree.pos == Position.NOPOS && tree != EmptyTree) {
	    error(tree.pos, "tree without position: " + tree)
	  } else if (tree.tpe == null && phase.id >= currentRun.typerPhase.id) {
	    error(tree.pos, "tree without type: " + tree);
          } else if (tree.isDef && tree.symbol.owner != currentOwner) {
            var owner = currentOwner;
            while (owner.isTerm && !owner.isMethod && tree.symbol.owner != owner)
              owner = owner.owner;
            if (tree.symbol.owner != owner) {
              error(tree.pos, "" + tree.symbol + " has wrong owner: " + tree.symbol.owner +
                    tree.symbol.owner.locationString + ", should be: " +
                    currentOwner + currentOwner.locationString)
            }
          } else {
            super.traverse(tree)
          }
        } catch {
          case ex: Throwable =>
	    if (settings.debug.value)
	      System.out.println("exception when traversing " + tree);
	    throw(ex)
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
