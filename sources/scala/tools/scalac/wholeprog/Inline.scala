/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2004, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

/*
**       Inline methods in monomoprhic callsites
**
** [iuli]   12.05.2004                                                  */

import scalac.{Global => scalac_Global}
import scalac.{CompilationUnit => scalac_CompilationUnit}
import scalac.symtab._;
import scalac.util._;
import scala.collection.mutable._;
import scalac.ast._;
import scala.tools.scalac.wholeprog.graph.{Node => GNode};
import scala.tools.scalac.wholeprog.graph._;
import scala.tools.util._;

package scala.tools.scalac.wholeprog {



/** Perform inlining of the sites passed as parameter */
class InlineMethods(sites: MonomorphicCallSites#InlinableCollection,
		    global: scalac_Global)
  extends Transformer(global) {
    var inlines: int = 0;
    var inlinedThis: Symbol = null;


  override def transform(tree: Tree): Tree = {
    tree match {
      case Tree$Apply(fun, args) => {

	sites.get(tree) match {
	  case Some(Tuple3(cl, ce, s)) => expand(tree, cl, ce);
	  case _ => super.transform(tree);
	}
      }

      case _ => super.transform(tree);
    }
  }

  def expand(tree: Tree, caller: GNode[Symbol, MethodNode], callee: GNode[Symbol, MethodNode]): Tree = {
    val expr: Tree = null;
    val Tree$DefDef(_, name, _, vparams, _, rhs) = callee.info.code;
    val subst = new HashMap[Symbol, Symbol];

    def createLocals(tree: Tree, calleeDef: Tree): Array[Tree] = {
      val Tree$Apply(fun, args) = tree;
      val Tree$DefDef(_, name, _, vparams, _, rhs) = calleeDef;

      val res: Array[Tree] = new Array[Tree](args.length + 1); // make room for $this
      assert(args.length == vparams(0).length,
	     "Call site has different nr. of arguments than def " + fun.symbol());

      res(0) = makeThisDef(fun);
      var i: int = 1;
      while (i < res.length) {
	// duplicate the formal parameter of the method and create a symbol for this def
	val arg = vparams(0)(i-1).duplicate().asInstanceOf[Tree$ValDef];
	val sym = caller.info.method.newVariable(fun.pos, 0, Name.fromString("$" + i));

	// set the type of the parameter to the type of the *actual* argument
//	sym.setType(args(i-1).getType());
	// or the formals?
	sym.setType(arg.tpe.getType());

//	Console.println("Type: actual " + sym.getType() + " : formal "
//			+ arg);

	arg.tpe.setType(sym.getType());
	// add the mapping to the substitution table of symbols
	subst += arg.symbol() -> sym;

	// set the initial value to the actual parameter
	arg.rhs = args(i - 1).duplicate();
	arg.setSymbol(sym);
	arg.rhs.setType(sym.getType());

	res(i) = arg;
	i = i + 1;
      }

      res
    }

    def makeThisDef(fun: Tree): Tree = {
      val Tree$Select(qualifier, selector) = fun;

//      val tpe = make.TypeTerm(fun.pos);
      val sym = caller.info.method.newVariable(fun.pos, 0, Name.fromString("inthis"));
//      sym.setType(qualifier.getType().singleDeref()); // it was .singleDeref() but unneded?
      sym.setType(callee.info.classz.getType());
      Logger.log("[inthis] Set type to " + sym.getType());

//      val t = make.ValDef(fun.pos, 0, Name.fromString("inthis"), tpe, qualifier.duplicate());
      val t = gen.ValDef(sym, qualifier.duplicate());

//      tpe.setType(qualifier.getType().deconst());
//      t.setSymbol(sym);
      inlinedThis = sym;

      t
    }

    val locals = createLocals(tree, callee.info.code);
    val updater = new UpdateAccesses(subst, caller.info.method);
    val newRhs = updater.transform(rhs.duplicate());

    Logger.log("[inline] expand reached");

    if (updater.touchedPrivate)
      tree
    else {
      Logger.log("Inlining at " +
		 caller.info.classz.name + "." +
		 caller.info.method.name + " [" + Position.toString(tree.pos) + "] with " +
		 callee.info.classz.name + "." +
		 callee.info.method.name);
      inlines = inlines + 1;

      gen.mkBlock(locals, newRhs);
    }
  }

  /** Update accesses to symbols that have been replaced according to the map */
  class UpdateAccesses(subst: HashMap[Symbol, Symbol], hostMethod: Symbol)
    extends GenTransformer(global) {

    var touchedPrivate: boolean = false;

    override def transform(tree: Tree): Tree = {
      tree match {
	case Tree$This(qualifier) => {
	  assert(inlinedThis != null, "<this> is null for " + tree);
	  gen.Ident(tree.pos, inlinedThis);
	}

	// create a new symbol for the new declaration in this method
 	case Tree$ValDef(mods, name, tpe, rhs) => {
	  val newSym = hostMethod.newVariable(tree.pos, mods, name);

	  newSym.setType(tpe.getType());
	  subst += tree.symbol() -> newSym;

	  tree.setSymbol(newSym);

          gen.ValDef(newSym, transform(rhs));
	}

	case Tree$Super(_, _) => {
	  Logger.log("[inline] Touched super.");
	  touchedPrivate = true; // not private, but still we cannot inline this function
	  super.transform(tree);
	}

	case Tree$Return(_) => {
	  Logger.log("[inline] Touched return.");
	  touchedPrivate = true; // not private, but still we cannot inline this function
	  super.transform(tree);
	}

	case Tree$LabelDef(name, params, rhs) => {
	  val newSym = hostMethod.newLabel(tree.pos, name);

 	  newSym.setInfo(tree.symbol().info());
	  subst += tree.symbol() -> newSym;

          gen.LabelDef(newSym, super.transform(params), transform(rhs));
	}

 	case Tree$PatDef(mods, pat, rhs) => {
	  Console.println("new pattern definition in inlined class");
	  tree
	}

 	case Tree$DefDef(_, _, _, _, _, _) => {
	  assert(false, "We should be after lambda lift, so no inner function allowed");
	  tree;
	}

	case _ =>  super.transform(tree);

      }

    }

    override def getSymbolFor(tree: Tree): Symbol = {
      if (!tree.symbol().isPublic() ||
	  (tree.symbol().flags & Modifiers.PACKAGE) > 0 ) { // attention, was isPrivate()
	touchedPrivate = true;
	Logger.log("[inline] touched private symbol " +
		   SymbolPrinter.fullName(tree.symbol()));
      }

      if (subst.contains(tree.symbol())) {
	Logger.log("[inline] Substituted " + tree.symbol() + " with " + subst(tree.symbol()));
	subst(tree.symbol());
      }
      else
	super.getSymbolFor(tree);
	//tree.symbol();
    }

  }
}

object Logger {
  var file: java.io.Writer = null;

  def setFile(name: String): Unit = {
    file = new java.io.FileWriter(name);
  }

  def log(s: String): Unit = {
    file.write(s + "\n");
  }

  def log(s1: String, s2: String): Unit = {
    file.write(s1 + " " + "\n");
  }

  def flush: unit = {
    file.flush();
  }
}

} // package scala.tools.scalac.wholeprog
