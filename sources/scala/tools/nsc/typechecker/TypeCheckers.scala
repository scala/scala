/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.typechecker;

import scala.tools.util.Position;

/** Methods to create symbols and to enter them into scopes. */
trait TypeCheckers: Analyzer {
  import symtab.Flags._;
  import global._;

  class TypeCheckPhase(prev: Phase) extends StdPhase(prev) {
    def name = "typechecker";
    val global: TypeCheckers.this.global.type = TypeCheckers.this.global;
    def apply(unit: CompilationUnit): unit =
      unit.body = new TypeChecker(startContext.make(unit)).transformTrees(unit.body);
  }

  class TypeChecker(context: Context) extends Transformer {

    import global._;
    import context.unit;

    def error(pos: int, msg: String) = unit.error(pos, msg);

    def reportTypeError(pos: int, ex: TypeError): unit = {
      if (settings.debug.value) ex.printStackTrace();
      ex match {
	case CyclicReference(sym, info: TypeCompleter) =>
	  info.tree match {
	    case ValDef(_, _, EmptyTypeTree(), _) =>
	      error(pos, "recursive " + sym + " needs type")
	    case DefDef(_, _, _, _, EmptyTypeTree(), _) =>
	      error(pos, "recursive " + sym + " needs result type")
	    case _ =>
	      error(pos, ex.getMessage())
	  }
	case _ =>
	  error(pos, ex.getMessage())
      }
    }

    def reenterValueParams(tree: Tree): unit = tree match {
      case DefDef(_, _, _, vparamss, _, _) =>
	for (val vparams <- vparamss; val vparam <- vparams) context.scope enter vparam.symbol
    }

    def checkStable(tree: Tree): Tree = tree;
    def checkNoEscape(pos: int, tpe: Type): Type = tpe;

    def transformConstr(tree: Tree): Tree = tree;
    def transformExpr(tree: Tree): Tree = tree;
    def transformType(tree: Tree): Tree = tree;

    abstract class ResolveError extends TermSymbol(NoSymbol, Position.NOPOS, nme.EMPTY) {
      setFlag(IS_ERROR);
      def msg(qual: Tree, name: Name): String;
    }

    object NotFoundError extends ResolveError {
      def msg(qual: Tree, name: Name) = name.decode + " is not a member of " + qual;
    }

    object NotAccessibleError extends TermSymbol(NoSymbol, Position.NOPOS, nme.EMPTY) {
      def msg(qual: Tree, name: Name) = name.decode + " is not accessible in " + qual;
    }

    def transform(tree: Tree, mode: int, pt: Type): Tree = {

      def stabilize(tree: Tree, pre: Type): Tree = tree;

      /** Attribute an identifier consisting of a simple name or an outer reference.
       *  @param tree      The tree representing the identifier.
       *  @param name      The name of the identifier.
       *  Transformations: (1) Prefix class members with this.
       *                   (2) Change imported symbols to selections
       */
      def transformIdent(name: Name): Tree = {
	def ambiguousError(msg: String) =
	  unit.error(tree.pos, "reference to " + name + " is ambiguous;\n" + msg);
	def importTree(imp: Symbol) =
	  imp.info.asInstanceOf[ImportType].tree;
	def importedSymbol(imp: Symbol) =
	  treeInfo.importedSymbol(importTree(imp), name);
	def isExplicitImport(imp: Symbol) =
	  treeInfo.isExplicitImport(importTree(imp), name);
	//System.out.println("transforming " + name);//DEBUG

	// Compute values of the following 4 variables,
	// except that `pre' is only set for directly inherited symbols.
	// Also detect double imports.
	var sym: Symbol = NoSymbol;  // the directly found symbol
	var sym1: Symbol = NoSymbol; // the imported symbol
	var pre: Type = null;             // if symbols are class members, their prefix type
	var impSym = NoSymbol;            // if symbols are imported, the import symbol from
					  // which they were imported.
	var cx: Context = context;
	while (sym == NoSymbol && cx != NoContext) {
	  val symEntry = cx.scope.lookupEntry(name);
	  var impEntry = cx.scope.lookupEntry(nme.IMPORT);
	  while (impEntry != null) {
	    var impSym1 = impEntry.sym;
	    impEntry = cx.scope.lookupNextEntry(impEntry);
	    sym1 = importedSymbol(impSym1);
	    if (sym1 != NoSymbol) { // check whether shadowed by sym, reset to NoSymbol if yes.
	      if (symEntry != null) {
		var impEntry1 = symEntry.owner.lookupEntry(nme.IMPORT);
		while (impEntry1 != null && sym1 != NoSymbol) {
		  if (impEntry1.sym == impSym1) sym1 = NoSymbol;
		  // directly found takes precedence
		  impEntry1 = symEntry.owner.lookupNextEntry(impEntry1);
		}
	      }
	    }
	    if (sym1 != NoSymbol) { // check for ambiguous imports
	      var impSym2: Symbol = NoSymbol;   // alternative import symbol
	      var sym2: Symbol = NoSymbol;      // alternative imported symbol
	      def ambiguousImportError = ambiguousError(
		"it is imported twice in the same scope by\n" +
		importTree(impSym1) +  "\nand " + importTree(impSym2));
	      while (impEntry != null) {
		impSym2 = impEntry.sym;
		impEntry = cx.scope.lookupNextEntry(impEntry);
		if (impSym2.owner == impSym1.owner) {
		  sym2 = importedSymbol(impSym2);
		  if (sym2 != NoSymbol) {
		    if (isExplicitImport(impSym2)) {
		      if (isExplicitImport(impSym1)) ambiguousImportError;
		      sym1 = sym2;
		      impSym1 = impSym2;
		    }
		    if (isExplicitImport(impSym1)) sym2 = NoSymbol;
		  }
		}
	      }
	      if (sym2 != NoSymbol) ambiguousImportError
	    }
	  }
	  if (symEntry != null) sym = symEntry.sym;
	  cx = cx.enclClass;
	  if (cx != NoContext) {
	    pre = cx.owner.thisType;
	    if (sym == NoSymbol) sym = pre.member(name);
	    cx = cx.outer;
	  }
	}

	// detect ambiguous definition/import,
	// update `sym' to be the final resolved symbol,
	// update `pre' to be `sym's prefix type in case it is a class member,
	// and compute value of:
	var qual: Tree = EmptyTree;   // the qualififier tree if transformed tree is a select
	if (sym != NoSymbol) {
	  if (sym1 != NoSymbol)
	    ambiguousError(
	      "it is both defined in " + sym.owner +
	      " and imported subsequently by \n" + importTree(impSym));
	  else if (sym.owner.isClass && !sym.owner.isPackageClass)
	    qual = gen.This(tree.pos, pre.symbol);
	} else {
	  if (sym1 != NoSymbol) {
	    sym = sym1;
	    qual = importTree(impSym).expr.duplicate;
	    pre = qual.tpe;
	  } else {
	    error(tree.pos, "not found: " + name.decode);
	    sym = context.owner.newErrorSymbol(name);
	  }
	}
	val symtype = pre.memberType(sym);
	if (symtype == NoType) error(tree.pos, "not found: " + name.decode);
	if (qual == EmptyTree) stabilize(tree.setSymbol(sym).setType(symtype), pre)
	else transform(Select(qual, name) setPos tree.pos, mode, pt)
      }
      tree//for now
    }
  }
}

/*
      def resolveSymbol(pos: int, qual: Tree, name: Name): Symbol = {
	def chekFeasible(sym: Symbol): Symbol = {


	var best: Symbol = notFoundError;
	var multiple = false;
	var alts = pre.lookupAll(name);
	while (alts.hasNext) {
	  val alt = checkFeasible(alts.next);
	  if (!improves(best, alt)) {
	    multiple = !best.isError;
	    best = alt;
	  }
	}
	if (multiple) {
	  alts = pre.lookupAll(name);
	  while (alts.hasNext) {
	    val alt = checkFeasible(alts.next);
	    if (!improves(best, alt)) {
	      error(pos,
		    "ambiguous reference to overloaded definition,\n" +
		    "both " + sym1 + ": " + pre.memberType(sym1) + "\n" +
		    "and  " + sym2 + ": " + pre.memberTyoe(sym2) + "\nmatch" +
		    context.resolveContext);
	    }
	  }
	}
	best match {
	  case err: ResolveError => error(pos, err.msg);
	  case _ => best
	}
	}

      /** Attribute a selection where `tree' is `qual.name'.
       *  `qual' is already attributed.
       */
      def transformSelect(tree: Tree, qual0: Tree, name: Name): Tree = {
	val qual = qual0;
	var uninst: List[Symbol] = List();
	qual.tpe match {
	  case PolyType(tparams, restpe) =>
	    qual = mkTypeApply(qual, tparams, restype, tparams map (.tpe));
	    uninst = tparams;
	  case _ =>
	}
	val sym = resolveSymbol(qual, name);
	// if (sym == NoSymbol) try to insert view.
	if (!uninst.isEmpty) {
	  def polymorphize(tp: Type): Type = tp match {
            case PolyType(tparams, restpe) => PolyType(tparams, polymorphize(restpe))
            case _ => PolyType(uninst, tp)
	  }
	  symtype = polymorphize(symtype);
	}
	//System.out.println(qual.getType() + ".member: " + sym + ":" + symtype);//DEBUG
	val tree1: Tree = tree match {
	  case Select(_, _) => copy.Select(tree, sym, qua);
	  case SelectFromType(_, _) => copy.SelectFromType(tree, sym, qual)
	}
 	mkStable(tree1.setType(symtype), qualtype)
      }

*/
