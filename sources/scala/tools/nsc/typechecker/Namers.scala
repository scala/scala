/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.typechecker;

import scala.tools.util.Position;
import symtab.Flags._;

/** Methods to create symbols and to enter them into scopes. */
trait Namers: Analyzer {
  import global._;

  class NamerPhase(prev: Phase) extends StdPhase(prev) {
    def name = "namer";
    val global: Namers.this.global.type = Namers.this.global;
    def apply(unit: CompilationUnit): unit =
      new Namer(startContext.make(unit)).enterSyms(unit.body);
  }

  class Namer(context: Context) {

    val typer = new Typer(context);

    private def doubleDefError(pos: int, sym: Symbol): unit =
      context.unit.error(pos,
        sym.name.toString() + " is already defined as " +
        (if ((sym.rawflags & CASE) != 0) "case class " + sym.name else sym.toString()));

    private def updatePosFlags(sym: Symbol, pos: int, mods: int): Symbol = {
      val sym1 = if (sym.isExternal && !sym.isPackage) sym
                 else { doubleDefError(pos, sym); sym.cloneSymbol }
      sym1.pos = pos;
      val oldflags = sym1.rawflags & (INITIALIZED | LOCKED);
      val newflags = mods & ~(INITIALIZED | LOCKED);
      sym1.rawflags = oldflags | newflags;
      if (sym1.isModule)
        updatePosFlags(sym1.moduleClass, pos, (mods & MODULE2CLASSFLAGS) | MODULE | FINAL);
      sym1
    }

    def enterInScope(sym: Symbol): Symbol = {
      if (!(sym.isMethod && sym.owner.isClass)) {
	val prev = context.scope.lookupEntry(sym.name);
	if (prev != null && prev.owner == context.scope && !prev.sym.isMethod)
	  doubleDefError(sym.pos, prev.sym);
      }
      context.scope enter sym;
      sym
    }

    private def enterPackageSymbol(pos: int, name: Name): Symbol = {
      val p: Symbol = context.scope.lookup(name);
      if (p.isPackage && context.scope == p.owner.info.decls) {
        p.pos = pos; p.moduleClass.pos = pos; p
      } else {
        enterInScope(context.owner.newPackage(pos, name))
      }
    }

    private def enterClassSymbol(pos: int, mods: int, name: Name): Symbol = {
      val c: Symbol = context.scope.lookup(name);
      if (c.isType && context.scope == c.owner.info.decls) {
        updatePosFlags(c, pos, mods)
      } else {
	enterInScope(context.owner.newClass(pos, name).setFlag(mods))
      }
    }

    private def enterModuleSymbol(pos: int, mods: int, name: Name): Symbol = {
      val m: Symbol = context.scope.lookup(name);
      if (m.isModule && context.scope == m.owner.info.decls) {
        updatePosFlags(m, pos, mods)
      } else {
        val newm = context.owner.newModule(pos, name);
        newm.setFlag(mods);
        newm.moduleClass.setFlag(mods);
	enterInScope(newm)
      }
    }

    private def enterCaseFactorySymbol(pos: int, mods: int, name: Name): Symbol = {
      val m: Symbol = context.scope.lookup(name);
      if (m.isModule && context.scope == m.owner.info.decls) {
        updatePosFlags(m, pos, mods)
      } else {
        enterInScope(context.owner.newMethod(pos, name).setFlag(mods))
      }
    }

    def enterSyms(trees: List[Tree]): unit =
      for (val tree <- trees) enterSym(tree);

    def enterSym(tree: Tree): Symbol = {

      def finishWith(tparams: List[AbsTypeDef]) = {
	val ltype = typer.typeCompleter(tree);
	def makeParam(tparam: AbsTypeDef): Symbol =
	  tree.symbol.newTypeParameter(tparam.pos, tparam.name);
	tree.symbol.setInfo(
	  if (tparams.isEmpty) ltype
	  else new LazyPolyType(tparams map makeParam, ltype))
      }
      def finish = finishWith(List());

      if (tree.symbol != null) tree.symbol
      else {
	val owner = context.owner;
	tree match {
	  case PackageDef(name, stats) =>
	    tree.symbol = enterPackageSymbol(tree.pos, name);
	    val namer = new Namer(
	      context.make(tree, tree.symbol.moduleClass, tree.symbol.info.decls));
	    stats map namer.enterSym;
            tree.symbol
	  case ClassDef(mods, name, tparams, _, _) =>
	    if ((mods & (CASE | ABSTRACT)) == CASE) { // enter case factory method.
	      tree.symbol = enterCaseFactorySymbol(
		tree.pos, mods & ACCESSFLAGS | CASE, name.toTermName);
	      finishWith(tparams);
	    }
	    tree.symbol = enterClassSymbol(tree.pos, mods, name);
	    val constr = tree.symbol.newConstructor(tree.pos)
	      .setFlag(tree.symbol.rawflags & CONSTRFLAGS)
	      .setInfo(typer.typeCompleter(tree));
	    tree.symbol.info.decls enter constr;
	    finishWith(tparams)
	  case ModuleDef(mods, name, _) =>
	    tree.symbol = enterModuleSymbol(tree.pos, mods, name);
	    tree.symbol.moduleClass.setInfo(typer.typeCompleter(tree));
	    finish
	  case ValDef(mods, name, tp, rhs) =>
            if (context.owner.isClass & (mods & PRIVATE) == 0) {
	      val accmods = ACCESSOR | (if ((mods & MUTABLE) != 0) mods & ~MUTABLE
                                        else mods | STABLE);
	      val getter = owner.newMethod(tree.pos, name)
	        .setFlag(accmods).setInfo(typer.getterTypeCompleter(tree));
	      enterInScope(getter);
	      if ((mods & MUTABLE) != 0) {
	        val setter = owner.newMethod(tree.pos, name)
		  .setFlag(accmods).setInfo(typer.setterTypeCompleter(tree));
	        enterInScope(setter)
	      }
	      tree.symbol =
	        if ((mods & DEFERRED) == 0)
		  owner.newValue(tree.pos, name)
	            .setFlag(mods | PRIVATE | LOCAL).setInfo(typer.typeCompleter(tree))
	        else getter;
              tree.symbol
            } else {
              tree.symbol =
                enterInScope(owner.newValue(tree.pos, name).setFlag(mods));
	      finish
            }
	  case DefDef(mods, nme.CONSTRUCTOR, tparams, vparams, tp, rhs) =>
	    if (!(owner.isClass && context.scope == owner.info.decls) ||
		owner.isModuleClass || owner.isAnonymousClass || owner.isRefinementClass)
	      context.unit.error(tree.pos, "constructor definition not allowed here");
	    tree.symbol = enterInScope(owner.newConstructor(tree.pos))
	      .setFlag(mods | owner.rawflags & CONSTRFLAGS);
	    finishWith(tparams)
	  case DefDef(mods, name, tparams, _, _, _) =>
	    tree.symbol = enterInScope(owner.newMethod(tree.pos, name)).setFlag(mods);
	    finishWith(tparams)
	  case AbsTypeDef(mods, name, _, _) =>
	    tree.symbol = enterInScope(owner.newAbstractType(tree.pos, name));
	    finish
	  case AliasTypeDef(mods, name, tparams, _) =>
	    tree.symbol = enterInScope(owner.newAliasType(tree.pos, name));
	    finishWith(tparams)
	  case Attributed(_, defn) =>
	    enterSym(defn)
	  case DocDef(_, defn) =>
	    enterSym(defn)
	  case Import(expr, selectors) =>
	    tree.symbol = NoSymbol.newImport(tree.pos);
	    finish
	  case _ =>
            tree.symbol
	}
      }
    }
  }
}

