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
      new Namer(startContext.make(unit)).enterSym(unit.body);
  }

  class Namer(context: Context) {

    val typer = {
      def isTemplateContext(context: Context): boolean = context.tree match {
	case Template(_, _) => true
	case Import(_, _) => isTemplateContext(context.outer)
	case _ => false
      }
      new Typer(
	if (isTemplateContext(context)) context.make(context.tree, context.owner, new Scope())
	else context)
    }

    private def doubleDefError(pos: int, sym: Symbol): unit =
      context.error(pos,
        sym.name.toString() + " is already defined as " +
        (if (sym.hasFlag(CASE)) "case class " + sym.name else sym.toString()));

    private def updatePosFlags(sym: Symbol, pos: int, mods: int): Symbol = {
      if (settings.debug.value) System.out.println("overwriting " + sym);
      sym.pos = pos;
      val oldflags = sym.flags & (INITIALIZED | LOCKED);
      val newflags = mods & ~(INITIALIZED | LOCKED);
      sym.flags = oldflags | newflags;
      if (sym.isModule)
        updatePosFlags(sym.moduleClass, pos, (mods & ModuleToClassFlags) | MODULE | FINAL);
      sym
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
        val pkg = context.owner.newPackage(pos, name);
        pkg.moduleClass.setInfo(new PackageClassInfoType(new Scope(), pkg));
        pkg.setInfo(pkg.moduleClass.tpe);
        enterInScope(pkg)
      }
    }

    private def enterClassSymbol(pos: int, mods: int, name: Name): Symbol = {
      val c: Symbol = context.scope.lookup(name);
      if (c.isType && c.isExternal && context.scope == c.owner.info.decls) {
        updatePosFlags(c, pos, mods)
      } else {
	enterInScope(context.owner.newClass(pos, name).setFlag(mods))
      }
    }

    private def enterModuleSymbol(pos: int, mods: int, name: Name): Symbol = {
      val m: Symbol = context.scope.lookup(name);
      if (m.isModule && !m.isPackage && m.isExternal && (context.scope == m.owner.info.decls)) {
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
      if (m.isModule && !m.isPackage && m.isExternal && context.scope == m.owner.info.decls) {
        m.resetFlag(MODULE);
        updatePosFlags(m, pos, mods)
      } else {
        enterInScope(context.owner.newMethod(pos, name).setFlag(mods))
      }
    }

    def enterSyms(trees: List[Tree]): Namer =
      (this /: trees) ((namer, tree) => namer.enterSym(tree));

    def enterSym(tree: Tree): Namer = {

      def finishWith(tparams: List[AbsTypeDef]): unit = {
        if (settings.debug.value) log("entered " + tree.symbol);
	var ltype: LazyType = typer.typeCompleter(tree);
        if (!tparams.isEmpty) {
	  new Namer(context.makeNewScope(tree, tree.symbol)).enterSyms(tparams);
	  ltype = new LazyPolyType(tparams map (.symbol), ltype);
	}
	tree.symbol.setInfo(ltype);
      }
      def finish = finishWith(List());

      if (tree.symbol == null || tree.symbol == NoSymbol) {
	val owner = context.owner;
	tree match {
	  case PackageDef(name, stats) =>
	    tree.symbol = enterPackageSymbol(tree.pos, name);
	    val namer = new Namer(
	      context.make(tree, tree.symbol.moduleClass, tree.symbol.info.decls));
	    namer.enterSyms(stats);
	  case ClassDef(mods, name, tparams, _, _) =>
	    if ((mods & (CASE | ABSTRACT)) == CASE) { // enter case factory method.
	      tree.symbol = enterCaseFactorySymbol(
		tree.pos, mods & AccessFlags | CASE, name.toTermName)
		setInfo typer.caseFactoryCompleter(tree)
	    }
	    tree.symbol = enterClassSymbol(tree.pos, mods, name);
	    if (settings.debug.value) System.out.println("entered: " + tree.symbol + flagsToString(tree.symbol.flags));//debug
	    finishWith(tparams)
	  case ModuleDef(mods, name, _) =>
	    tree.symbol = enterModuleSymbol(tree.pos, mods | MODULE | FINAL, name);
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
	        val setter = owner.newMethod(tree.pos, nme.SETTER_NAME(name))
		  .setFlag(accmods).setInfo(typer.setterTypeCompleter(tree));
	        enterInScope(setter)
	      }
	      tree.symbol =
	        if ((mods & DEFERRED) == 0)
		  owner.newValue(tree.pos, name)
	            .setFlag(mods & FieldFlags | PRIVATE | LOCAL).setInfo(typer.typeCompleter(tree))
	        else getter;
            } else {
              tree.symbol =
                enterInScope(owner.newValue(tree.pos, name).setFlag(mods));
	      finish
            }
	  case DefDef(mods, nme.CONSTRUCTOR, tparams, vparams, tp, rhs) =>
	    tree.symbol = enterInScope(owner.newConstructor(tree.pos))
	      .setFlag(mods | owner.getFlag(ConstrFlags));
	    finishWith(tparams)
	  case DefDef(mods, name, tparams, _, _, _) =>
	    tree.symbol = enterInScope(owner.newMethod(tree.pos, name)).setFlag(mods);
	    finishWith(tparams)
	  case AbsTypeDef(mods, name, _, _) =>
	    tree.symbol = enterInScope(owner.newAbstractType(tree.pos, name)).setFlag(mods);
	    finish
	  case AliasTypeDef(mods, name, tparams, _) =>
	    tree.symbol = enterInScope(owner.newAliasType(tree.pos, name)).setFlag(mods);
	    finishWith(tparams)
	  case Attributed(_, defn) =>
	    enterSym(defn)
	  case DocDef(_, defn) =>
	    enterSym(defn)
	  case imp @ Import(_, _) =>
	    tree.symbol = NoSymbol.newImport(tree.pos).setInfo(typer.typeCompleter(tree));
	    return new Namer(context.makeNewImport(imp));
	  case _ =>
	}
      }
      this
    }
  }
}

