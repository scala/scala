/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.typechecker;

import scala.tools.util.Position;

/** Methods to create symbols and to enter them into scopes. */
trait Typers: Analyzer {
  import symtab.Flags;
  import symtab.Flags._;
  import global._;

  abstract class TypeCompleter(val tree: Tree) extends LazyType;

  class Typer(context: Context) {
    import context.unit;

    val typechecker = new TypeChecker(context);

    def typeCompleter(tree: Tree) = new TypeCompleter(tree) {
      override def complete(sym: Symbol): unit = {
	if (settings.debug.value) log("defining " + sym);
	sym.setInfo(typeSig(tree));
        if (settings.debug.value) log("defined " + sym);
        validate(sym);
      }
    }

    def selfTypeCompleter(tree: Tree) = new TypeCompleter(tree) {
      override def complete(sym: Symbol): unit =
	sym.setInfo(typechecker.transformType(tree).tpe)
    }

    private def deconstIfNotFinal(sym: Symbol, tpe: Type): Type =
      if (sym.isVariable || !sym.hasFlag(FINAL)) tpe.deconst else tpe;

    private def enterTypeParams(owner: Symbol, tparams: List[AbsTypeDef]): List[Symbol] = {
      List.map2(owner.typeParams, tparams)
        { (tpsym, tptree) => tptree.symbol = tpsym; context.scope enter tpsym; tpsym }
    }

    private def enterValueParams(owner: Symbol,
				 vparamss: List[List[ValDef]]): List[List[Symbol]] = {
      def enterValueParam(param: ValDef): Symbol = {
	param.symbol = owner.newValueParameter(param.pos, param.name)
	  .setInfo(typeCompleter(param));
        context.scope enter param.symbol;
        param.symbol
      }
      vparamss.map(.map(enterValueParam))
    }

    /** A creator for polytypes. If tparams is empty, simply returns result type */
    private def makePolyType(tparams: List[Symbol], tpe: Type): Type =
      if (tparams.isEmpty) tpe
      else
	PolyType(tparams, tpe match {
	  case PolyType(List(), tpe1) => tpe1
	  case _ => tpe
	});

    private def templateSig(clazz: Symbol, templ: Template): Type = {
      // determine parent types
      val parents =
        if (templ.parents.isEmpty) List()
        else
          typechecker.transformSuperType(templ.parents.head).tpe ::
          (templ.parents.tail map (p => typechecker.transformType(p).tpe));
      if (!parents.isEmpty && parents.head.symbol.hasFlag(INTERFACE)
			   && parents.head.symbol.hasFlag(JAVA))
	unit.error(templ.parents.head.pos, "cannot extend a Java interface");

      // enter all members
      val decls = new Scope();
      new Namer(context.make(templ, clazz, decls)).enterSyms(templ.body);
      ClassInfoType(parents, decls, clazz)
    }

    private def classSig(clazz: Symbol,
			 tparams: List[AbsTypeDef],
			 tpt: Tree, impl: Template): Type = {
      val tparamSyms = enterTypeParams(clazz, tparams);
      if (!tpt.isEmpty) clazz.typeOfThis = selfTypeCompleter(tpt);
      val constrTree = treeInfo.firstConstructor(impl.body);
      val constr = new Namer(context).enterSym(constrTree).initialize;
      typechecker.reenterValueParams(constrTree);
      makePolyType(tparamSyms, templateSig(clazz, impl))
    }

    private def methodSig(meth: Symbol,
			  tparams: List[AbsTypeDef], vparamss: List[List[ValDef]],
			  tpt: Tree, rhs: Tree): Type = {
      val tparamSyms = enterTypeParams(meth, tparams);
      val vparamSymss = enterValueParams(meth, vparamss);
      val restype =
        typechecker.checkNoEscape(tpt.pos,
	  deconstIfNotFinal(meth,
	    if (meth.name == nme.CONSTRUCTOR) context.enclClass.owner.tpe
	    else if (tpt.isEmpty) { tpt.tpe = typechecker.transformExpr(rhs).tpe; tpt.tpe }
	    else typechecker.transformType(tpt).tpe));
      def mkMethodType(vparams: List[Symbol], restpe: Type) =
	MethodType(vparams map (.tpe), restpe);
      makePolyType(
	tparamSyms,
	if (vparamSymss.isEmpty) PolyType(List(), restype)
	else (vparamSymss :\ restype)(mkMethodType))
    }

    private def aliasTypeSig(tpsym: Symbol, tparams: List[AbsTypeDef], rhs: Tree): Type =
      makePolyType(enterTypeParams(tpsym, tparams), typechecker.transformType(rhs).tpe);

    private def typeSig(tree: Tree): Type =
      try {
	val sym: Symbol = tree.symbol;
	tree match {
	  case ClassDef(_, _, tparams, tpt, impl) =>
	    new Typer(context.makeNewScope(tree, sym)).classSig(sym, tparams, tpt, impl)

	  case ModuleDef(_, _, tpt, impl) =>
	    val clazz = sym.moduleClass;
            clazz.setInfo(new Typer(context.make(tree, clazz)).templateSig(clazz, impl));
	    if (tpt.isEmpty) { tpt.tpe = clazz.tpe; tpt.tpe }
	    else typechecker.transformType(tpt).tpe

	  case DefDef(_, _, tparams, vparamss, tpt, rhs) =>
	    new Typer(context.makeNewScope(tree, sym))
	      .methodSig(sym, tparams, vparamss, tpt, rhs)

	  case ValDef(_, _, tpt, rhs) =>
            deconstIfNotFinal(sym,
              if (tpt.isEmpty)
                if (rhs.isEmpty) {
		  unit.error(tpt.pos, "missing parameter type");
                  ErrorType
                } else {
                  tpt.tpe = new TypeChecker(context.make(tree, sym))
                    .transformExpr(rhs).tpe;
                  tpt.tpe
                }
              else typechecker.transformType(tpt).tpe)

	  case AliasTypeDef(_, _, tparams, rhs) =>
            new Typer(context.makeNewScope(tree, sym)).aliasTypeSig(sym, tparams, rhs)

	  case AbsTypeDef(_, _, lo, hi) =>
            TypeBounds(typechecker.transformType(lo).tpe, typechecker.transformType(hi).tpe);

	  case imptree @ Import(expr, selectors) =>
            val expr1 = typechecker.transformExpr(expr);
	    val base = expr1.tpe;
            typechecker.checkStable(expr1);
            for (val Pair(from, to) <- selectors) {
	      if (from != nme.WILDCARD && base != ErrorType &&
		  base.member(from) == NoSymbol && base.member(from.toTypeName) == NoSymbol)
	        unit.error(tree.pos, from.decode + " is not a member of " + expr);
	      if (to != null && to != nme.WILDCARD &&  (selectors exists (p => p._2 == to)))
		unit.error(tree.pos, to.decode + " appears twice as a target of a renaming");
	    }
            ImportType(imptree)
        }
      } catch {
        case ex: TypeError =>
	  typechecker.reportTypeError(tree.pos, ex);
	  ErrorType
      }

    /** Check that symbol's definition is well-formed. This means:
     *   - no conflicting modifiers
     *   - `abstract' modifier only for classes
     *   - `override' modifier never for classes
     *   - `def' modifier never for parameters of case classes
     *   - declarations only in traits or abstract classes
     *   - todo: in desugarize: replace ABSTRACT OVERRIDE with ABSOVERRIDE
     */
    def validate(sym: Symbol): unit = {
      def checkNoConflict(flag1: int, flag2: int): unit =
	if (sym.hasFlag(flag1) && sym.hasFlag(flag2))
	  unit.error(sym.pos,
	    if (flag1 == DEFERRED)
	      "abstract member may not have " + Flags.flagsToString(flag2) + " modifier";
	    else
	      "illegal combination of modifiers: " +
	      Flags.flagsToString(flag1) + " and " + Flags.flagsToString(flag2));
      if (sym.hasFlag(ABSTRACT) && !sym.isClass)
	unit.error(sym.pos, "`abstract' modifier can be used only for classes; " +
	  "\nit should be omitted for abstract members");
      if (sym.hasFlag(OVERRIDE | ABSOVERRIDE) && sym.isClass)
	unit.error(sym.pos, "`override' modifier not allowed for classes");
      if (sym.info.symbol == definitions.FunctionClass(0) &&
	  sym.isValueParameter && sym.owner.isClass && sym.owner.hasFlag(CASE))
	unit.error(sym.pos, "pass-by-name arguments not allowed for case class parameters");
      if ((sym.flags & DEFERRED) != 0) {
	if (!sym.owner.isClass || sym.owner.isModuleClass || sym.owner.isAnonymousClass) {
	  unit.error(sym.pos,
	    "only classes can have declared but undefined members" +
	    (if (!sym.isVariable) ""
	     else "\n(Note that variables need to be initialized to be defined)"));
	  sym.resetFlag(DEFERRED);
	}
      }
      checkNoConflict(DEFERRED, PRIVATE);
      checkNoConflict(FINAL, SEALED);
      if (!sym.hasFlag(MODULE)) checkNoConflict(FINAL, PRIVATE);
      checkNoConflict(PRIVATE, PROTECTED);
      checkNoConflict(PRIVATE, OVERRIDE);
      checkNoConflict(DEFERRED, FINAL);
    }
  }
}
