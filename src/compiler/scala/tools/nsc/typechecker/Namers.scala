/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.typechecker;

import scala.tools.nsc.util.Position;
import symtab.Flags;
import symtab.Flags._;

/** Methods to create symbols and to enter them into scopes. */
trait Namers requires Analyzer {
  import global._;
  import definitions._;

  /** Convert to corresponding type parameters all skolems which satisfy one of the
   *  following two conditions:
   *  1. The skolem is a parameter of a class or alias type
   *  2. The skolem is a method parameter which appears in parameter `tparams'
   */
  class DeSkolemizeMap(tparams: List[Symbol]) extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case TypeRef(pre, sym, args) =>
      val tparam = sym.deSkolemize;
      mapOver(
          if (tparam == sym || !(tparams contains tparam)) tp
          else rawTypeRef(NoPrefix, tparam, args))
      case SingleType(pre, sym) if (sym.isThisSkolem) =>
        ThisType(sym.deSkolemize)
      case PolyType(tparams1, restpe) =>
        new DeSkolemizeMap(tparams1 ::: tparams).mapOver(tp)
      case ClassInfoType(parents, decls, clazz) =>
        val parents1 = List.mapConserve(parents)(this);
        if (parents1 eq parents) tp else ClassInfoType(parents1, decls, clazz);
      case _ =>
        mapOver(tp)
    }
  }

  class Namer(val context: Context) {

    val typer = newTyper(context);

    def setPrivateWithin(sym: Symbol, mods: Modifiers): Symbol = {
      if (!mods.privateWithin.isEmpty)
        sym.privateWithin = typer.qualifyingClassContext(EmptyTree, mods.privateWithin).owner;
      sym
    }

    def updatePosFlags(sym: Symbol, pos: int, flags: int): Symbol = {
      if (settings.debug.value) log("overwriting " + sym);
      val lockedFlag = sym.flags & LOCKED;
      sym.reset(NoType);
      sym setPos pos;
      sym.flags = flags | lockedFlag;
      if (sym.isModule && sym.moduleClass != NoSymbol)
        updatePosFlags(sym.moduleClass, pos, (flags & ModuleToClassFlags) | MODULE | FINAL);
      if (sym.owner.isPackageClass && sym.linkedSym.rawInfo.isInstanceOf[loaders.SymbolLoader])
        // pre-set linked symbol to NoType, in case it is not loaded together with this symbol.
        sym.linkedSym.setInfo(NoType);
      sym
    }

    private def isTemplateContext(context: Context): boolean = context.tree match {
      case Template(_, _) => true
      case Import(_, _) => isTemplateContext(context.outer)
      case _ => false
    }

    private var innerNamerCache: Namer = null;
    def innerNamer: Namer = {
      if (innerNamerCache == null)
        innerNamerCache = if (!isTemplateContext(context)) this
                          else new Namer(context.make(context.tree, context.owner, new Scope()));
      innerNamerCache
    }

    private def doubleDefError(pos: int, sym: Symbol): unit =
      context.error(pos,
        sym.name.toString() + " is already defined as " +
        (if (sym.hasFlag(CASE)) "case class " + sym.name else sym.toString()));

    def enterInScope(sym: Symbol): Symbol = {
      // allow for overloaded methods
      if (!(sym.isSourceMethod && sym.owner.isClass)) {
      	val prev = context.scope.lookupEntry(sym.name);
      	if (prev != null && prev.owner == context.scope && !prev.sym.isSourceMethod) {
/*
      	  if (sym.sourceFile == null && prev.sym.sourceFile == null) {}

      	  else if (sym.sourceFile != null && prev.sym.sourceFile != null &&
      		     sym.sourceFile.equals(prev.sym.sourceFile)) {}
      	  else {
      	    System.err.println("SYM: " + sym.sourceFile);
      	    System.err.println("PRV: " + prev.sym.sourceFile);
*/
     	    doubleDefError(sym.pos, prev.sym);
//      	  }
      	}
      }
      context.scope enter sym;
      sym
    }

    private def enterPackageSymbol(pos: int, name: Name): Symbol = {
      val cscope = if (context.owner == EmptyPackageClass) RootClass.info.decls
                   else context.scope;
      val p: Symbol = cscope.lookup(name);
      if (p.isPackage && cscope == p.owner.info.decls) {
        p
      } else {
        val cowner = if (context.owner == EmptyPackageClass) RootClass else context.owner;
        val pkg = cowner.newPackage(pos, name);
        pkg.moduleClass.setInfo(new PackageClassInfoType(new Scope(), pkg.moduleClass));
        pkg.setInfo(pkg.moduleClass.tpe);
        enterInScope(pkg)
      }
    }

    private def inConstructorFlag: long =
      if (context.owner.isConstructor && !context.inConstructorSuffix) INCONSTRUCTOR
      else 0l;

    private def enterClassSymbol(pos: int, flags: int, name: Name): Symbol = {
      var c: Symbol = context.scope.lookup(name);
      if (c.isType && !currentRun.compiles(c) && context.scope == c.owner.info.decls) {
        updatePosFlags(c, pos, flags);
      } else {
        c = enterInScope(context.owner.newClass(pos, name)).setFlag(flags | inConstructorFlag);
      }
      if (c.owner.isPackageClass) {
      	val file = context.unit.source.getFile();
      	val clazz = c.asInstanceOf[ClassSymbol];
      	if (c.owner.isPackageClass) {
      	  if (clazz.sourceFile != null && !clazz.sourceFile.equals(file)) {
            System.err.println("SOURCE MISMATCH: " + clazz.sourceFile + " vs. " + file + " SYM=" + c);
          }
          clazz.sourceFile = file;
      	}
      	if (clazz.sourceFile != null) {
      	  assert(!currentRun.compiles(clazz) || clazz.sourceFile == currentRun.symSource(c));
      	  currentRun.symSource(c) = clazz.sourceFile
      	}
      }
      c
    }

    private def enterModuleSymbol(pos: int, flags: int, name: Name): Symbol = {
      var m: Symbol = context.scope.lookup(name);
      if (m.isModule && !m.isPackage && !currentRun.compiles(m) && (context.scope == m.owner.info.decls)) {
        updatePosFlags(m, pos, flags)
      } else {
        if (m.isTerm && !m.isPackage && !currentRun.compiles(m) && (context.scope == m.owner.info.decls))
          context.scope.unlink(m);
        m = context.owner.newModule(pos, name);
        m.setFlag(flags);
        m.moduleClass.setFlag(flags | inConstructorFlag);
	enterInScope(m)
      }
      if (m.owner.isPackageClass) {
        m.moduleClass.sourceFile = context.unit.source.getFile();
        currentRun.symSource(m) = m.moduleClass.sourceFile
      }
      m
    }

    private def enterCaseFactorySymbol(pos: int, flags: int, name: Name): Symbol = {
      var m: Symbol = context.scope.lookup(name);
      if (m.isTerm && !m.isPackage && !currentRun.compiles(m) && context.scope == m.owner.info.decls) {
        updatePosFlags(m, pos, flags)
      } else {
        m = enterInScope(context.owner.newMethod(pos, name)).setFlag(flags);
      }
      if (m.owner.isPackageClass) currentRun.symSource(m) = context.unit.source.getFile();
      m
    }

    def enterSyms(trees: List[Tree]): Namer =
      (this /: trees) ((namer, tree) => namer.enterSym(tree));

    def newTypeSkolems(tparams: List[Symbol]): List[Symbol] = {
      val tskolems = tparams map (.newTypeSkolem);
      val ltp = new LazyType {
        override def complete(sym: Symbol): unit =
          sym setInfo sym.deSkolemize.info.substSym(tparams, tskolems);
      }
      tskolems foreach (.setInfo(ltp));
      tskolems
    }

    def skolemize(tparams: List[AbsTypeDef]): unit = if (settings.Xgadt.value) {
      val tskolems = newTypeSkolems(tparams map (.symbol));
      for (val Pair(tparam, tskolem) <- tparams zip tskolems) tparam.symbol = tskolem
    }

    def applicableTypeParams(owner: Symbol): List[Symbol] =
      if (owner.isTerm || owner.isPackageClass) List()
      else applicableTypeParams(owner.owner) ::: owner.unsafeTypeParams;

    def deSkolemize: TypeMap = new DeSkolemizeMap(applicableTypeParams(context.owner));

    def enterSym(tree: Tree): Namer = {

      def finishWith(tparams: List[AbsTypeDef]): unit = {
        if (settings.debug.value) log("entered " + tree.symbol + " in " + context.owner + ", scope-id = " + context.scope.hashCode());
	var ltype: LazyType = innerNamer.typeCompleter(tree);
        if (!tparams.isEmpty) {
	  new Namer(context.makeNewScope(tree, tree.symbol)).enterSyms(tparams);
	  ltype = new LazyPolyType(tparams map (.symbol), ltype);
          skolemize(tparams);
	}
	tree.symbol.setInfo(ltype);
      }
      def finish = finishWith(List());


      if (tree.symbol == NoSymbol) {
	val owner = context.owner;
	tree match {
	  case PackageDef(name, stats) =>
	    tree.symbol = enterPackageSymbol(tree.pos, name);
	    val namer = new Namer(
	      context.make(tree, tree.symbol.moduleClass, tree.symbol.info.decls));
	    namer.enterSyms(stats);
	  case ClassDef(mods, name, tparams, _, impl) =>
	    if ((mods.flags & (CASE | ABSTRACT)) == CASE) { // enter case factory method.
	      tree.symbol = enterCaseFactorySymbol(
        		tree.pos, mods.flags & AccessFlags | METHOD | CASE, name.toTermName)
        	          .setInfo(innerNamer.caseFactoryCompleter(tree));
                      setPrivateWithin(tree.symbol, mods);
      }
	    tree.symbol = enterClassSymbol(tree.pos, mods.flags, name);
            setPrivateWithin(tree.symbol, mods);
	    finishWith(tparams);
	  case ModuleDef(mods, name, _) =>
	    tree.symbol = enterModuleSymbol(tree.pos, mods.flags | MODULE | FINAL, name);
            setPrivateWithin(tree.symbol, mods);
            setPrivateWithin(tree.symbol.moduleClass, mods);
	    tree.symbol.moduleClass.setInfo(innerNamer.moduleClassTypeCompleter(tree));
	    finish
	  case ValDef(mods, name, tp, rhs) =>
            if (context.owner.isClass & (mods.flags & LOCAL) == 0) {
	      val accflags =
                ((if ((mods.flags & MUTABLE) != 0) mods.flags & ~MUTABLE else mods.flags | STABLE) |
                 (if ((mods.flags & DEFERRED) == 0) ACCESSOR else 0));
	      val getter = owner.newMethod(tree.pos, name)
	        .setFlag(accflags)
                .setInfo(innerNamer.getterTypeCompleter(tree));
              setPrivateWithin(getter, mods);
	      enterInScope(getter);
	      if ((mods.flags & MUTABLE) != 0) {
	        val setter = owner.newMethod(tree.pos, nme.getterToSetter(name))
		  .setFlag(accflags & ~STABLE & ~CASEACCESSOR)
                  .setInfo(innerNamer.setterTypeCompleter(tree));
                setPrivateWithin(setter, mods);
	        enterInScope(setter)
	      }
	      tree.symbol =
		if ((mods.flags & DEFERRED) == 0)
		  enterInScope(owner.newValue(tree.pos, nme.getterToLocal(name)))
 	            .setFlag(mods.flags & FieldFlags | PRIVATE | LOCAL)
	            .setInfo(innerNamer.typeCompleter(tree))
		else getter;
            } else {
              tree.symbol = enterInScope(owner.newValue(tree.pos, name))
                .setFlag(mods.flags);
	      finish
            }
	  case DefDef(mods, nme.CONSTRUCTOR, tparams, vparams, tp, rhs) =>
	    tree.symbol = enterInScope(owner.newConstructor(tree.pos))
	      .setFlag(mods.flags | owner.getFlag(ConstrFlags));
            setPrivateWithin(tree.symbol, mods);
	    finishWith(tparams);
	  case DefDef(mods, name, tparams, _, _, _) =>
	    tree.symbol = enterInScope(owner.newMethod(tree.pos, name))
              .setFlag(mods.flags);
            setPrivateWithin(tree.symbol, mods);
	    finishWith(tparams);
	  case AbsTypeDef(mods, name, _, _) =>
	    tree.symbol = enterInScope(owner.newAbstractType(tree.pos, name))
              .setFlag(mods.flags);
            setPrivateWithin(tree.symbol, mods);
	    finish
	  case AliasTypeDef(mods, name, tparams, _) =>
	    tree.symbol = enterInScope(owner.newAliasType(tree.pos, name))
              .setFlag(mods.flags);
            setPrivateWithin(tree.symbol, mods);
	    finishWith(tparams)
	  case Attributed(attr, defn) =>
	    enterSym(defn);
	  case DocDef(_, defn) =>
	    enterSym(defn)
	  case imp @ Import(_, _) =>
	    tree.symbol = NoSymbol.newImport(tree.pos).setInfo(innerNamer.typeCompleter(tree));
	    return new Namer(context.makeNewImport(imp));
	  case _ =>
	}
      }
      this
    }

// --- Lazy Type Assignment --------------------------------------------------

    def typeCompleter(tree: Tree) = new TypeCompleter(tree) {
      override def complete(sym: Symbol): unit = {
        if (settings.debug.value) log("defining " + sym);
        val tp = typeSig(tree);
        sym.setInfo(tp);
        if (settings.Xgadt.value) System.out.println("" + sym + ":" + tp);
        if (settings.debug.value) log("defined " + sym);
        validate(sym);
      }
    }

    def moduleClassTypeCompleter(tree: Tree) = new TypeCompleter(tree) {
      override def complete(sym: Symbol): unit = {
        tree.symbol.info // sets moduleClass info as a side effect.
      }
    }

    def getterTypeCompleter(tree: Tree) = new TypeCompleter(tree) {
      override def complete(sym: Symbol): unit = {
        if (settings.debug.value) log("defining " + sym);
        sym.setInfo(PolyType(List(), typeSig(tree)));
        if (settings.debug.value) log("defined " + sym);
        validate(sym);
      }
    }

    def setterTypeCompleter(tree: Tree) = new TypeCompleter(tree) {
      override def complete(sym: Symbol): unit = {
        if (settings.debug.value) log("defining " + sym);
        sym.setInfo(MethodType(List(typeSig(tree)), UnitClass.tpe));
        if (settings.debug.value) log("defined " + sym);
        validate(sym);
      }
    }

    def selfTypeCompleter(tree: Tree) = new TypeCompleter(tree) {
      override def complete(sym: Symbol): unit = {
        sym.setInfo(typer.typedType(tree).tpe);
      }
    }

    def caseFactoryCompleter(tree: Tree) = new TypeCompleter(tree) {
      override def complete(sym: Symbol): unit = {
	val clazz = tree.symbol;
	var tpe = clazz.primaryConstructor.tpe;
	val tparams = clazz.unsafeTypeParams;
	if (!tparams.isEmpty) tpe = PolyType(tparams, tpe).cloneInfo(sym);
	sym.setInfo(tpe);
      }
    }

    private def deconstIfNotFinal(sym: Symbol, tpe: Type): Type =
      if (sym.isVariable ||
	  !(sym hasFlag FINAL) ||
	  sym.isMethod && !(sym hasFlag ACCESSOR)) tpe.deconst
      else tpe;

    def enterValueParams(owner: Symbol, vparamss: List[List[ValDef]]): List[List[Symbol]] = {
      def enterValueParam(param: ValDef): Symbol = {
	param.symbol = owner.newValueParameter(param.pos, param.name)
	  .setInfo(typeCompleter(param))
          .setFlag(param.mods.flags & (BYNAMEPARAM | IMPLICIT));
        setPrivateWithin(param.symbol, param.mods);
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

    private def templateSig(templ: Template): Type = {
      val clazz = context.owner;
      val parents = typer.parentTypes(templ) map (p => if (p.tpe.isError) AnyRefClass.tpe else p.tpe);
      val decls = new Scope();
      log("members of " + clazz + "=" + decls.hashCode());//debug
      new Namer(context.make(templ, clazz, decls)).enterSyms(templ.body);
      ClassInfoType(parents, decls, clazz)
    }

    private def classSig(tparams: List[AbsTypeDef], tpt: Tree, impl: Template): Type = {
      val tparamSyms = typer.reenterTypeParams(tparams);
      if (!tpt.isEmpty)
        context.owner.typeOfThis = selfTypeCompleter(tpt);
      else tpt.tpe = NoType;
      makePolyType(tparamSyms, templateSig(impl))
    }

    private def methodSig(tparams: List[AbsTypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree): Type = {
      val meth = context.owner;
      val tparamSyms = typer.reenterTypeParams(tparams);
      val vparamSymss = enterValueParams(meth, vparamss);
      val restype =
	if (tpt.isEmpty) {
	  tpt.tpe = if (meth.name == nme.CONSTRUCTOR) context.enclClass.owner.tpe
		    else deconstIfNotFinal(meth, typer.computeType(rhs));
	  tpt.tpe
	} else typer.typedType(tpt).tpe;
      def mkMethodType(vparams: List[Symbol], restpe: Type) = {
	val formals = vparams map (.tpe);
	if (!vparams.isEmpty && vparams.head.hasFlag(IMPLICIT)) ImplicitMethodType(formals, restpe)
	else MethodType(formals, restpe);
      }
      makePolyType(
	tparamSyms,
	if (vparamSymss.isEmpty) PolyType(List(), restype)
	else (vparamSymss :\ restype)(mkMethodType))
    }

    /** If `sym' is an implicit value, check that its type signature `tp' is contractive.
     *  This means: The type of every implicit parameter is properly contained
     *  in the type that is obtained by removing all implicit parameters and converting
     *  the rest to a function type.
     *  If the check succeeds return `tp' itself, otherwise `ErrorType'.
     */
    private def checkContractive(sym: Symbol, tp: Type): Type = {
      /* The type signature without implicit parameters converted to function type */
      def provided(tp: Type): Type = tp match {
	case PolyType(_, restpe) => provided(restpe);
	case mt: ImplicitMethodType => mt.resultType;
	case MethodType(formals, restpe) => functionType(formals, provided(restpe))
	case _ => tp
      }
      /* The types of all implicit parameters */
      def required(tp: Type): List[Type] = tp match {
	case PolyType(_, restpe) => required(restpe);
	case mt: ImplicitMethodType => mt.paramTypes;
	case MethodType(formals, restpe) => required(restpe);
	case _ => List()
      }
      var result = tp;
      if (sym hasFlag IMPLICIT) {
	val p = provided(tp);
	for (val r <- required(tp)) {
	  if (!isContainedIn(r, p) || (r =:= p)) {
	    context.error(sym.pos, "implicit " + sym + " is not contractive," +
			  "\n because the implicit parameter type " + r +
			  "\n is not strictly contained in the signature " + p);
	    result = ErrorType;
	  }
	}
      }
      result
    }

    private def aliasTypeSig(tpsym: Symbol, tparams: List[AbsTypeDef], rhs: Tree): Type =
      makePolyType(typer.reenterTypeParams(tparams), typer.typedType(rhs).tpe);

    private def typeSig(tree: Tree): Type = deSkolemize {
      try {
	val sym: Symbol = tree.symbol;
	tree match {
	  case ClassDef(_, _, tparams, tpt, impl) =>
	    new Namer(context.makeNewScope(tree, sym)).classSig(tparams, tpt, impl)

	  case ModuleDef(_, _, impl) =>
	    val clazz = sym.moduleClass;
	    clazz.setInfo(new Namer(context.make(tree, clazz)).templateSig(impl));
	    //clazz.typeOfThis = singleType(sym.owner.thisType, sym);
	    clazz.tpe;

	  case DefDef(_, _, tparams, vparamss, tpt, rhs) =>
	    val result =
	      new Namer(context.makeNewScope(tree, sym)).methodSig(tparams, vparamss, tpt, rhs);
	    checkContractive(sym, result)

	  case ValDef(_, _, tpt, rhs) =>
	    if (tpt.isEmpty)
	      if (rhs.isEmpty) {
		context.error(tpt.pos, "missing parameter type");
		ErrorType
	      } else {
		tpt.tpe = deconstIfNotFinal(sym, newTyper(context.make(tree, sym)).computeType(rhs));
		tpt.tpe
	      }
	    else {
              val typer1 =
                if (false && sym.hasFlag(PARAM) && sym.owner.isConstructor && !phase.erasedTypes) {
                  //todo: find out instead why Template contexts can be nested in Template contexts?
                  var c = context.enclClass;
                  while (c.tree.isInstanceOf[Template]) c = c.outer;
                  newTyper(c)
                } else typer;
              typer1.typedType(tpt).tpe
            }

	  case AliasTypeDef(_, _, tparams, rhs) =>
	    new Namer(context.makeNewScope(tree, sym)).aliasTypeSig(sym, tparams, rhs)

	  case AbsTypeDef(_, _, lo, hi) =>
            //System.out.println("bounds of " + sym + ":" + sym.tpe + " = " + typer.typedType(hi).tpe);
	    TypeBounds(typer.typedType(lo).tpe, typer.typedType(hi).tpe);

	  case Import(expr, selectors) =>
	    val expr1 = typer.typedQualifier(expr);
	    val base = expr1.tpe;
	    typer.checkStable(expr1);
	    def checkSelectors(selectors: List[Pair[Name, Name]]): unit = selectors match {
	      case Pair(from, to) :: rest =>
		if (from != nme.WILDCARD && base != ErrorType &&
		    base.member(from) == NoSymbol && base.member(from.toTypeName) == NoSymbol)
		  context.error(tree.pos, from.decode + " is not a member of " + expr);
		if (from != nme.WILDCARD && (rest.exists (sel => sel._1 == from)))
		  context.error(tree.pos, from.decode + " is renamed twice");
		if (to != null && to != nme.WILDCARD && (rest exists (sel => sel._2 == to)))
		  context.error(tree.pos, to.decode + " appears twice as a target of a renaming");
		checkSelectors(rest)
	      case Nil =>
	    }
	    checkSelectors(selectors);
	    ImportType(expr1)
	}
      } catch {
        case ex: TypeError =>
          //System.out.println("caught " + ex + " in typeSig");//DEBUG
	  typer.reportTypeError(tree.pos, ex);
	  ErrorType
      }
    }

    /** Check that symbol's definition is well-formed. This means:
     *   - no conflicting modifiers
     *   - `abstract' modifier only for classes
     *   - `override' modifier never for classes
     *   - `def' modifier never for parameters of case classes
     *   - declarations only in mixins or abstract classes
     */
    def validate(sym: Symbol): unit = {
      def checkNoConflict(flag1: int, flag2: int): unit =
	if (sym.hasFlag(flag1) && sym.hasFlag(flag2))
	  context.error(sym.pos,
	    if (flag1 == DEFERRED)
	      "abstract member may not have " + Flags.flagsToString(flag2) + " modifier";
	    else
	      "illegal combination of modifiers: " +
	      Flags.flagsToString(flag1) + " and " + Flags.flagsToString(flag2));
      if (sym.hasFlag(IMPLICIT) && !sym.isTerm)
	context.error(sym.pos, "`implicit' modifier can be used only for values, variables and methods");
      if (sym.hasFlag(MIXIN) && !sym.isClass)
	context.error(sym.pos, "`mixin' modifier can be used only for classes");
      if (sym.hasFlag(ABSTRACT) && !sym.isClass)
	context.error(sym.pos, "`abstract' modifier can be used only for classes; " +
	  "\nit should be omitted for abstract members");
      if (sym.hasFlag(OVERRIDE | ABSOVERRIDE) && sym.isClass)
	context.error(sym.pos, "`override' modifier not allowed for classes");
      if (sym.hasFlag(ABSOVERRIDE) && !sym.owner.isMixin)
	context.error(sym.pos, "`abstract override' modifier only allowed for members of mixin classes");
      if (sym.info.symbol == FunctionClass(0) &&
	  sym.isValueParameter && sym.owner.isClass && sym.owner.hasFlag(CASE))
	context.error(sym.pos, "pass-by-name arguments not allowed for case class parameters");
      if ((sym.flags & DEFERRED) != 0) {
	if (!sym.isValueParameter && !sym.isTypeParameterOrSkolem &&
	    (!sym.owner.isClass || sym.owner.isModuleClass || sym.owner.isAnonymousClass)) {
	  context.error(sym.pos,
	    "only classes can have declared but undefined members" +
	    (if (!sym.isVariable) ""
	     else "\n(Note that variables need to be initialized to be defined)"));
	  sym.resetFlag(DEFERRED);
	}
      }
      checkNoConflict(DEFERRED, PRIVATE);
      checkNoConflict(FINAL, SEALED);
      checkNoConflict(PRIVATE, PROTECTED);
      checkNoConflict(PRIVATE, OVERRIDE);
      checkNoConflict(DEFERRED, FINAL);
    }
  }

  /* Is type `tp1' properly contained in type `tp2'? */
  def isContainedIn(tp1: Type, tp2: Type) = {
    //System.out.println("is " + tp1 + " contained in " + tp2 + "?");//DEBUG
    new ContainsTraverser(tp1).traverse(tp2).result;
  }

  /* Type `elemtp' is contained in type `tp' is one of the following holds:
   *  - elemtp and tp are the same
   *  - tp is a function type and elemtp is not
   *  - tp and elemtp are function types, and arity of tp is greater than arity of elemtp
   *  - tp and elemtp are both parameterized types with same type constructor and prefix,
   *    and each type argument of elemtp is contained in the corresponding type argument of tp.
   */
  private class ContainsTraverser(elemtp: Type) extends TypeTraverser {
    var result = false;
    def traverse(tp: Type): ContainsTraverser = {
      if (!result) {
        if (elemtp =:= tp)
          result = true
        else if (isFunctionType(tp) &&
                 (!isFunctionType(elemtp) || tp.typeArgs.length > elemtp.typeArgs.length))
          result = true
        else Pair(tp, elemtp) match {
          case Pair(TypeRef(pre, sym, args), TypeRef(elempre, elemsym, elemargs)) =>
            if ((sym == elemsym) && (pre =:= elempre) && (args.length == elemargs.length))
              result = List.forall2(elemargs, args) (isContainedIn)
          case _ =>
        }
      }
      if (!result) mapOver(tp);
      this
    }
  }

  abstract class TypeCompleter(val tree: Tree) extends LazyType;
}

