/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.backend.msil;

import scalac.Global;
import scalac.Unit;
import scalac.ApplicationError;

import scalac.util.Debug;

import scalac.util.Name;
import scalac.util.Names;
import scalac.util.Debug;
import scalac.ast.Tree;
import Tree.*;
import scalac.symtab.Symbol;
import scalac.symtab.Scope;
import scalac.symtab.Kinds;
import scalac.symtab.TypeTags;
import scalac.symtab.Modifiers;
import scalac.symtab.Definitions;
import Scope.SymbolIterator;

import scalac.backend.Primitive;
import scalac.backend.Primitives;

import ch.epfl.lamp.compiler.msil.*;
import ch.epfl.lamp.compiler.msil.emit.*;
import ch.epfl.lamp.util.Position;

import Item.*;

import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.io.IOException;

/**
 * Generates MS IL code via the Reflection.Emit API
 * (ch.epfl.lamp.compiler.msil.emit package)
 *
 * @author Nikolay Mihaylov
 */

public class GenMSIL /*implements Modifiers */ {

    final Map assemblies;

    // The position of the paramater in the parameter list
    final Map/*<Symbol, Integer>*/ params = new HashMap();

    // The LocalBuilder corresponding to the local variable symbol
    final Map/*<Symbol, LocalBuilder>*/ locals = new HashMap();

    AssemblyBuilder defaultAssembly, currAssembly;
    ModuleBuilder currModule, scalaModule;

    ILGenerator code;

    final Global global;
    final TypeCreator tc;
    final Definitions defs;
    final Primitives primitives;
    final ItemFactory items;

    Symbol currentPackage;


    static final Item TRUE_ITEM  = Item.CondItem(Test.True, null, null);
    static final Item FALSE_ITEM = Item.CondItem(Test.False, null, null);

    final Symbol STRING_CONCAT;
    static final Type SCALA_UNIT = TypeCreator.getJavaType("scala.Unit");
    static final FieldInfo RUNTIME_UNIT_VAL =
	TypeCreator.getJavaType("scala.runtime.RunTime").GetField("UNIT_VAL");

    /**
     */
    public GenMSIL(Global global, GenMSILPhase phase) {
        this.global = global;
	defs = global.definitions;
	primitives = global.primitives;
	assemblies = phase.assemblies;

	tc = new TypeCreator(this, phase);
	items = new ItemFactory(this);

	currentPackage = defs.ROOT_CLASS; /// ???
	scalaModule = getPackage("scala", false);

	STRING_CONCAT = defs.STRING_CLASS.members().
	    lookup(Name.fromString("concat"));

    }


   /** Generate .NET assembly */
    public ModuleBuilder getPackage(String name, boolean isRealAssembly) {
	AssemblyBuilder assem = (AssemblyBuilder) assemblies.get(name);
	if (assem == null) {
	    AssemblyName an = new AssemblyName();
	    an.Name = name;
	    assem = EmitUtils.DefineDynamicAssembly(an);
	    if (isRealAssembly)
		assemblies.put(name, assem);
	}
	ModuleBuilder module = (ModuleBuilder) assem.GetModule(name);
	if (module == null) {
	    module = assem.DefineDynamicModule(name, name + ".dll");
	}
	return module;
    }


    /**
     */
    public void apply() {
	if (global.target == global.TARGET_MSIL) {
	    currModule = getPackage("prog", true);
	    main = (MethodBuilder) currModule.GetMethod("Main", Type.EmptyTypes);
	    if (main == null) {
		main = currModule.DefineGlobalMethod
		    ("Main", MethodAttributes.Static,
		     Type.GetType("System.Void"),
		     new Type[] {Type.GetType("System.String[]")} );
		main.DefineParameter(0, 0L, "args");
	    }

	    // create mappings from Symbols to MSIL Types
	    tc.traverse(global.units);

	    try {
                for (int i = 0; i < global.units.length; i++) {
                    apply(global.units[i]);
                }
	    } catch (Throwable e) {
		//log("params: " + params);
		//log("locals: " + locals);
		if (code != null) {
		    MethodBase method = code.owner;
		    log("Processing method " + method.DeclaringType +
			"::" +method.Name);
		}
		e.printStackTrace();
		throw (Error) e;
	    }
	    tc.createTypes();
	    main.GetILGenerator().Emit(OpCodes.Ret);
	    ((AssemblyBuilder)currModule.Assembly).SetEntryPoint(main);
	    try {
		Iterator iter = assemblies.values().iterator();
		while (iter.hasNext()) {
		    AssemblyBuilder assem = (AssemblyBuilder) iter.next();
// 		    log("Types defined in assembly: " + assem.FullName);
// 		    Type[] types = assem.GetTypes();
// 		    for (int i = 0; i < types.length; i++)
// 			log("\t" + types[i]);
		    assem.Save(assem.FullName + ".il");
		}
	    }
	    catch (IOException e) {
		e.printStackTrace();
	    }
	}
    }

    MethodBuilder main;

    static final Type STRING_ARRAY = Type.GetType("System.String[]");

    final Map mains = new HashMap();

    void checkMain(MethodBase method) {
	//log("checking method: " + method);
	if ( ! currentClass.isModuleClass() )
	    return;

	if (method.IsConstructor || method.IsAbstract ||
	    !method.Name.equals("main"))
	    return;
	ParameterInfo[] params = method.GetParameters();
	if (params.length != 1)
	    return;
	if (params[0].ParameterType != STRING_ARRAY)
	    return;

	//log("'main' method found: " + method);
	mains.put(currentClass, method);
	ILGenerator code = main.GetILGenerator();
	code.Emit(OpCodes.Ldsfld, tc.getModuleField(currentClass));
	code.Emit(OpCodes.Ldarg_0);
	code.Emit(OpCodes.Callvirt, (MethodInfo)method);
	if (!returnsVoid(main))
	    code.Emit(OpCodes.Pop);
    }


    Unit currUnit;

    /**
     */
    public void apply(Unit unit) {
	currUnit = unit;
	for (int i = 0; i < unit.body.length; i++) {
	    Tree tree = unit.body[i];
	    Symbol sym = tree.symbol();
	    switch (tree) {
	    case Empty: break;
	    case ClassDef(_, _, _, _, _, Template(_, Tree[] body)):
		genClass(sym, body);
		break;
	    case PackageDef(Tree packaged, Template impl):
		//String packageName = packaged.symbol().fullNameString();
		//log("Package: " + dumpSym(packaged.symbol()));// +
		//"; symbol.fullName() = " + packaged.symbol().fullName());
		//currModule = getPackage(packageName);
		genPackage(currAssembly, impl.body);
		break;
	    case ValDef(_, _, _, _):
		//log("ValDef: " + dumpSym(tree.symbol()));
		break;
	    default:
		throw new ApplicationError
		    ("Illegal top-level definition: " + Debug.show(tree));
	    }
	}
    }


    /** Generate the code for Package definition
     */
    public void genPackage(AssemblyBuilder assembly, Tree[] body) {
	for (int i = 0; i < body.length; i++) {
	    Symbol sym = body[i].symbol();
	    //log("genPackage: " + dumpSym(body[i].symbol()));
	    switch (body[i]) {
	    case Empty:
		break;

	    case ClassDef(_, _, _, _, _, Template(_, Tree[] classBody)):
		genClass(sym, classBody);
		break;

	    case ValDef(_, _, Tree tpe, Tree rhs):
		if (sym.isModule()) {
		} else
		    throw new ApplicationError();
		//log("ValDef: " + dumpSym(tree.symbol()));
		break;

	    case PackageDef(Tree packaged, Template(_ , Tree[] body2)):
		//log("genPackage: nested package: " + dumpSym(sym));
		genPackage(currAssembly, body2);
		break;

	    default:
		log("genPackage: Only class definitions expected in a package!: Found instead: " +
		    dumpSym(sym));
	    }
	}
  }

    public Symbol currentClass;

    public MethodBase currentMethod;

    /** Generate the code for a class
     */
    public void genClass(Symbol clazz, Tree[] body) {
	//log("genClass: " + dumpSym(clazz));
	//log("\tfullName = " + clazz.fullName());
	//log("\tqualifier = " + Debug.show(clazz.qualifier()));
	//log("class members: " + clazz.members());
	Symbol outerClass = currentClass;
	currentClass = clazz;
	if ( clazz.isModule() || clazz.isModuleClass() ) {
	    //log("genClass: initializing module field for module " + dumpSym(clazz));
	    tc.getModuleField(clazz);
	}
	for (int i = 0; i < body.length; i++) {
	    Symbol sym = body[i].symbol();
	    switch (body[i]) {
	    case Empty:
		break;

	    case ValDef(_, _, _, _):
		// just to generate the field declaration
		// the rhs should've been moved to the constructor body
 		tc.getField(sym);
		break;

	    case ClassDef(_, _, _, _, _, Template impl):
		genClass(sym, impl.body);
		break;

	    case DefDef(_, _, _, ValDef[][] vparams, Tree tpe, Tree rhs):
		//log("genClass.DefDef: " + dumpSym(sym));
		//log("DefDef: type = " + Debug.show(tpe.type));
		currentMethod = tc.getMethod(sym);
		if (!currentMethod.IsAbstract) {
		    checkMain(currentMethod);
		    genDef(sym, vparams[0], rhs, type2MSILType(tpe.type));
		}
		break;

	    default:
		assert false : "Illegal class body definition: " + body[i];
	    }
	}
	currentClass = outerClass;
    } //genClass()


    boolean lastStatement;
    static final boolean enableTailCalls = true;


    /** Generate the code for Methods
     */
    public void genDef(Symbol sym, ValDef[] parameters, Tree rhs, MSILType toType) {
	MethodBase method = tc.getMethod(sym);
	//log("genDef: " + method.getSignature());

	params.clear();
	locals.clear();
	int argOffset = method.IsStatic ? 0 : 1;
	for (int i = 0; i < parameters.length; i++) {
	    params.put(parameters[i].symbol(), new Integer(i + argOffset));
	}

	if (method.IsConstructor) {
	    ConstructorInfo ctor = (ConstructorInfo) method;
	    code = ((ConstructorBuilder)ctor).GetILGenerator();
	    FieldInfo moduleField = tc.getModuleField(currentClass);
	    if (moduleField != null) {
// 		log("genDef: initializing " + moduleField +
// 		    " for class " + method.DeclaringType);

		// emit the call to the superconstructor
		switch (rhs) {
		case Block(Tree[] stats):
		    // this is the call to the super constructor
		    drop(gen(stats[0], MSILType.VOID));
		    break;
		default:
		    drop(gen(rhs, MSILType.VOID));
		}

		ConstructorBuilder cctor = ((TypeBuilder)(method.DeclaringType)).
		    DefineConstructor(MethodAttributes.Static |
				      MethodAttributes.Public,
				      CallingConventions.Standard,
				      Type.EmptyTypes);
		currentMethod = cctor;
		ILGenerator ctorCode = code;
		code = cctor.GetILGenerator();
		// initialize the static module reference
		code.Emit(OpCodes.Newobj, ctor);
		code.Emit(OpCodes.Stsfld, moduleField);
		switch (rhs) {
		case Block(Tree[] stats):
		    int n = stats.length;
		    assert n > 0;
		    for (int i = 1; i < n; i++)
			drop(gen(stats[i], MSILType.VOID));
		    break;
		}
		code.Emit(OpCodes.Ret);
		code = ctorCode;
	    } else
		drop(gen(rhs, MSILType.VOID));
	}
	else {
	    lastStatement = true;
	    code = ((MethodBuilder)method).GetILGenerator();
	    Item item = gen(rhs, toType);
	    if (returnsVoid(method))
		drop(item);
	    else
		coerce(load(item), toType); // coerce???
	}
	code.Emit(OpCodes.Ret);

	lastStatement = false;
	code = null;
    } // genDef();


    /** check if the result type of a method is void
     */
    public boolean returnsVoid(MethodBase method) {
	return method.IsConstructor ||
	    (((MethodInfo)method).ReturnType == TypeCreator.VOID);
    }


    /** emit the code for this'
     */
    void emitThis() {
	if (currentMethod.IsStatic) {
	    if (currentMethod.IsConstructor) {
		code.Emit(OpCodes.Ldsfld, tc.getModuleField(currentClass));
	    } else
		throw new ApplicationError
		    ("Static methods don't have 'this' pointer");
	} else
	    code.Emit(OpCodes.Ldarg_0);
    }


    /** Generate code for array of trees
     */
    public Item gen(Tree[] trees) {
	int n = trees.length;
	if (n == 0)
	    return items.VoidItem();
	boolean tmpLastStatement = lastStatement; lastStatement = false;
	for (int i = 0; i < n-1; i++) {
	    drop(gen(trees[i], MSILType.VOID));
	}
	lastStatement = tmpLastStatement;
	return gen(trees[n-1], type2MSILType(trees[n-1].type));
    }


    public Item gen(Tree tree) {
	return gen(tree, type2MSILType(tree.type));
    }


    Item check(Item i) {
	assert i != null;
	return i;
    }

    int pos;
    /** The main generator function. It keeps track of
     *	the current position in the tree for better error messages
     */
    Item gen(Tree tree, MSILType toType) {
	int tmpPos = pos;
	pos = tree.pos;
	Item i = null;
	//i = gen2(tree, toType);
	try { i = gen2(tree, toType); }
	catch (Throwable e) {
	    logErr(tree.pos, "gen(): Exception");
	    e.printStackTrace();
	    System.exit(1);
	    throw (Error) e;
	}
	pos = tmpPos;
	return i;
    }

    /** Generate the code corresponding to a tree node
     */
    Item gen2(Tree tree, MSILType toType) {
	Symbol sym = tree.hasSymbol() ? tree.symbol() : null;
	switch (tree) {
	case Empty:
	    return items.VoidItem();

	case Block(Tree[] stats):
	    return gen(stats);

	case ValDef(_, Name name, Tree tpe, Tree rhs):
	    LocalBuilder local = code.DeclareLocal(tc.getType(sym));
	    local.SetLocalSymInfo(name.toString());
	    locals.put(sym, local);
	    if (rhs == Tree.Empty)
		return items.VoidItem();
	    MSILType type = type2MSILType(tpe.type);
	    load(coerce(gen(rhs, type), type));
	    return check(store(items.LocalItem(type, local)));

	case Ident(Name name):
	    //log("Ident: " + Debug.show(tree));
	    if (sym == defs.NULL)
		//return items.LiteralItem(MSILType.NULL_REF, null);
		return items.LiteralItem(MSILType.OBJECT, null);
	    MSILType type = type2MSILType(sym.type());
	    if ( sym.isModule() ) // ???
 		return load(items.StaticItem
			    (type2MSILType(tree.type), tc.getModuleField(sym)));
	    else {
		Integer slot = (Integer) params.get(sym);
		if (slot != null) {
		    return items.ArgItem(type, slot.intValue());
		} else {
		    LocalBuilder local = (LocalBuilder) locals.get(sym);
		    if (local != null)
			return items.LocalItem(type, local);
		}
		return items.SelectItem(type,
					items.SelfItem(tc.getType(currentClass)),
					tc.getField(sym));
	    }

	case Select(Tree qualifier, Name selector):
	    if (sym.isModule()) {
		//log("gen: Select from a module: " + sym);
		if (sym.isJava())
		    logErr("gen.Select: Cannot treat Java class '" +
			   sym.fullNameString() + "' as value:\n\t" +
			   dumpSym(sym));
		else { // scala module
		    Type module = tc.getType(sym);
		    return items.StaticItem(MSILType.REF(module),
					    tc.getModuleField(sym));
		}
	    }
	    if (!qualifier.hasSymbol()) {
// 		log("gen2(): " + Debug.show(tree));
// 		log("\tqualifier =  " + Debug.show(qualifier));
		return items.SelectItem(type2MSILType(tree.type),
					gen(qualifier), tc.getField(sym));
	    }
//??? 	    if ((qualifier.symbol().flags & NOVAL) == NOVAL) {
// 		log("gen.Select: owner NOVAL");
// 		return items.StaticItem(type2MSILType(tree.type), tc.getField(sym));
// 	    }
	    // check if the tree corresponds to a static Java field
	    if (qualifier.symbol().isModule() && sym.isJava()) {
		//log("gen.Select: static Java field");
		return items.StaticItem
		    (type2MSILType(tree.type), tc.getField(sym));
	    }
	    if ( qualifier.symbol().isModule() ) { // ?????
		//log("gen: Select from a non-Java module: " + qualifier.symbol() + "::" + selector);
		return items.SelectItem
		    (type2MSILType(tree.type), gen(qualifier), tc.getField(sym));
	    }

	    if ( sym.isValue() || sym.isVariable() ) {
 		return items.SelectItem
		    (type2MSILType(sym.type()),
		     gen(qualifier, type2MSILType(qualifier.type)),
		     tc.getField(sym));
	    }

	    log("gen.Select: Dunno what to do with: " + dumpSym(sym));
	    break;


	case Apply(Tree fun, Tree[] args):
	    return check(genApply(fun, args, type2MSILType(tree.type)));

	case Assign(Tree lhs, Tree rhs):
	    boolean tmpLastStatement = lastStatement; lastStatement = false;
	    MSILType type = type2MSILType(lhs.type);
	    Item var = gen(lhs, type);
	    load(gen(rhs, type));
	    lastStatement = tmpLastStatement;
	    return check(store(var));

	case Typed(Literal(Object value), Tree tpe):
	    log("Typed.Literal: " + Debug.show(tree));
	    return items.LiteralItem(type2MSILType(tpe.type), value);

	case Typed(Tree expr, Tree tpe):
	    //log("gen.Typed: processing node: " + Debug.show(tree));
	    return gen(expr, type2MSILType(tpe.type));

	case New(Template(Tree[] baseClasses, Tree[] body)):
	    assert body.length == 0 : "Template should not have a body!";
	    switch (baseClasses[0]) {
	    case Apply(Tree fun, Tree[] args):
		//MethodBase mctor = tc.getMethod(fun.symbol());
		//log("GenMSIL.New: " + dumpSym(fun.symbol()));
		//log("\t" + mctor);
		ConstructorInfo ctor = (ConstructorInfo) tc.getMethod(fun.symbol());
		loadArgs(args, ctor.GetParameters());
		code.Emit(OpCodes.Newobj, ctor);
		return items.StackItem(MSILType.REF(ctor.DeclaringType));
	    default:
		throw new ApplicationError("Dunno what to do!");
	    }

	case This(_):
	    return items.SelfItem(tc.getType(currentClass));

	case Super(_):
	    //logErr("Not implemented yet: Super(" + Debug.show(sym) + ")");
	    //log("gen.Super(_): Super.symbol() = " + dumpSym(sym));
	    //log("gen.Super(tpe): tpe.symbol() = " + dumpSym(tpe.symbol()));
	    //log("gen.Super(tpe): tpe = " + Debug.show(tpe));
	    //log("gen.Super(tpe): tpe.type() = " + Debug.show(tpe.type()));
	    //return items.VoidItem();
	    Item retItem = items.SelfItem(tc.getType(currentClass));
	    //log("gen.Super: generated item: " + retItem);
	    return retItem;

	case Literal(Object value):
	    //log("Literal: " + Debug.show(tree));
	    //log("\ttype = " + Debug.show(tree.type));
	    MSILType t = type2MSILType(tree.type);
	    //log("\tmsil type = " + t);
	    return items.LiteralItem(t, value);

	case If(Tree cond, Tree thenp, Tree elsep):
	    Item item = genIf(cond, thenp, elsep, toType);
	    return check(item);

	default:
	    throw new ApplicationError("Dunno what to do: " + tree);
	}
	throw new ApplicationError
	    ("Dunno what to do with tree node: " + Debug.show(tree));

    } //gen()


    Item genIf(Tree condp, Tree thenp, Tree elsep, MSILType toType) {
	Item iThen = null;
	//log("genIf: coerce to type: " + toType);
	//log("       thenTree = " + Debug.show(thenp));
	//log("       elseTree = " + Debug.show(elsep));
	if (elsep == Tree.Empty && toType == MSILType.VOID) {
	    boolean tmpLastStatement = lastStatement; lastStatement = false;
	    Item condi = gen(condp, type2MSILType(condp.type));
	    Item.CondItem cond = mkCond(condi); //gen(cond, type2MSILType(cond.type)));
	    lastStatement = tmpLastStatement;
	    if (cond.success == null) {
		Chain success = new Chain(code.DefineLabel(), null);
		branch(cond.test, success);
		iThen = coerce(load(gen(thenp, toType)), toType);
		resolve(success);
		resolve(cond.failure);
	    } else {
		Chain fail = (cond.failure != null) ?
		    cond.failure : new Chain(code.DefineLabel(), null);
		branch(cond.test, fail);
		resolve(cond.success);
		iThen = coerce(load(gen(thenp, toType)), toType);
		resolve(fail);
	    }
	} else {
	    boolean tmpLastStatement = lastStatement; lastStatement = false;
	    Item condi = gen(condp, type2MSILType(condp.type));
	    Item.CondItem cond = mkCond(condi); //gen(cond, type2MSILType(cond.type)));
	    lastStatement = tmpLastStatement;

	    Chain fail = cond.failure != null ?
		cond.failure : new Chain(code.DefineLabel(), null);
	    branch(cond.test, fail);
	    resolve(cond.success);
	    Item thenItem = gen(thenp, toType);
	    iThen = load(coerce(thenItem, toType));
	    Label exit = null;
	    if (lastStatement)
		code.Emit(OpCodes.Ret);
	    else {
		if (lastStatement)
		    code.Emit(OpCodes.Ret);
		else {
		    exit = code.DefineLabel();
		    code.Emit(OpCodes.Br, exit);
		}
	    }
	    resolve(fail);
	    Item iElse= null;
	    if (elsep == Tree.Empty) {
		iElse = items.StaticItem(MSILType.REF(SCALA_UNIT), RUNTIME_UNIT_VAL);
		iElse = coerce(iElse, toType);
	    } else
		iElse = load(coerce(gen(elsep, toType), toType));
	    resolve(exit);
	    //log("\tgenIf: 'then' item = " + iThen + "; 'else' item" + iElse);
	}
	return iThen;
    } //genIf


    Item coerce(Item item, MSILType toType) {
	//log("coerce(): coercing item " + item + " to " + toType);
	assert toType != null;
	if (toType == MSILType.VOID)
	    return drop(item);
	switch (item) {
	case VoidItem():
	case SelfItem():
	    return item;
	case StackItem():
	    if (item.type == MSILType.BOOL && toType == MSILType.I4) //?????????
		return items.StackItem(toType);
	    if (item.type != toType) {
		emitConvert(toType);
		return items.StackItem(toType);
	    }
	    return item;
	case LiteralItem(MSILType type, Object value):
	    //log("coercing Literal " + type + " -> " + toType);
	    switch (type) {
	    case REF(_): return item;
	    default:
		return (type == toType) ? item : items.LiteralItem(toType, value);
	    }
	    //return (type == toType) ? item : items.LiteralItem(toType, value);

	default:
	    return coerce(load(item), toType);
	}
    }

    void emitConvert(MSILType toType) {
	switch (toType) {
	case I1: code.Emit(OpCodes.Conv_I1); break;
	case I2: code.Emit(OpCodes.Conv_I2); break;
	case I4: code.Emit(OpCodes.Conv_I4); break;
	case I8: code.Emit(OpCodes.Conv_I8); break;
	case R4: code.Emit(OpCodes.Conv_R4); break;
	case R8: code.Emit(OpCodes.Conv_R8); break;
	case CHAR: code.Emit(OpCodes.Conv_U2); break;
	case REF(_): break;
	case ARRAY(_): break;
	default:
	    logErr("emitConvert: " + toType);
	}
    }


    /** Generate the code for an Apply node */
    Item genApply(Tree fun, Tree[] args, MSILType resType) {
	boolean tmpLastStatement = lastStatement; lastStatement = false;
	Symbol sym = fun.symbol();
// 	if (primitives.isPrimitive(sym)) {
// 	    log("genApply: primitive " + primitives.getPrimitive(sym).tag +
// 		" = " + dumpSym(sym));
// 	}

	switch (fun) {
	case Ident(_):
	    emitThis();
	    lastStatement = tmpLastStatement;
	    return check(invokeMethod(sym, args, resType, true));

	case Select(Tree qualifier, Name selector):

//    	    log("\tfun: " + Debug.show(fun));
//    	    log("\tqualifier: " + Debug.show(qualifier));
//    	    log("\tqualifier.symbol(): " + Debug.show(qualifier.symbol()));

//  	    log("\tqualifier.type: " + Debug.show(qualifier.type));

	    if (sym == primitives.BOX_UVALUE) {
		return items.StaticItem(MSILType.REF(SCALA_UNIT), RUNTIME_UNIT_VAL);
	    }
// 	    if (sym == global.primitives.AS_UVALUE) {
// 		return coerce(gen(qualifier, MSILType.VOID), MSILType.VOID);
// 	    }

	    if (sym == defs.EQEQ) {
		return genEq(qualifier, args[0]);
	    }
	    if (sym == defs.BANGEQ) {
		return negate(genEq(qualifier, args[0]));
	    }

	    if (sym == STRING_CONCAT) {
		assert args.length == 1;
		load(gen(qualifier));
		load(gen(args[0]));
		code.Emit(OpCodes.Call, TypeCreator.CONCAT_OBJECT_OBJECT);
		return items.StackItem(MSILType.STRING_REF);
	    }

	    if (isPrimitiveArrayOp(sym)) {
		Primitive prim = primitives.getPrimitive(sym);
		loadArgs(args, tc.getMethod(sym).GetParameters());
		Type elemType = getArrayType(prim);
		switch (prim) {
		case ZARRAY_LENGTH: case BARRAY_LENGTH: case SARRAY_LENGTH:
		case CARRAY_LENGTH: case IARRAY_LENGTH: case LARRAY_LENGTH:
		case FARRAY_LENGTH: case DARRAY_LENGTH: case OARRAY_LENGTH:
		    code.Emit(OpCodes.Ldlen);
		    return items.StackItem(MSILType.I4);
		case NEW_ZARRAY:
		case NEW_BARRAY:
		case NEW_SARRAY:
		case NEW_CARRAY:
		case NEW_IARRAY:
		case NEW_LARRAY:
		case NEW_FARRAY:
		case NEW_DARRAY:
		    //case NEW_OARRAY:
		    code.Emit(OpCodes.Newarr, elemType);
		    return items.StackItem(MSILType.ARRAY(type2MSILType(elemType)));

		case ZARRAY_GET: case BARRAY_GET: case SARRAY_GET:
		case CARRAY_GET: case IARRAY_GET: case LARRAY_GET:
		case FARRAY_GET: case DARRAY_GET: case OARRAY_GET:
		    emitLdelem(MSILArrayElemType(elemType));
		    return items.StackItem(MSILType.BOOL);

		case ZARRAY_SET: case BARRAY_SET: case SARRAY_SET:
		case CARRAY_SET: case IARRAY_SET: case LARRAY_SET:
		case FARRAY_SET: case DARRAY_SET: case OARRAY_SET:
		    emitStelem(MSILArrayElemType(elemType));
		    return items.VoidItem();
		}
	    }

  	    if (isPrimitiveOp(sym)) {
		assert args.length <= 1;
		Tree right = args.length > 0 ? args[0] : Tree.Empty;
		return primitiveOp(qualifier, sym, right, resType);
 	    }

	    switch (qualifier.type) {
	    case TypeRef(_, _, _):
	    case SingleType(_, _):

// 		log("genApply.Select: " + dumpSym(sym));
// 		log("\tfun: " + Debug.show(fun));
// 		log("\tqualifier: " + Debug.show(qualifier));
// 		log("\tqualifier.symbol(): " + Debug.show(qualifier.symbol()));
// 		log("\tqualifier.type: " + Debug.show(qualifier.type));

		MethodBase method = tc.getMethod(sym);
		boolean virtualCall = false;
		if (!method.IsStatic || becomesStatic(sym)) {
		    //log("genApply.Select: qualifier = " + Debug.show(qualifier));
		    //log("genApply.Select: qualifier.type() = " +
		    //Debug.show(qualifier.type()));
		    /// FIXME after the Super attribution is correct
		    switch (qualifier) {
		    case Super(_):
			load(items.SelfItem(tc.getType(currentClass)));
			break;
		    default:
			load(gen(qualifier));
			virtualCall = true;
		    }
		    //load(gen(qualifier));
		}
		lastStatement = tmpLastStatement;
		return check(invokeMethod(sym, args, resType, virtualCall));

	    default:
		throw new ApplicationError();
	    } // switch(qualifier.type)

 	case TypeApply(Tree tfun, Tree[] targs):
	    final Symbol tsym = tfun.symbol();
	    if (primitives.isPrimitive(tsym)) {
// 		log("genApply.TypeApply: primitive " + primitives.getPrimitive(tsym).tag +
// 		    " = " + dumpSym(tsym));
		return primitiveOp(((Select)tfun).qualifier, tsym, targs[0], resType);
	    }
	    log("genApply.TypeApply.Select: Dunno how to process TypeApply: "
		+ Debug.show(fun));
	    log("\tTypeApply.fun: " + Debug.show(tfun));
	    log("\tTypeApply.fun.symbol(): " + tsym);
	    throw new ApplicationError();

	default:
	    throw new ApplicationError
		("genApply: Unknown function node: " + Debug.show(fun));
	} // switch (fun)
    } //genApply()


    /** Generate code for scala's '==' */
    Item genEq(Tree left, Tree right) {
	LocalBuilder tmpLocal = (LocalBuilder)locals.get(defs.EQEQ);
	if (tmpLocal == null) {
	    tmpLocal = code.DeclareLocal(TypeCreator.SYSTEM_OBJECT);
	    locals.put(defs.EQEQ, tmpLocal);
	}
	Label l1 = code.DefineLabel(), l2 = code.DefineLabel();
	load(gen(left));
	load(gen(right));
	code.Emit(OpCodes.Stloc, tmpLocal);
	code.Emit(OpCodes.Dup);
	code.Emit(OpCodes.Ldnull);
	code.Emit(OpCodes.Bne_Un, l1);
	code.Emit(OpCodes.Ldloc, tmpLocal);
	code.Emit(OpCodes.Ceq);
	code.Emit(OpCodes.Br, l2);
	code.MarkLabel(l1);
	code.Emit(OpCodes.Ldloc, tmpLocal);
	code.Emit(OpCodes.Callvirt, TypeCreator.OBJECT_EQUALS);
	code.MarkLabel(l2);
	return items.StackItem(MSILType.BOOL);
    }


    MSILType arithmCoercion(MSILType t1, MSILType t2) {
	if (t1 == t2) return t1;
	int i, j, n = MSILType.ARITHM_PRECISION.length;
	for (i = 0; i < n; i++)
	    if (t1 == MSILType.ARITHM_PRECISION[i])
		break;
	for (j = 0; j < n; j++)
	    if (t2 == MSILType.ARITHM_PRECISION[j])
		break;
	if (i >= n || j >= n)
	    log("arithmCoercion(): cannot find coercion for (" +
		t1 + ", " + t2 + ")");
	else
	    return MSILType.ARITHM_PRECISION[Math.max(i, j)];
	return null;
    }

    public boolean isPrimitiveOp(Symbol sym) {
	switch (primitives.getPrimitive(sym)) {
	case POS: case NEG: case NOT:
	case ADD: case SUB: case MUL: case DIV: case MOD:
	case OR: case XOR: case AND:
	case LSL: case LSR: case ASR:
	case EQ: case NE: case LT: case LE: case GT: case GE:
	case ZNOT: case ZOR: case ZAND:
	case IS: case AS:
	case CONCAT:
	case THROW:
	    return true;
	default:
	    return false;
	}
    }

    public boolean isPrimitiveArrayOp(Symbol sym) {
	switch (primitives.getPrimitive(sym)) {
	case NEW_ZARRAY: case NEW_BARRAY: case NEW_SARRAY:
	case NEW_CARRAY: case NEW_IARRAY: case NEW_LARRAY:
	case NEW_FARRAY: case NEW_DARRAY: //case NEW_OARRAY:
	case ZARRAY_LENGTH: case BARRAY_LENGTH: case SARRAY_LENGTH:
	case CARRAY_LENGTH: case IARRAY_LENGTH: case LARRAY_LENGTH:
	case FARRAY_LENGTH: case DARRAY_LENGTH: case OARRAY_LENGTH:
	case ZARRAY_GET: case BARRAY_GET: case SARRAY_GET:
	case CARRAY_GET: case IARRAY_GET: case LARRAY_GET:
	case FARRAY_GET: case DARRAY_GET: case OARRAY_GET:
	case ZARRAY_SET: case BARRAY_SET: case SARRAY_SET:
	case CARRAY_SET: case IARRAY_SET: case LARRAY_SET:
	case FARRAY_SET: case DARRAY_SET: case OARRAY_SET:
	    return true;
	default:
	    return false;
	}
    }

    //------------------------------------------------------------------------
    public Item primitiveOp(Tree left, Symbol opSym, Tree right,
			    MSILType resType)
    {
	assert primitives.isPrimitive(opSym);
	Primitive op = primitives.getPrimitive(opSym);
// 	log("primaryOp(): primitive index = " + op.tag);
// 	log("\t" + dumpSym(opSym));

	// treat some special cases
	switch (op) {
	case BOX:
	    return invokeMethod(opSym, new Tree[]{right}, resType, false);

	case CONCAT:
// 	    log("primaryOp().CONCAT: string concat!");
	    load(gen(left));
	    load(gen(right));
	    code.Emit(OpCodes.Call, TypeCreator.CONCAT_OBJECT_OBJECT);
	    return items.StackItem(MSILType.STRING_REF);

	case ADD:
	    if (tc.getTypeFromType(right.type) == TypeCreator.SYSTEM_STRING) {
		log("primaryOp().ADD: string concat!");
		load(gen(left));
		load(gen(right));
		code.Emit(OpCodes.Call, TypeCreator.CONCAT_OBJECT_OBJECT);
		return items.StackItem(MSILType.STRING_REF);
	    }
	    break;

	case IS:
// 	    log("primitiveOp(): Any.is");
	    load(gen(left));
	    final Type type = tc.getTypeFromType(right.type);
	    code.Emit(OpCodes.Isinst, type);
	    return mkCond(items.StackItem(MSILType.REF(type)));

	case AS:
// 	    log("primitiveOp(): Any.as");
	    Item i = load(gen(left));
	    final Type type = tc.getTypeFromType(right.type);
	    if (!i.type.isType(type) && type != TypeCreator.SYSTEM_OBJECT) {
		//log("genApply: casting item: " + i + " to type: " + type);
		code.Emit(OpCodes.Castclass, type);
	    }
	    return items.StackItem(MSILType.REF(type));
	}

	Item iLeft = null;

	switch (left) {
	case Apply(Tree fun , Tree[] args):
	    Primitive p = primitives.getPrimitive(fun.symbol());
	    //log("primitiveOp: primitive.tag = " + p.tag);
	    switch (p) {
	    case BOX:
		assert args.length == 1;
		iLeft = gen(args[0]);
		break;
	    default:
		iLeft = gen(left);
	    }
	    break;
	default:
	    iLeft = gen(left);
	}

	switch (op) {
	case THROW:
	    //log("primitiveOp().THROW: " + Debug.show(left));
	    load(iLeft);
	    code.Emit(OpCodes.Throw);
	    code.Emit(OpCodes.Ldnull);
	    return items.StackItem(MSILType.NULL_REF);

	case POS: return load(coerce(iLeft, resType));
	case NEG:
	    iLeft = load(coerce(iLeft, resType));
	    code.Emit(OpCodes.Neg);
	    return iLeft;
	case NOT:
	    iLeft = load(coerce(iLeft, resType));
	    code.Emit(OpCodes.Not);
	    return iLeft;
	case ZNOT:
	    return negate(mkCond(iLeft));
	case ZOR:
	    Item.CondItem lcond = mkCond(iLeft), rcond = null;
	    Chain success = lcond.success, failure = lcond.failure;
	    switch (lcond.test) {
	    case True:
		return lcond;
	    case False:
 		return mkCond(gen(right, MSILType.BOOL));
	    case And(Test t):
		success = (success != null) ?
		    success : new Chain(code.DefineLabel(), null);
		branch(negate(lcond.test), success);
		resolve(failure);
		failure = null;
		break;
	    default:
		success = (success != null) ?
		    success : new Chain(code.DefineLabel(), null);
		branch(negate(lcond.test), success);
	    }
	    rcond = mkCond((gen(right, MSILType.BOOL)));
	    rcond = items.CondItem(Test.Or(rcond.test),
				   merge(success, rcond.success),
				   merge(failure, rcond.failure));
	    return rcond;
	case ZAND:
	    Item.CondItem lcond = mkCond(iLeft), rcond = null;
	    Chain success = lcond.success, failure = lcond.failure;
	    switch (lcond.test) {
	    case False:
		return lcond;
	    case True:
 		return mkCond(gen(right, MSILType.BOOL));
	    case Or(Test t):
		failure = (failure != null) ?
		    failure : new Chain(code.DefineLabel(), null);
		branch(lcond.test, failure);
		resolve(success);
		success = null;
		break;
	    default:
		failure = (failure != null) ?
		    failure : new Chain(code.DefineLabel(), null);
		branch(lcond.test, failure);
	    }
	    rcond = mkCond(gen(right, MSILType.BOOL));
	    rcond = items.CondItem(Test.And(rcond.test),
				   merge(success, rcond.success),
				   merge(failure, rcond.failure));
	    return rcond;
	}
	MSILType toType = arithmCoercion(primitiveType(left.type),
					 primitiveType(right.type));
	load(coerce(iLeft, toType));
	load(coerce(gen(right, toType), toType));
	switch (op) {
	case ADD: code.Emit(OpCodes.Add); return items.StackItem(resType);
	case SUB: code.Emit(OpCodes.Sub); return items.StackItem(resType);
	case MUL: code.Emit(OpCodes.Mul); return items.StackItem(resType);
	case DIV: code.Emit(OpCodes.Div); return items.StackItem(resType);
	case MOD: code.Emit(OpCodes.Rem); return items.StackItem(resType);

	case OR:  code.Emit(OpCodes.Or);  return items.StackItem(resType);
	case XOR: code.Emit(OpCodes.Xor); return items.StackItem(resType);
	case AND: code.Emit(OpCodes.And); return items.StackItem(resType);

	case LSL: code.Emit(OpCodes.Shl); return items.StackItem(resType);
	case LSR: code.Emit(OpCodes.Shr); return items.StackItem(resType);
	case ASR: code.Emit(OpCodes.Shr_Un); return items.StackItem(resType);

	case EQ: return items.CondItem(Test.Binary(Test.EQ, toType), null, null);
	case NE: return items.CondItem(Test.Binary(Test.NE, toType), null, null);
	case LT: return items.CondItem(Test.Binary(Test.LT_IS, toType), null, null);
	case LE: return items.CondItem(Test.Binary(Test.LE_IS, toType), null, null);
	case GT: return items.CondItem(Test.Binary(Test.GT_IS, toType), null, null);
	case GE: return items.CondItem(Test.Binary(Test.GE_IS, toType), null, null);

	}

	log("primitiveOp(): dunno what to do with primitive: " + op.tag);
	return items.StackItem(resType);
    }


    void loadArgs(Tree[] args, ParameterInfo[] params) {
	boolean tmpLastStatement = lastStatement;
	lastStatement = false;
	for (int i = 0; i < args.length; i++) {
	    MSILType toType = type2MSILType(params[i].ParameterType); //type2MSILType(args[i].type);
	    load(coerce(gen(args[i], toType), toType));
	}
	lastStatement = tmpLastStatement;
    }

    protected boolean isStaticMember(Symbol sym) {
	return sym.owner().isModuleClass() && sym.owner().isJava();
    }

    private boolean becomesStatic(Symbol sym) {
	MethodBase method = tc.getMethod(sym);
	//return method.IsStatic && method.DeclaringType == TypeCreator.MONITOR;
	boolean becomesStatic = !isStaticMember(sym) && method.IsStatic;
// 	if (becomesStatic) {
// 	    log("method becomes static from: " + dumpSym(sym));
// 	    log("the new method is " + method);
// 	}
	return becomesStatic;
    }

    /** Generate code for method invocation
     */
    Item invokeMethod(Symbol fun, Tree[] args, MSILType resType,
		      boolean virtualCall) {
	//log("invokeMethod: " + dumpSym(fun));
	MethodBase method = tc.getMethod(fun);
	assert method != null : "Coudn't resolve method: " + dumpSym(fun);
	//log("\tmethod found: " + method);
	Item res = null;
	if (method.IsStatic) {
	    ParameterInfo[] params = method.GetParameters();
	    if (becomesStatic(fun)) {
		assert params.length == args.length + 1;
		ParameterInfo[] newParams = new ParameterInfo[params.length - 1];
		for (int i = 0; i < newParams.length; i++)
		    newParams[i] = params[i + 1];
		params = newParams;
	    }
	    loadArgs(args, params);
	    code.Emit(OpCodes.Call, (MethodInfo)method);
	    res = returnsVoid(method) ? items.VoidItem() :
		items.StackItem(resType);
	} else if (method.IsConstructor) {
	    // used only for calls to super constructor
	    //emitThis();
	    loadArgs(args, method.GetParameters());
	    code.Emit(OpCodes.Call, (ConstructorInfo)method);
	    res = items.VoidItem();
	} else {
	    loadArgs(args, method.GetParameters());
	    if (enableTailCalls && lastStatement && method == currentMethod)
		code.Emit(OpCodes.Tailcall);
	    code.Emit(virtualCall ? OpCodes.Callvirt : OpCodes.Call,
		      (MethodInfo)method);
	    res = returnsVoid(method) ? items.VoidItem() : items.StackItem(resType);
	}
	return check(res);
    }

    public MSILType type2MSILType(scalac.symtab.Type type) {
	switch (type) {
	case UnboxedType(int kind):
	    return MSILType.fromKind(kind);
	case TypeRef(_, Symbol s, _):
	    //log("TypeRef: " + dumpSym(s));
	    return MSILType.REF(tc.getType(s));
	case ThisType(Symbol s):
	    //log("TypeRef: " + dumpSym(s));
	    return MSILType.REF(tc.getType(s));
	case UnboxedArrayType(scalac.symtab.Type t):
	    return MSILType.ARRAY(type2MSILType(t));
	default:
	    log("type2MSILType: don't know how to convert type " + Debug.show(type));
	    //return MSILType.NULL_REF;
	    return MSILType.OBJECT;
	    //logErr("type2MSILType: " + Debug.show(type));
	    //throw new ApplicationError();
	}
    }

    public MSILType type2MSILType(Type type) {
	if (type == TypeCreator.BYTE)   return MSILType.I1;
	if (type == TypeCreator.SHORT)  return MSILType.I2;
	if (type == TypeCreator.INT)    return MSILType.I4;
	if (type == TypeCreator.LONG)   return MSILType.I8;
	if (type == TypeCreator.FLOAT)  return MSILType.R4;
	if (type == TypeCreator.DOUBLE) return MSILType.R8;
	if (type == TypeCreator.CHAR)   return MSILType.CHAR;
	if (type == TypeCreator.BOOLEAN)return MSILType.BOOL;
	if (type == TypeCreator.SYSTEM_STRING) return MSILType.STRING_REF;
	MSILType mtype =  MSILType.REF(type);
	//log("type2MSILType: convert " + type + " to " + mtype);
	return mtype;
    }

    public MSILType primitiveType(scalac.symtab.Type type) {
	MSILType mtype = type2MSILType(type);
	switch (mtype) {
	case REF(Type t):
	    if (t == TypeCreator.SCALA_BYTE)    return MSILType.I1;
	    if (t == TypeCreator.SCALA_SHORT)   return MSILType.I2;
	    if (t == TypeCreator.SCALA_INT)     return MSILType.I4;
	    if (t == TypeCreator.SCALA_LONG)    return MSILType.I8;
	    if (t == TypeCreator.SCALA_FLOAT)   return MSILType.R4;
	    if (t == TypeCreator.SCALA_DOUBLE)  return MSILType.R8;
	    if (t == TypeCreator.SCALA_CHAR)    return MSILType.CHAR;
	    if (t == TypeCreator.SCALA_BOOLEAN) return MSILType.BOOL;
	    return type2MSILType(t);
	case ARRAY(_): log("primitiveType: cannot convert " + mtype); return null;
	default: return mtype;
	}
    }

    public MSILType MSILArrayElemType(Type type) {
	MSILType mtype = type2MSILType(type);
	switch (mtype) {
	case BOOL: return MSILType.I1;
	case CHAR: return MSILType.I4;
	default: return mtype;
	}
    }

    public Type getArrayType(Primitive p) {
	switch (p) {
	case NEW_ZARRAY: case ZARRAY_LENGTH: case ZARRAY_GET: case ZARRAY_SET:
	    return TypeCreator.BOOLEAN;
	case NEW_BARRAY: case BARRAY_LENGTH: case BARRAY_GET: case BARRAY_SET:
	    return TypeCreator.BYTE;
	case NEW_SARRAY: case SARRAY_LENGTH: case SARRAY_GET: case SARRAY_SET:
	    return TypeCreator.SHORT;
	case NEW_CARRAY: case CARRAY_LENGTH: case CARRAY_GET: case CARRAY_SET:
	    return TypeCreator.CHAR;
	case NEW_IARRAY: case IARRAY_LENGTH: case IARRAY_GET: case IARRAY_SET:
	    return TypeCreator.INT;
	case NEW_LARRAY: case LARRAY_LENGTH: case LARRAY_GET: case LARRAY_SET:
	    return TypeCreator.LONG;
	case NEW_FARRAY: case FARRAY_LENGTH: case FARRAY_GET: case FARRAY_SET:
	    return TypeCreator.FLOAT;
	case NEW_DARRAY: case DARRAY_LENGTH: case DARRAY_GET: case DARRAY_SET:
	    return TypeCreator.DOUBLE;
	case NEW_OARRAY: case OARRAY_LENGTH: case OARRAY_GET: case OARRAY_SET:
	    return TypeCreator.SYSTEM_OBJECT;
	}
	log("getArrayElemType(): unknown primitive " + p.tag);
	return null;
    }

    public Item emitLdelem(MSILType type) {
	switch (type) {
	case I1: code.Emit(OpCodes.Ldelem_I1); break;
	case I2: code.Emit(OpCodes.Ldelem_I2); break;
	case I4: code.Emit(OpCodes.Ldelem_I4); break;
	case I8: code.Emit(OpCodes.Ldelem_I8); break;
	case R4: code.Emit(OpCodes.Ldelem_R4); break;
	case R8: code.Emit(OpCodes.Ldelem_R8); break;
	case ARRAY(_):
	case REF(_):
	    code.Emit(OpCodes.Ldelem_Ref); break;
	}
	return items.StackItem(type);
    }

    public Item emitStelem(MSILType type) {
	switch (type) {
	case I1: code.Emit(OpCodes.Stelem_I1); break;
	case I2: code.Emit(OpCodes.Stelem_I2); break;
	case I4: code.Emit(OpCodes.Stelem_I4); break;
	case I8: code.Emit(OpCodes.Stelem_I8); break;
	case R4: code.Emit(OpCodes.Stelem_R4); break;
	case R8: code.Emit(OpCodes.Stelem_R8); break;
	case ARRAY(_):
	case REF(_):
	    code.Emit(OpCodes.Stelem_Ref); break;
	}
	return items.StackItem(type);
    }

    /** load an item onto the stack
     */
    public Item load(Item that) {
	switch (that) {
 	case VoidItem():
 	    return that;

	case StackItem():
	    return (StackItem) that;

	case LiteralItem(MSILType type, Object value):
	    return loadLiteral(type, value);

	case SelfItem():
	    emitThis();
	    return items.StackItem(that.type);

	case ArgItem(int slot):
	    if (slot > 255)
		code.Emit(OpCodes.Ldarg, slot);
	    else if(slot > 3)
		code.Emit(OpCodes.Ldarg_S, slot);
	    else switch (slot) {
	    case 0: code.Emit(OpCodes.Ldarg_0); break;
	    case 1: code.Emit(OpCodes.Ldarg_1); break;
	    case 2: code.Emit(OpCodes.Ldarg_2); break;
	    case 3: code.Emit(OpCodes.Ldarg_3); break;
	    }
	    return items.StackItem(that.type);

	case LocalItem(LocalBuilder local):
	    code.Emit(OpCodes.Ldloc, local);
	    return items.StackItem(that.type);

	case StaticItem(FieldInfo field):
	    code.Emit(OpCodes.Ldsfld, field);
	    return items.StackItem(that.type);

	case SelectItem(Item qual, FieldInfo field):
	    Item i = load(qual);
// 	    switch (i.type) {
// 	    case REF(Type t):
// 		if (t.IsInterface) {
// 		    Symbol s = tc.getSymbol(t);
// 		    s = (Symbol) global.PHASE.ADDINTERFACES.interfaceToClass.get(s);
// 		    Type classType = tc.getType(s);
// 		    log("load.SelectItem: warning: inserting class cast from " + t + " to " + classType);
// 		    code.Emit(OpCodes.Castclass, classType);
// 		}
// 		break;
// 	    default:
// 	    }
	    code.Emit(OpCodes.Ldfld, field);
	    //log("load(Item): SelectItem; type = " + that.type);
	    return items.StackItem(that.type);

	case CondItem(Test test, Chain success, Chain failure):
	    load(test);
	    switch (test) {
	    case Or(_):
	    case And(_):
		//log("Loading " + that);
		Label exit = null;
		if (lastStatement)
		    code.Emit(OpCodes.Ret);
		else {
		    exit = code.DefineLabel();
		    code.Emit(OpCodes.Br, exit);
		}
		if (failure != null) {
		    resolve(failure);
		    load(FALSE_ITEM);
		    if (success != null) {
			if (lastStatement)
			    code.Emit(OpCodes.Ret);
			else
			    code.Emit(OpCodes.Br, exit);
		    }
		}
		if (success != null) {
		    resolve(success);
		    load(TRUE_ITEM);
		}
		resolve(exit);
		//log("Loading " + that);
		break;
	    }
	    return items.StackItem(MSILType.BOOL);

	default:
	    throw new ApplicationError("load item: " + that);
	}
    }


    /**
     */
    void load(Test test) {
	switch (test) {
	case False:
	    code.Emit(OpCodes.Ldc_I4_0);
	    break;
	case True:
	    code.Emit(OpCodes.Ldc_I4_1);
	    break;
	case Bool(boolean value):
	    if (value) negate_load();
	    break;
	case Binary(int opcode, MSILType type):
	    code.Emit(load(opcode));
	    if (negate_load(opcode)) negate_load();
	    break;
	case And(Test t):
	    load(t);
	    break;
	case Or(Test t):
	    load(t);
	    break;
	default:
	    throw new ApplicationError(test.getClass().getName());
	}
    }

    /**
     * Generate the code for a literal
     */
    protected Item.StackItem loadLiteral(MSILType type, Object obj) {
	switch (type) {
	case I1:
	case I2:
	case I4:
	case CHAR:
	    int i = (type == MSILType.CHAR) ?
		(int)((Character) obj).charValue() :
		((Number)obj).intValue();
	    switch (i) {
	    case -1:code.Emit(OpCodes.Ldc_I4_M1); break;
	    case 0: code.Emit(OpCodes.Ldc_I4_0); break;
	    case 1: code.Emit(OpCodes.Ldc_I4_1); break;
	    case 2: code.Emit(OpCodes.Ldc_I4_2); break;
	    case 3: code.Emit(OpCodes.Ldc_I4_3); break;
	    case 4: code.Emit(OpCodes.Ldc_I4_4); break;
	    case 5: code.Emit(OpCodes.Ldc_I4_5); break;
	    case 6: code.Emit(OpCodes.Ldc_I4_6); break;
	    case 7: code.Emit(OpCodes.Ldc_I4_7); break;
	    case 8: code.Emit(OpCodes.Ldc_I4_8); break;
	    default:
		if (i >= -128 && i <= 127)
		    code.Emit(OpCodes.Ldc_I4_S, i);
		else
		    code.Emit(OpCodes.Ldc_I4, i);
	    }
	    break;
	case I8:
	    code.Emit(OpCodes.Ldc_I8, ((Number)obj).longValue());
	    break;
	case R4:
	    code.Emit(OpCodes.Ldc_R4, ((Number)obj).floatValue());
	    break;
	case R8:
	    code.Emit(OpCodes.Ldc_R8, ((Number)obj).doubleValue());
	    break;
 	case BOOL:
	    if (((Boolean)obj).booleanValue())
		code.Emit(OpCodes.Ldc_I4_1);
	    else
		code.Emit(OpCodes.Ldc_I4_0);
	    break;
	case VOID:
	    code.Emit(OpCodes.Ldsfld, RUNTIME_UNIT_VAL);
	    break;
	case REF(Type refType):
	    if (obj == null) {
		code.Emit(OpCodes.Ldnull);
	    } else if (refType == TypeCreator.SYSTEM_STRING) {
		code.Emit(OpCodes.Ldstr, obj.toString());
	    } else {
		throw new ApplicationError
		    ("loadLiteral(): unexpected literal type: " + refType +
		     "; value = " + obj);
	    }
	    break;
	default:
	    throw new ApplicationError
		("loadLiteral(): Unknown literal type: " + type);
	}
	return items.StackItem(type);
    } // genLiteral()


    /**
     */
    public Item store(Item that) {
	switch (that) {
	case ArgItem(int slot):
	    code.Emit(OpCodes.Starg, slot);
	    break;

	case LocalItem(LocalBuilder local):
	    code.Emit(OpCodes.Stloc, local);
	    break;

	case StaticItem(FieldInfo field):
	    code.Emit(OpCodes.Stsfld, field);
	    break;

	case SelectItem(Item qual, FieldInfo field):
	    load(qual);
	    code.Emit(OpCodes.Stfld, field);
	    break;

	default:
	    throw new ApplicationError("Cannot store item: " + that);
	}
	return items.VoidItem();
    }

    /**
     */
    Item drop(Item that) {
	switch (that) {
	case VoidItem():
	case SelfItem():
	case LiteralItem(_, _):
	case ArgItem(_):
	case LocalItem(_):
	case StaticItem(_):
	    break;
	case StackItem():
	    code.Emit(OpCodes.Pop);
	    break;
 	case SelectItem(Item qual, _):
 	    drop(qual);
 	    break;
 	case CondItem(Test test, _, _):
	    switch (test) {
	    case False:
	    case True:
		break;
	    case Bool(_):
		code.Emit(OpCodes.Pop);
		break;
	    case And(_):
	    case Or(_):
		drop(load(that));
		break;
	    case Binary(_, _):
		code.Emit(OpCodes.Pop);
		code.Emit(OpCodes.Pop);
		break;
	    default :
		throw new ApplicationError(that.getClass().getName());
	    }
 	    break;
	default:
	    drop(load(that));
	}
	return Item.VoidItem();
    }

    public Item.CondItem mkCond(Item that) {
	switch (that) {
	case CondItem(_, _, _): return (Item.CondItem) that;
	default:
	    load(that);
	    return items.CondItem(Test.Bool(false), null, null);
	}
    }

    public Item.CondItem negate(Item item) {
	Item.CondItem cond = mkCond(item);
	// swap the failure and success chains
	return items.CondItem(negate(cond.test), cond.success, cond.failure);
    }

    void negate_load() {
	code.Emit(OpCodes.Ldc_I4_1);
	code.Emit(OpCodes.Xor);
    }


    public Test negate(Test that) {
        switch (that) {

        case False:
            return Test.True;

        case True:
            return Test.False;

        case Bool(boolean value):
            return Test.Bool(!value);

	case And(Test test):
	    return Test.Or(negate(test));

	case Or(Test test):
	    return Test.And(negate(test));

        case Binary(int opcode, MSILType type):
            return Test.Binary(negate(opcode), type);

        default:
            throw new ApplicationError(that.getClass().getName());
        }
    }


    Label branch(Test test, Chain chain) {
	assert chain != null;
	Label label = chain.label;
	switch (test) {
	case False:
	    code.Emit(OpCodes.Br, label);
	    return label;
	case True:
	    return null;
	case Bool(boolean value):
	    code.Emit(value ? OpCodes.Brtrue : OpCodes.Brfalse, label);
	    return label;
	case And(Test t):
	    return branch(t, chain);
	case Or(Test t):
	    return branch(t, chain);
	case Binary(int opcode, MSILType type):
	    code.Emit(branch(negate(opcode)), label);
	    return label;
	default:
	    throw new ApplicationError();
	}
    }


    static Chain merge(Chain c1, Chain c2) {
	if (c1 == null) return c2;
	if (c2 == null) return c1;
	Chain c = c1;
	for(; c.next != null; c = c.next) ;
	c.next = c2;
	return c1;
    }

    void resolve(Chain c) {
	for (; c != null; c = c.next)
	    code.MarkLabel(c.label);
    }

    void resolve(Label l) {
	if (l != null)
	    code.MarkLabel(l);
    }

    public static boolean negate_load(int opcode) {
        switch (opcode) {
        case Test.LT_IS: return false;
        case Test.LE_IS: return true;
        case Test.GT_IS: return false;
        case Test.GE_IS: return true;
        case Test.LT_IU: return false;
        case Test.LE_IU: return true;
        case Test.GT_IU: return false;
        case Test.GE_IU: return true;
        case Test.LT_RO: return false;
        case Test.LE_RO: return true;
        case Test.GT_RO: return false;
        case Test.GE_RO: return true;
        case Test.LT_RU: return false;
        case Test.LE_RU: return true;
        case Test.GT_RU: return false;
        case Test.GE_RU: return true;
        case Test.EQ   : return false;
        case Test.NE   : return true;
        default        : throw new ApplicationError("" + opcode);
        }
    }

    public static OpCode load(int opcode) {
        switch (opcode) {
        case Test.LT_IS: return OpCodes.Clt;
        case Test.LE_IS: return OpCodes.Cgt;    // negate
        case Test.GT_IS: return OpCodes.Cgt;
        case Test.GE_IS: return OpCodes.Clt;    // negate
        case Test.LT_IU: return OpCodes.Clt_Un;
        case Test.LE_IU: return OpCodes.Cgt_Un; // negate
        case Test.GT_IU: return OpCodes.Cgt_Un;
        case Test.GE_IU: return OpCodes.Clt_Un; // negate
        case Test.LT_RO: return OpCodes.Clt;
        case Test.LE_RO: return OpCodes.Cgt_Un; // negate
        case Test.GT_RO: return OpCodes.Cgt;
        case Test.GE_RO: return OpCodes.Clt_Un; // negate
        case Test.LT_RU: return OpCodes.Clt_Un;
        case Test.LE_RU: return OpCodes.Cgt;    // negate
        case Test.GT_RU: return OpCodes.Cgt_Un;
        case Test.GE_RU: return OpCodes.Clt;    // negate
        case Test.EQ   : return OpCodes.Ceq;
        case Test.NE   : return OpCodes.Ceq;    // negate
        default        : throw new ApplicationError("" + opcode);
        }
    }

    public static int negate(int opcode) {
        switch (opcode) {
        case Test.LT_IS: return Test.GE_IS;
        case Test.LE_IS: return Test.GT_IS;
        case Test.GT_IS: return Test.LE_IS;
        case Test.GE_IS: return Test.LT_IS;
        case Test.LT_IU: return Test.GE_IU;
        case Test.LE_IU: return Test.GT_IU;
        case Test.GT_IU: return Test.LE_IU;
        case Test.GE_IU: return Test.LT_IU;
        case Test.LT_RO: return Test.GE_RU;
        case Test.LE_RO: return Test.GT_RU;
        case Test.GT_RO: return Test.LE_RU;
        case Test.GE_RO: return Test.LT_RU;
        case Test.LT_RU: return Test.GE_RO;
        case Test.LE_RU: return Test.GT_RO;
        case Test.GT_RU: return Test.LE_RO;
        case Test.GE_RU: return Test.LT_RO;
        case Test.EQ   : return Test.NE;
        case Test.NE   : return Test.EQ;
        default        : throw new ApplicationError("" + opcode);
        }
    }

    public static OpCode branch(int opcode) {
        switch (opcode) {
        case Test.LT_IS:
        case Test.LT_RO: return OpCodes.Blt;
        case Test.LE_IS:
        case Test.LE_RO: return OpCodes.Ble;
        case Test.GT_IS:
        case Test.GT_RO: return OpCodes.Bgt;
        case Test.GE_IS:
        case Test.GE_RO: return OpCodes.Bge;
        case Test.LT_IU:
        case Test.LT_RU: return OpCodes.Blt_Un;
        case Test.LE_IU:
        case Test.LE_RU: return OpCodes.Ble_Un;
        case Test.GT_IU:
        case Test.GT_RU: return OpCodes.Bgt_Un;
        case Test.GE_IU:
        case Test.GE_RU: return OpCodes.Bge_Un;
        case Test.EQ   : return OpCodes.Beq;
        case Test.NE   : return OpCodes.Bne_Un;
        default        : throw new ApplicationError("" + opcode);
        }
    }

    ///////////////////////////////////////////////////////////////////////////

    public static String dumpSym(Symbol sym) {
	if (sym == null) return "<null>";
	if (sym == Symbol.NONE) return "NoSymbol";
	return "symbol = " + Debug.show(sym) +
	    "; owner = " + Debug.show(sym.owner()) +
	    "; info = " + Debug.show(sym.info()) +
	    "; kind = " + sym.kind +
	    "; flags = " + Integer.toHexString(sym.flags);
    }

    /**
     */
    void log(String message) {
        global.reporter.printMessage(message);
    }

    void logErr(int pos, String message) {
        global.reporter.printMessage(currUnit.position(pos), message);
    }

    void logErr(String message) {
	if (code != null) {
	    MethodBase method = code.owner;
	    System.err.print("Processing " + method.DeclaringType.FullName +
			     "::" + method.Name + " -> ");
	}
	System.err.println(message);
	throw new ApplicationError();
	//log(1, message);
    }

    private static final int DEBUG_LEVEL = 2;

    private void log(int level, String message) {
	if (level < DEBUG_LEVEL)
	    global.log(message);
    }


    void emitComment(String str) {
	code.Emit(OpCodes.Ldstr, str);
	code.Emit(OpCodes.Pop);
    }

} // class GenMSIL


public class MSILType {
    public case BOOL;
    public case CHAR;
    public case I1;
    public case I2;
    public case U2;
    public case I4;
    public case I8;
    public case R4;
    public case R8;
    public case REF(Type t);
    public case ARRAY(MSILType t);
    public case VOID;

    public static final MSILType NULL_REF = REF(null);
    public static final MSILType OBJECT = REF(TypeCreator.SYSTEM_OBJECT);
    public static final MSILType STRING_REF = REF(TypeCreator.SYSTEM_STRING);
    public static final MSILType [] ARITHM_PRECISION =
	new MSILType[] {I1, I2, I4, I8, R4, R8};

    public static MSILType fromKind(int kind) {
	switch (kind) {
	case TypeTags.BYTE: return I1;
	case TypeTags.CHAR: return CHAR;
	case TypeTags.SHORT: return I2;
	case TypeTags.INT: return I4;
	case TypeTags.LONG: return I8;
	case TypeTags.FLOAT: return R4;
	case TypeTags.DOUBLE: return R8;
	case TypeTags.BOOLEAN: return BOOL;
	case TypeTags.UNIT: return VOID;
	case TypeTags.STRING: return STRING_REF;
	default:
	    throw new ApplicationError("Unknown kind: " + kind);

	}
    }


    public boolean isType(Type type) {
	switch (this) {
	case REF(Type t): return (type == t);
	default:
	    return false;
	}
    }

    public String toString() {
        switch (this) {
        case I1: return "I1";
        case I2: return "I2";
        case U2: return "U2";
        case I4: return "I4";
        case I8: return "I8";
        case R4: return "R4";
        case R8: return "R8";
        case BOOL: return "BOOL";
	case CHAR: return "CHAR";
        case REF(Type t): return "REF(" + t + ")";
	case ARRAY(MSILType t): return "ARRAY(" + t + ")";
        case VOID: return "VOID";
        default: return getClass().getName();
        }
    }
}


/** class representing the items
 */
class Item {
    public MSILType type;

    public case VoidItem();
    public case StackItem();
    public case SelfItem();
    public case LiteralItem(MSILType typ, Object value);
    public case ArgItem(int slot);
    public case LocalItem(LocalBuilder local);
    public case StaticItem(FieldInfo field);
    public case SelectItem(Item qual, FieldInfo field);
    public case CondItem(Test test, Chain success, Chain failure);

    public String toString() {
	switch (this) {
	case VoidItem(): return "VoidItem: " + type;
	case StackItem(): return "StackItem: " + type ;
	case SelfItem(): return "this: " + type;
	case LiteralItem(_, Object value): return "LiteralItem(" + value + "): " + type;
	case ArgItem( int slot): return "ArgItem(" + slot + "): " + type;
	case LocalItem( LocalBuilder local): return "LocalItem(" + local + "): " + type;
	case StaticItem( FieldInfo field): return "StaticItem(" + field + "): " + type;
	case SelectItem(_, FieldInfo field): return "SelectItem(" + field + "): " +type;
	case CondItem(Test test, Chain success, Chain failure):
	    return "CondItem(" + test + ", " + success + ", " + failure + "): " + type;
	}
	return "??Item??";
    }
}


/** class implementing a chain (list) of labels
 */
class Chain {
    Label label;
    Chain next;
    Chain(Label l, Chain n) { label = l; next = n; }
}


class ItemFactory {
    GenMSIL coder;
    private static final Item.VoidItem VOID = Item.VoidItem();
    static { VOID.type = MSILType.VOID; }

    public ItemFactory(GenMSIL _coder) {
	coder = _coder;
    }
    public Item.VoidItem VoidItem() {
	return VOID;
    }
    public Item.StackItem StackItem(MSILType type) {
	Item.StackItem item =  Item.StackItem();
	item.type = type;
	return item;
    }
    public Item.SelfItem SelfItem(Type t) {
	Item.SelfItem item = Item.SelfItem();
	item.type = MSILType.REF(t);
	return item;
    }
    public Item.LiteralItem LiteralItem(MSILType type, Object value) {
	Item.LiteralItem item = Item.LiteralItem(type, value);
	item.type = type;
	return item;
    }
    public Item.ArgItem ArgItem(MSILType type, int slot) {
	Item.ArgItem item = Item.ArgItem(slot);
	item.type = type;
	return item;
    }
    public Item.LocalItem LocalItem(MSILType type, LocalBuilder local) {
	Item.LocalItem item = Item.LocalItem(local);
	item.type = type;
	return item;
    }
    public Item.StaticItem StaticItem(MSILType type, FieldInfo field) {
	assert field.IsStatic;
	Item.StaticItem item = Item.StaticItem(field);
	item.type = type;
	return item;
    }
    public Item.SelectItem SelectItem(MSILType type, Item qualifier, FieldInfo field) {
	assert !field.IsStatic;
	Item.SelectItem item =  Item.SelectItem(coder.load(qualifier), field);
	item.type = type;
	return item;
    }
    public Item.CondItem CondItem(Test test, Chain success, Chain failure) {
	Item.CondItem item = Item.CondItem(test, success, failure);
	item.type = MSILType.BOOL;
	return item;
    }
}

/** class representing the possible tests in conditional item
 */
public class Test {

    public case False;
    public case True;
    public case Bool(boolean value);
    public case Or(Test test);
    public case And(Test test);
    public case Binary(int opcode, MSILType type);

    public static final int LT_IS = 0x00;
    public static final int LE_IS = 0x01;
    public static final int GT_IS = 0x02;
    public static final int GE_IS = 0x03;

    public static final int LT_IU = 0x04;
    public static final int LE_IU = 0x05;
    public static final int GT_IU = 0x06;
    public static final int GE_IU = 0x07;

    public static final int LT_RO = 0x08;
    public static final int LE_RO = 0x09;
    public static final int GT_RO = 0x0A;
    public static final int GE_RO = 0x0B;

    public static final int LT_RU = 0x0C;
    public static final int LE_RU = 0x0D;
    public static final int GT_RU = 0x0E;
    public static final int GE_RU = 0x0F;

    public static final int EQ    = 0x10;
    public static final int NE    = 0x11;

    public static String toString(int opcode) {
        switch (opcode) {
        case LT_IS: return "GE_IS";
        case LE_IS: return "GT_IS";
        case GT_IS: return "LE_IS";
        case GE_IS: return "LT_IS";
        case LT_IU: return "GE_IU";
        case LE_IU: return "GT_IU";
        case GT_IU: return "LE_IU";
        case GE_IU: return "LT_IU";
        case LT_RO: return "GE_RU";
        case LE_RO: return "GT_RU";
        case GT_RO: return "LE_RU";
        case GE_RO: return "LT_RU";
        case LT_RU: return "GE_RO";
        case LE_RU: return "GT_RO";
        case GT_RU: return "LE_RO";
        case GE_RU: return "LT_RO";
        case EQ   : return "NE";
        case NE   : return "EQ";
        default   : throw new InternalError("" + opcode);
        }
    }

    public String toString() {
        switch (this) {

        case False:
            return "False";
        case True:
            return "True";
	case Bool(boolean value):
	    return "Test(" + value + ")";
	case Or(Test test):
	    return "Or(" + test + ")";
	case And(Test test):
	    return "And(" + test + ")";
	case Binary(int opcode, MSILType type):
	    return "Binary(" + toString(opcode) +"," + type + ")";

        default:
            throw new ApplicationError(getClass().getName());
        }
    }
} // class Test
