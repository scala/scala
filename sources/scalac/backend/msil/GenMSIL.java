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
import scalac.util.SourceRepresentation;
import scalac.ast.Tree;
import scalac.ast.TreeList;
import scalac.atree.AConstant;
import Tree.*;
import scalac.symtab.Symbol;
import scalac.symtab.Scope;
import scalac.symtab.Kinds;
import scalac.symtab.TypeTags;
import scalac.symtab.Modifiers;
import scalac.symtab.Definitions;
import scalac.symtab.classfile.Pickle;
import scalac.symtab.classfile.CLRPackageParser;
import Scope.SymbolIterator;

import scalac.backend.Primitive;
import scalac.backend.Primitives;

import scala.tools.util.Position;

import ch.epfl.lamp.compiler.msil.*;
import ch.epfl.lamp.compiler.msil.emit.*;

import Item.*;

import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.io.IOException;

/**
 * Generates MS IL code via Reflection.Emit-like API
 * (ch.epfl.lamp.compiler.msil.emit package)
 *
 * @author Nikolay Mihaylov
 */
public final class GenMSIL {

    //################################################################

    // The position of the paramater in the parameter list
    private final Map/*<Symbol, Integer>*/ params = new HashMap();

    // The LocalBuilder corresponding to the local variable symbol
    private final Map/*<Symbol, LocalBuilder>*/ locals = new HashMap();

    // mapping from LabelDef symbols to labels
    private final Map/*<Symbol, Label>*/ sym2label = new HashMap();

    // the Assembly the program is compiled into.
    private AssemblyBuilder currAssembly;

    // the main module of the assembly
    private ModuleBuilder currModule;

    // used by TypeCreator.createType()
    ModuleBuilder getCurrentModule() { return currModule; }

    // the code generator for the current method
    private ILGenerator code;

    private final Global global;
    private final TypeCreator tc;
    private final Definitions defs;
    private final Primitives primitives;
    private final ItemFactory items;

    private static final Item TRUE_ITEM  = Item.CondItem(Test.True, null, null);
    private static final Item FALSE_ITEM = Item.CondItem(Test.False, null, null);

    /**
     * The public constructor of the code generator.
     */
    public GenMSIL(Global global, GenMSILPhase phase) {
        this.global = global;
	this.defs = global.definitions;
	this.primitives = global.primitives;
	this.tc = new TypeCreator(global, this, phase);
	this.items = new ItemFactory(this);
    }

    /**
     * Initialize the code generator. Called from GenMSILPhase
     * before processing any compilation unit.
     */
    void initGen() {
	tc.init();
	AssemblyName an = new AssemblyName();
	an.Name = "prog";
	currAssembly = AssemblyBuilder.DefineDynamicAssembly(an);
	currModule = (ModuleBuilder) currAssembly.GetModule(an.Name);
	if (currModule == null) {
	    currModule = currAssembly.DefineDynamicModule(an.Name, an.Name + "Module");
	}
    }

    /** Finilize the code generation. Called from GenMSILPhase
     *  after processing all compilation units.
     */
    public void finalizeGen() {
	if (mainObjectField != null && mainMethod != null) {
	    MethodBuilder main = currModule.DefineGlobalMethod
		("Main", MethodAttributes.Public | MethodAttributes.Static,
		 tc.VOID, new Type[] {tc.STRING_ARRAY} );
	    main.DefineParameter(0, 0, "args");
	    ((AssemblyBuilder)currModule.Assembly).SetEntryPoint(main);
	    code = main.GetILGenerator();
	    code.setPosition(mainLineNum, mainSourceFilename);
	    code.Emit(OpCodes.Ldsfld, mainObjectField);
	    code.Emit(OpCodes.Ldarg_0);
	    code.Emit(OpCodes.Callvirt, mainMethod);
	    if (!returnsVoid(mainMethod))
		code.Emit(OpCodes.Pop);
	    code.Emit(OpCodes.Ret);
	}
	tc.createTypes();
	try { currAssembly.Save(currAssembly.GetName().Name + ".il"); }
	catch (IOException e) {
	    if (global.debug) e.printStackTrace(); // FIXME
	}
    }


    private FieldInfo mainObjectField = null;
    private MethodInfo mainMethod = null;
    private int mainLineNum;
    private String mainSourceFilename;

    /*
     * Check if the given method is a main function
     */
    private void checkMain(MethodBase method, int line) {
	if ( !currentClass.isModuleClass() )
	    return;
	// do not consider nested objects' main methods
	if (method.DeclaringType.DeclaringType != null)
	    return;
	if (method.IsConstructor() || method.IsAbstract() ||
	    !method.Name.equals("main"))
	    return;
	ParameterInfo[] params = method.GetParameters();
	if (params.length != 1 || params[0].ParameterType != tc.STRING_ARRAY)
	    return;
	mainObjectField = tc.getModuleField(currentClass);
	mainMethod = (MethodInfo) method;
	mainLineNum = line;
	mainSourceFilename = getSourceFilename();
    }

    private String getSourceFilename() {
	assert currUnit != null;
	return SourceRepresentation.escape(currUnit.source.getFile().getPath());
    }

    // keeps track of the current compilation unit for better error reporting
    private Unit currUnit;

    /**
     * The main entry point into the code generator. Called from GenMSILPhase
     * for every compilation unit.
     */
    public void apply(Unit unit) {
	currUnit = unit;
	try {
	    for (int i = 0; i < unit.body.length; i++) {
		Tree tree = unit.body[i];
		Symbol sym = tree.symbol();
		switch (tree) {
		case Empty: break;
		case ClassDef(_, _, _, _, _, Template(_, Tree[] body)):
		    genClass(sym, body);
		    break;
		case PackageDef(_, Template(_ , Tree[] body)):
		    genPackage(body);
		    break;
		default:
		    throw new ApplicationError
			("Illegal top-level definition: " + Debug.show(tree));
		}
	    }
	} catch (Throwable e) {
	    e.printStackTrace();
	    finalizeGen();
	    System.exit(1);
	}
    }

    /**
     * Generate the code for a Package definition.
     */
    private void genPackage(Tree[] body) {
	for (int i = 0; i < body.length; i++) {
	    Symbol sym = body[i].symbol();
	    switch (body[i]) {
	    case Empty:
		break;

	    case ClassDef(_, _, _, _, _, Template(_, Tree[] classBody)):
		Type type = tc.getType(sym);
		assert type instanceof TypeBuilder
		    : Debug.show(sym) + " => [" + type.Assembly + "]" + type;
		if (type instanceof TypeBuilder)
		    genClass(sym, classBody);
		break;

	    case PackageDef(_, Template(_ , Tree[] body2)):
		genPackage(body2);
		break;

	    default:
		throw new ApplicationError("Class definition expected: " +
					   Debug.show(sym));
	    }
	}
    }

    /** The symbol of the class that is currently being compiled. */
    private Symbol currentClass;

    /** The method that is currently being compiled. */
    private MethodBase currentMethod;

    /*
     * Emit the symbol table for the class given class as an MSIL attribute.
     */
    private void emitSymtab(Symbol clazz) {
	Pickle pickle = (Pickle)global.symdata.get(clazz);
        if (pickle != null) {
	    byte[] symtab = new byte[pickle.size() + 8];
	    symtab[0] = 1;
	    for (int size = pickle.size(), i = 2; i < 6; i++) {
		symtab[i] = (byte)(size & 0xff);
		size >>= 8;
	    }
	    System.arraycopy(pickle.bytes, 0, symtab, 6, pickle.size());
	    TypeBuilder type = (TypeBuilder) tc.getType(clazz);
	    if (clazz.isModuleClass()) {
		type = tc.getStaticType(clazz);
		type.setPosition(Position.line(clazz.pos), getSourceFilename());
	    }
	    type.SetCustomAttribute(tc.SCALA_SYMTAB_ATTR_CONSTR, symtab);
	}
    }

    /*
     * Generate the code for a class definition
     */
    private void genClass(Symbol clazz, Tree[] body) {
	Symbol outerClass = currentClass;
	currentClass = clazz;
	if (clazz.isModuleClass()) {
	    tc.getModuleField(clazz);
	}
	final TypeBuilder type = (TypeBuilder)tc.getType(clazz);
	type.setPosition(Position.line(clazz.pos), getSourceFilename());
	emitSymtab(clazz);
	for (int i = 0; i < body.length; i++) {
	    Symbol sym = body[i].symbol();
	    switch (body[i]) {
	    case Empty:
		break;

	    case ValDef(_, _, _, _):
		// just to generate the field declaration
		// the rhs should've been moved to the constructor body
 		tc.createField(sym);
		break;

	    case ClassDef(_, _, _, _, _, Template impl):
		genClass(sym, impl.body);
		break;

	    case DefDef(_, _, _, ValDef[][] vparams, Tree tpe, Tree rhs):
// 		if (!currentMethod.IsAbstract()) {
		if (!sym.isDeferred()) {
		    currentMethod = tc.getMethod(sym);
		    checkMain(currentMethod, Position.line(body[i].pos));
		    genDef(sym, vparams[0], rhs, msilType(tpe.type));
		}
		break;

	    default:
		assert false : "Illegal class body definition: "
		    + Debug.show(body[i]);
	    }
	}
	currentClass = outerClass;
    } //genClass()


    /*
     * Specifies if the currently processed tree node is the last expression
     * in a function so the generated code can be optimized: emitting ret
     * instead of branch, emitting tailcalls, etc.
     */
    private boolean lastExpr;

    /*
     * Specifies if the control flow may come through the current point
     * in the instruction sequence.
     */
    private boolean willReturn;

    private boolean enableTailCalls = true;

    /*
     * Generate code for constructors and methods.
     */
    private void genDef(Symbol sym, ValDef[] parameters, Tree rhs, MSILType toType) {
	MethodBase method = tc.getMethod(sym);

	params.clear();
	locals.clear();
	int argOffset = method.IsStatic() ? 0 : 1;
	for (int i = 0; i < parameters.length; i++) {
	    params.put(parameters[i].symbol(), new Integer(i + argOffset));
	}
	if (method.IsConstructor()) {
	    ConstructorInfo ctor = (ConstructorInfo) method;
	    code = ((ConstructorBuilder)ctor).GetILGenerator();
	    code.setPosition(Position.line(rhs.pos));
	    if (sym.owner().isModuleClass()
		&& sym.owner().owner().isPackageClass()) {
		Tree[] cstats = null;
		switch (rhs) {
		case Block(Tree[] stats, Tree value):
		    TreeList stms = new TreeList();
		    for (int i = 0; i < stats.length; i++) {
			switch (stats[i]) {
			case Block(Tree[] stats2, Tree value2):
			    stms.append(stats2);
			    stms.append(value2);
			    break;
			default:
			    stms.append(stats[i]);
			}
		    }
		    switch (value) {
		    case Block(Tree[] stats2, Tree value2):
			stms.append(stats2);
			stms.append(value2);
			break;
		    }
		    cstats = stms.toArray();
		    break;
		default:
		    cstats = new Tree[]{rhs};
		}

		// emit the call to the superconstructor
		drop(gen(cstats[0], MSILType.VOID));
		code.Emit(OpCodes.Ret); // conclude the instance constructor
		ConstructorBuilder cctor = ((TypeBuilder)(method.DeclaringType)).
		    DefineConstructor((short)(MethodAttributes.Static
					      | MethodAttributes.Public),
				      CallingConventions.Standard,
				      Type.EmptyTypes);
		currentMethod = cctor;
		code = cctor.GetILGenerator();
		code.setPosition(Position.line(sym.owner().pos));
		// initialize the static module reference
		code.Emit(OpCodes.Newobj, ctor);
		code.Emit(OpCodes.Stsfld, tc.getModuleField(currentClass));
		for (int i = 1; i < cstats.length; i++) {
		    drop(gen(cstats[i], MSILType.VOID));
		}
		code.Emit(OpCodes.Ret); // conclude the static constructor
	    } else {
		drop(gen(rhs, MSILType.VOID));
		code.Emit(OpCodes.Ret);
	    }
	} else if (!method.IsAbstract()) {
	    lastExpr = true;
	    code = ((MethodBuilder)method).GetILGenerator();
	    code.setPosition(Position.line(rhs.pos));
	    Item item = gen(rhs, toType);
	    if (returnsVoid(method))
		drop(item);
	    else
		coerce(load(item), toType); // FIXME: coerce???
	    code.Emit(OpCodes.Ret);
	    if (currentClass.isModuleClass()) {
		MethodBuilder staticMethod = tc.getStaticObjectMethod(sym);
		if (staticMethod != null) {
		    code = staticMethod.GetILGenerator();
		    code.setPosition(Position.line(sym.pos));
		    code.Emit(OpCodes.Ldsfld, tc.getModuleField(currentClass));
		    for (int i = 0; i < parameters.length; i++)
			emitLdarg(i);
		    code.Emit(OpCodes.Call, (MethodInfo)method);
		    code.Emit(OpCodes.Ret);
		}
	    }
	}

	lastExpr = false;
	code = null;
    } // genDef();


    /*
     * Check if the result type of a method is void
     */
    private boolean returnsVoid(MethodBase method) {
	return method.IsConstructor() ||
	    (((MethodInfo)method).ReturnType == tc.VOID);
    }


    /*
     * Emit the code for this.
     */
    private void emitThis() {
	if (currentMethod.IsStatic()) {
	    if (currentMethod.IsConstructor()) {
		code.Emit(OpCodes.Ldsfld, tc.getModuleField(currentClass));
	    } else
		throw new ApplicationError
		    ("Static methods don't have 'this' pointer");
	} else
	    code.Emit(OpCodes.Ldarg_0);
    }


    /*
     * Generate code for array of trees
     */
    private Item gen(Tree[] trees, MSILType toType) {
	int n = trees.length;
	if (n == 0)
	    return items.VoidItem();
	boolean tmpLastExpr = lastExpr; lastExpr = false;
	for (int i = 0; i < n-1; i++) {
	    drop(gen(trees[i], MSILType.VOID));
	}
	lastExpr = tmpLastExpr;
	return gen(trees[n-1], toType);
    }

    /*
     * Sanity checks for items.
     */
    private Item check(Item item) {
	assert item != null && item.type != null : "" + item;
	return item;
    }

    /*
     *
     */
    private Item genLoad(Tree tree, MSILType toType) {
	return load(gen(tree, toType));
    }

    /** the position of the treenode which is currently being processed. */
    private int pos;

    /*
     * The main generator function. It keeps track of
     * the current position in the tree for better error messages
     */
    private Item gen(Tree tree, MSILType toType) {
	willReturn = true;
 	if (code != null && tree.pos != Position.NOPOS
	    && Position.line(tree.pos) != Position.line(pos))
 	    code.setPosition(Position.line(tree.pos));
	int tmpPos = pos;
	pos = tree.pos;
	Item item = null;
	//i = gen0(tree, toType);
	try {
	    item = gen0(tree, toType);
	    assert item.type.equals(toType) : "" + item + " <> " + toType
		+ "; tree = " + tree.getClass();
	}
	catch (Throwable e) {
// 	    currUnit.error(tree.pos,
// 		 "Exception caught: " + e.getMessage()
// 		 + (global.debug ? "" : "; Use -debug to get a stack trace."));
// 	    //if (global.debug)
// 		e.printStackTrace();
// 	    //System.exit(1);
	    throw Debug.abort(tree, e);
	}
	pos = tmpPos;
	return check(item);
    }

    /*
     * Generate the code corresponding to the given tree node
     */
    private Item gen0(Tree tree, MSILType toType) {
	Symbol sym = tree.hasSymbol() ? tree.symbol() : null;
	Item item = null;
	switch (tree) {
	case Empty:
	    return items.VoidItem();

	case Block(Tree[] stats, Tree value):
            boolean tmpLastExpr = lastExpr; lastExpr = false;
            for (int i = 0; i < stats.length; i++) {
                drop(gen(stats[i], MSILType.VOID));
            }
            lastExpr = tmpLastExpr;
            return gen(value, toType);

	case ValDef(_, Name name, Tree tpe, Tree rhs):
	    LocalBuilder local = code.DeclareLocal(tc.getType(sym));
	    //local.SetLocalSymInfo(name.toString());
	    local.SetLocalSymInfo(tree.symbol().name.toString());
	    locals.put(sym, local);
	    if (rhs == Tree.Empty)
		return items.VoidItem();
	    MSILType type = msilType(tpe.type);
	    genLoad(rhs, type);
	    return check(store(items.LocalItem(local)));

	case Ident(_):
	    if (sym.isModule()) {
		FieldInfo field = tc.getModuleField(sym);
		// force the loading of the module
 		item = coerce(load(items.StaticItem(field)), toType);
	    } else {
		MSILType type = msilType(sym.info());
		Integer slot = (Integer) params.get(sym);
		if (slot != null) {
		    item = items.ArgItem(type, slot.intValue());
		} else {
		    LocalBuilder local = (LocalBuilder) locals.get(sym);
		    if (local != null)
			item = items.LocalItem(local);
		    else {
			assert sym.isStatic() : Debug.show(sym);
			//global.fail("Ident = " + Debug.show(sym));
			FieldInfo field = tc.getField(sym);
			assert field != null : Debug.show(sym);
			if (field.IsLiteral()) {
			    assert field.FieldType.IsEnum()
				&& field.getValue() != null
				: field.toString();
			    return coerce(mkLiteral(msilType(field.FieldType),
						    field.getValue()),
					  toType);
			}
			return coerce(items.StaticItem(field), toType);
		    }
		}
	    }
	    return coerce(item, toType);

	case Select(Tree qualifier, _):
	    if (sym.isModule()) {
		assert !sym.isJava() : "Cannot treat Java class '" +
		    Debug.show(sym) + "' as value.";
		//System.out.println("gen2.Select: object " + Debug.show(qualifier)
		//		   + " . " + Debug.show(sym));
		return coerce(load(items.StaticItem(tc.getModuleField(sym))), toType);
	    }
	    assert !sym.isStatic() :Debug.show(sym);
	    item = items.SelectItem(genLoad(qualifier, msilType(qualifier)),
				    tc.getField(sym));
	    return coerce(item, toType);

	case Apply(Tree fun, Tree[] args):
	    //System.out.println("gen2.Apply: tree.type = " + msilType(tree.type));
	    return coerce(check(genApply(fun, args, msilType(tree.type))), toType);

	case Assign(Tree lhs, Tree rhs):
	    boolean tmpLastExpr = lastExpr; lastExpr = false;
	    MSILType type = msilType(lhs.type);
	    Item var = gen(lhs, type);
	    genLoad(rhs, type);
	    lastExpr = tmpLastExpr;
	    return check(store(var));

	case New(Tree init):
	    switch (init) {
	    case Apply(Tree fun, Tree[] args):
		ConstructorInfo ctor = (ConstructorInfo) tc.getMethod(fun.symbol());
		loadArgs(args, ctor.GetParameters());
		code.Emit(OpCodes.Newobj, ctor);
		return coerce(items.StackItem(msilType(ctor.DeclaringType)), toType);
	    default:
		throw Debug.abort("Incorrect tree", init);
	    }

	case This(_):
	    //return items.SelfItem(tc.getType(currentClass));
	    return coerce(items.SelfItem(tc.getType(sym)), toType);

	case Super(_, _):
// 	    Item retItem = items.SelfItem(tc.getType(currentClass));
// 	    return retItem;
	    return coerce(items.SelfItem(tc.getType(sym)), toType);

	case Literal(AConstant value):
	    if (toType == MSILType.VOID)
		return items.VoidItem();
	    return coerce(items.LiteralItem(value), toType);

	case If(Tree cond, Tree thenp, Tree elsep):
	    item = genIf(cond, thenp, elsep, toType);
	    return check(item);

	case LabelDef(_, _ /*Ident[] params*/, Tree rhs):
	    Label l = code.DefineLabel();
	    code.MarkLabel(l);
	    sym2label.put(sym, l);
	    return gen(rhs, toType);

	case Switch(Tree test, int[] tags, Tree[] bodies, Tree otherwise):
	    LocalBuilder testLoc = code.DeclareLocal(tc.INT);
	    Item loc = items.LocalItem(testLoc);
	    boolean tmpLastExpr = lastExpr; lastExpr = false;
	    genLoad(test, MSILType.I4);
	    store(loc);
	    lastExpr = tmpLastExpr;
	    Label exit = code.DefineLabel();
	    Label nextCase = code.DefineLabel();
	    assert tags.length == bodies.length;
	    for (int i = 0; i < tags.length; i++) {
		load(loc);
		loadI4(tags[i]);
		code.Emit(OpCodes.Bne_Un, nextCase);
		genLoad(bodies[i], toType);
// 		if (lastExpr)
// 		    code.Emit(OpCodes.Ret);
// 		else
		if (willReturn)
		    code.Emit(OpCodes.Br, exit);
		code.MarkLabel(nextCase);
		nextCase = code.DefineLabel();
	    }
	    Item i = genLoad(otherwise, toType);
	    code.MarkLabel(exit);
	    return i;

	default:
	    throw new ApplicationError("Dunno what to do: " + tree.getClass());
	}

    } //gen()

    /* Generate code for conditional expressions.
     */
    private Item genIf(Tree condp, Tree thenp, Tree elsep, MSILType toType) {
	Item iThen = null;
	if (elsep == Tree.Empty && toType == MSILType.VOID) {
	    boolean tmpLastExpr = lastExpr; lastExpr = false;
	    Item condi = gen(condp, msilType(condp.type));
	    Item.CondItem cond = mkCond(condi);
	    lastExpr = tmpLastExpr;
	    if (cond.success == null) {
		Chain success = new Chain(code.DefineLabel(), null);
		branch(cond.test, success);
		iThen = genLoad(thenp, toType);
		resolve(success);
		resolve(cond.failure);
	    } else {
		Chain fail = (cond.failure != null) ?
		    cond.failure : new Chain(code.DefineLabel(), null);
		branch(cond.test, fail);
		resolve(cond.success);
		iThen = genLoad(thenp, toType);
		resolve(fail);
	    }
	} else {
	    boolean tmpLastExpr = lastExpr; lastExpr = false;
	    Item condi = gen(condp, msilType(condp.type));
	    Item.CondItem cond = mkCond(condi);
	    lastExpr = tmpLastExpr;

	    Chain fail = cond.failure != null ?
		cond.failure : new Chain(code.DefineLabel(), null);
	    branch(cond.test, fail);
	    resolve(cond.success);
	    Item thenItem = gen(thenp, toType);
	    iThen = load(coerce(thenItem, toType));
	    Label exit = null;
	    /*if (lastExpr) {
		code.Emit(OpCodes.Ret);
		} else */

	    if (willReturn) {
		exit = code.DefineLabel();
		code.Emit(OpCodes.Br, exit);
	    }
	    resolve(fail);
	    Item iElse = load(gen(elsep, toType));
	    resolve(exit);
	}
	return iThen;
    } //genIf

    /* Coerce the item to the specified type. */
    private Item coerce(Item item, MSILType toType) {
	assert item != null && toType != null : "" + item + " => " + toType;
	//System.out.println("coerce: " + item + " => " + toType);
	if (item.type.equals(toType))
	    return item;
	if (toType == MSILType.VOID)
	    return drop(item);
	switch (item) {
	case VoidItem():
	    assert toType == MSILType.VOID : "" + toType;
	    return item;
	case SelfItem():
	    assert item.type.isSubtypeOf(toType);
	    item.type = toType;
	    return item;
	case StackItem():
	    emitConvert(item.type, toType);
	    item.type = toType;
	    return item;
	case LiteralItem(AConstant value):
	    switch (item.type) {
	    case REF(Type t):
		if (item.type.isValueType()) {
		    assert t.IsEnum() || toType.equals(MSILType.OBJECT)
			: item + " => "+ toType;
		    return coerce(load(item), toType);
		}
		assert (item.type.isValueType())
		    || (item.type.isReferenceType()
			&& item.type.isSubtypeOf(toType));
	    }
	    item.type = toType;
	    return item;
	case ArgItem(_):
	case LocalItem(_):
// 	    System.out.println("coerce: " + item + " => " + toType);
// 	    System.out.println("\t" + item.type + ".isSubtypeOf(" + toType
// 			       + ") = " + item.type.isSubtypeOf(toType));
	    if (item.type.isSubtypeOf(toType)) {
		item.type = toType;
		return item;
	    }
	    break;
	case CondItem(_, _, _):
	    if (item.type == MSILType.BOOL)
		return item;
	    break;
	}
	return coerce(load(item), toType);
    }

    /***/
    private void emitConvert(MSILType fromType, MSILType toType) {
	//System.out.println("emitConvert: " + fromType + " => " + toType);
	switch (fromType) {
	case REF(Type t):
	    if (t.IsValueType() && toType.isType(tc.OBJECT)) {
		code.Emit(OpCodes.Box, t);
		return;
	    } else if (t.IsEnum()) {
		emitConvert(msilType(t.getUnderlyingType()), toType);
		return;
	    }
	}
	if (fromType.isSubtypeOf(toType))
	    return;
	switch (toType) {
	case BOOL: code.Emit(OpCodes.Conv_I4); return; // TODO: is this correct/best?
	case I1: code.Emit(OpCodes.Conv_I1); return;
	case I2: code.Emit(OpCodes.Conv_I2); return;
	case I4: code.Emit(OpCodes.Conv_I4); return;
	case I8: code.Emit(OpCodes.Conv_I8); return;
	case R4: code.Emit(OpCodes.Conv_R4); return;
	case R8: code.Emit(OpCodes.Conv_R8); return;
	case CHAR: code.Emit(OpCodes.Conv_U2); return;
	case REF(Type t):
	    if (t.IsEnum()) {
		assert fromType.isType(t.getUnderlyingType());
		return;
	    }
	    break;
	}
	throw Debug.abort("emitConvert: " + fromType + " => " + toType
			  + "; fromType.isSubtypeOf(toType) = "
			  + fromType.isSubtypeOf(toType));
    }


    /** Generate the code for an Apply node */
    private Item genApply(Tree fun, Tree[] args, MSILType resType) {
	boolean tmpLastExpr = lastExpr; lastExpr = false;
	Symbol sym = fun.symbol();
	switch (fun) {
	case Ident(_):
	    Label l = (Label)sym2label.get(sym);
	    if (l != null) {
		lastExpr = false;
		for (int i = 0; i < args.length; i++)
		    genLoad(args[i], msilType(args[i].type));
		for (int i = args.length; i > 0; i--)
		    code.Emit(OpCodes.Starg_S, i);
		code.Emit(OpCodes.Br, l);
		willReturn = false;
		MSILType retType = msilType(sym.info().resultType());
		Item i = retType == MSILType.VOID ? items.VoidItem()
		    : items.StackItem(retType);
		return coerce(i, resType);
	    }
	    //throw Debug.abort("" + Debug.show(sym));
	    assert sym.isStatic() : Debug.show(sym);
	    lastExpr = tmpLastExpr;
	    //emitThis();
	    return check(invokeMethod(sym, args, resType, true));

	case Select(Tree qualifier, _):
	    // scala.Any.==
	    if (sym == defs.ANY_EQEQ) {
		return genEq(qualifier, args[0]);
	    }
	    // scala.Any.!=
	    if (sym == defs.ANY_BANGEQ) {
		return negate(genEq(qualifier, args[0]));
	    }
	    // java.lang.String.substring(int, int)
	    // needs emulation due to diffenerent semantics
	    // from System.String.Substring
	    if (sym == tc.SYM_SUBSTRING_INT_INT) {
		assert args.length == 2;
		genLoad(qualifier, MSILType.STRING);
		genLoad(args[0], MSILType.I4);
		code.Emit(OpCodes.Dup);
		code.Emit(OpCodes.Neg);
		genLoad(args[1], MSILType.I4);
		code.Emit(OpCodes.Add);
		code.Emit(OpCodes.Call, tc.SUBSTRING_INT_INT);
		return coerce(items.StackItem(MSILType.STRING), resType);
	    }

	    if (sym == tc.SYM_COMPARE_TO_IGNORE_CASE) {
		assert args.length == 1;
		genLoad(qualifier, MSILType.STRING);
		genLoad(args[0], MSILType.STRING);
		code.Emit(OpCodes.Ldc_I4_1);
		code.Emit(OpCodes.Call, tc.COMPARE_TO_IGNORE_CASE);
		return coerce(items.StackItem(MSILType.STRING), resType);
	    }

	    if (isPrimitiveArrayOp(sym)) {
		Primitive prim = primitives.getPrimitive(sym);
		loadArgs(args, tc.getMethod(sym).GetParameters());
		Type elemType = getArrayElemType(prim);
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
		    return items.StackItem(MSILType.ARRAY(msilType(elemType)));
		case ZARRAY_GET: case BARRAY_GET: case SARRAY_GET:
		case CARRAY_GET: case IARRAY_GET: case LARRAY_GET:
		case FARRAY_GET: case DARRAY_GET: case OARRAY_GET:
		    MSILType mType = MSILArrayElemType(elemType);
		    emitLdelem(mType);
		    return items.StackItem(mType);
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
		return primitiveOp(primitives.getPrimitive(sym),
				   qualifier, right, resType);
 	    }

	    Symbol owner = sym.owner();
	    Type t = tc.getType(owner);
	    assert t != null;
	    if (t.IsEnum()) {
		Primitive enumOp = null;
		if (sym.name == Names.EQ) enumOp = Primitive.EQ;
		else if (sym.name == Names.NE) enumOp = Primitive.NE;
		else if (sym.name == Names.LT) enumOp = Primitive.LT;
		else if (sym.name == Names.LE) enumOp = Primitive.LE;
		else if (sym.name == Names.GT) enumOp = Primitive.GT;
		else if (sym.name == Names.GE) enumOp = Primitive.GE;
		else if (sym.name == Names.OR) enumOp = Primitive.OR;
		else if (sym.name == Names.AND) enumOp = Primitive.AND;
		else if (sym.name == Names.XOR) enumOp = Primitive.XOR;
		if (enumOp != null) {
		    assert args.length == 1;
		    return primitiveOp(enumOp, qualifier, args[0], resType);
		}
	    }

	    MSILType convTo = primitiveConvert(sym);
	    if (convTo != null) {
		assert args.length == 1;
		genLoad(args[0], convTo);
		return items.StackItem(convTo);
	    }

	    switch (qualifier.type) {
	    case TypeRef(_, _, _):
	    case SingleType(_, _):
	    case ThisType(_):
		MethodBase method = tc.getMethod(sym);
		boolean virtualCall = false;
		if (!method.IsStatic() || becomesStatic(sym)) {
		    // FIXME: after the Super attribution is correct
		    switch (qualifier) {
		    case Super(_, _):
			load(items.SelfItem(tc.getType(currentClass)));
			break;
		    default:
			genLoad(qualifier, msilType(qualifier));
			virtualCall = true;
		    }
		}
		lastExpr = tmpLastExpr;
		return check(invokeMethod(sym, args, resType, virtualCall));

	    default:
		throw Debug.abort(Debug.show(fun));
	    } // switch(qualifier.type)

 	case TypeApply(Tree tfun, Tree[] targs):
	    final Symbol tsym = tfun.symbol();
	    if (primitives.isPrimitive(tsym)) {
		return primitiveOp(primitives.getPrimitive(tsym),
				   ((Select)tfun).qualifier,
				   targs[0], resType);
	    }
	    throw Debug.abort(Debug.show(fun));

	default:
	    throw Debug.abort(Debug.show(fun));
	} // switch (fun)
    } //genApply()


    /** Generate code for scala's '==' */
    private Item genEq(Tree left, Tree right) {
	LocalBuilder tmpLocal = (LocalBuilder)locals.get(defs.ANY_EQEQ);
	if (tmpLocal == null) {
	    tmpLocal = code.DeclareLocal(tc.OBJECT);
	    locals.put(defs.ANY_EQEQ, tmpLocal);
	}
	Label l1 = code.DefineLabel(), l2 = code.DefineLabel();
	genLoad(left, MSILType.OBJECT);
	genLoad(right, MSILType.OBJECT);
	code.Emit(OpCodes.Stloc, tmpLocal);
	code.Emit(OpCodes.Dup);
	code.Emit(OpCodes.Ldnull);
	code.Emit(OpCodes.Bne_Un, l1);
	code.Emit(OpCodes.Ldloc, tmpLocal);
	code.Emit(OpCodes.Ceq);
	code.Emit(OpCodes.Br, l2);
	code.MarkLabel(l1);
	code.Emit(OpCodes.Ldloc, tmpLocal);
	code.Emit(OpCodes.Callvirt, tc.OBJECT_EQUALS);
	code.MarkLabel(l2);
	return items.StackItem(MSILType.BOOL);
    }

    /** Is it a primitive (bot not an array) operation*/
    private boolean isPrimitiveOp(Symbol sym) {
	switch (primitives.getPrimitive(sym)) {
	case POS: case NEG: case NOT:
	case ADD: case SUB: case MUL: case DIV: case MOD:
	case OR: case XOR: case AND:
	case LSL: case LSR: case ASR:
	case ID: case EQ: case NE: case LT: case LE: case GT: case GE:
	case ZNOT: case ZOR: case ZAND:
	case IS: case AS:
	case CONCAT:
	case THROW:
	case SYNCHRONIZED:
	    return true;
	default:
	    return false;
	}
    }

    /* Is this a primitive Array operation. */
    private boolean isPrimitiveArrayOp(Symbol sym) {
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

    /* Is this a primitive conversion operation. */
    private MSILType primitiveConvert(Symbol sym) {
	switch (primitives.getPrimitive(sym)) {
	case B2B: case S2B: case C2B: case I2B: case L2B: case F2B: case D2B:
	    return MSILType.I1;
	case B2S: case S2S: case C2S: case I2S: case L2S: case F2S: case D2S:
	    return MSILType.I2;
	case B2C: case S2C: case C2C: case I2C: case L2C: case F2C: case D2C:
	    return MSILType.CHAR;
	case B2I: case S2I: case C2I: case I2I: case L2I: case F2I: case D2I:
	    return MSILType.I4;
	case B2L: case S2L: case C2L: case I2L: case L2L: case F2L: case D2L:
	    return MSILType.I8;
	case B2F: case S2F: case C2F: case I2F: case L2F: case F2F: case D2F:
	    return MSILType.R4;
	case B2D: case S2D: case C2D: case I2D: case L2D: case F2D: case D2D:
	    return MSILType.R8;
	default:
	    return null;
	}
    }

    /* Generate code for primitive operations. */
    private Item primitiveOp(Primitive op, Tree left, Tree right, MSILType resType)
    {
	// treat some special cases
	switch (op) {

	case CONCAT:
	    genLoad(left, MSILType.OBJECT);
	    genLoad(right, MSILType.OBJECT);
	    code.Emit(OpCodes.Call, tc.CONCAT_OBJECT_OBJECT);
	    return items.StackItem(MSILType.STRING);

	case ADD:
	    if (tc.getType(right.type) == tc.STRING) {
		System.out.println("primaryOp().ADD: string concat!");
		genLoad(left, MSILType.OBJECT);
		genLoad(right, MSILType.OBJECT);
		code.Emit(OpCodes.Call, tc.CONCAT_OBJECT_OBJECT);
		return items.StackItem(MSILType.STRING);
	    }
	    break;

	case IS:
	    genLoad(left, MSILType.OBJECT);
	    final Type type = tc.getType(right.type);
	    code.Emit(OpCodes.Isinst, type);
	    return mkCond(items.StackItem(msilType(type)));

	case AS:
	    Item item = genLoad(left, MSILType.OBJECT);
	    final Type type = tc.getType(right.type);
	    final MSILType mtype = msilType(type);
	    if (!item.type.equals(mtype) && !type.equals(MSILType.OBJECT)) {
		if (type.IsValueType()) {
		    code.Emit(OpCodes.Unbox, type);
		    if (type.IsEnum())
			emitLdind(type.getUnderlyingType());
		    else
			code.Emit(OpCodes.Ldobj, type);
		} else {
		    code.Emit(OpCodes.Castclass, type);
		}
	    }
	    return items.StackItem(mtype);

	case SYNCHRONIZED:
	    // TODO: reuse temporary local variable whenever possible
	    LocalBuilder tmp = code.DeclareLocal(tc.OBJECT);
	    genLoad(left, MSILType.OBJECT);
	    code.Emit(OpCodes.Stloc, tmp);
	    code.Emit(OpCodes.Ldloc, tmp);
	    code.Emit(OpCodes.Call, tc.MONITOR_ENTER);
	    genLoad(right, MSILType.OBJECT);
	    code.Emit(OpCodes.Ldloc, tmp);
	    code.Emit(OpCodes.Call, tc.MONITOR_EXIT);
	    return items.StackItem(MSILType.OBJECT);
	}

	Item iLeft = null;
	MSILType tleft = msilType(left);
	switch (left) {
	case Apply(Tree fun , Tree[] args):
	    Primitive p = primitives.getPrimitive(fun.symbol());
	    switch (p) {
	    case BOX:
		assert args.length == 1;
		iLeft = gen(args[0], msilType(args[0]));
		break;
	    }
	    break;
	default:
	    iLeft = gen(left, tleft);
	}

	switch (op) {
	case THROW:
	    load(iLeft);
	    code.Emit(OpCodes.Throw);
// 	    code.Emit(OpCodes.Ldnull);
// 	    return items.StackItem(MSILType.NULL);
	    //return coerce(loadNull(), resType);
	    return loadNull();

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

	MSILType toType = MSILType.arithmCoercion(iLeft.type.asPrimitive(),
						  primitiveType(right.type));
	load(coerce(iLeft, toType));
	genLoad(right, toType);
	Item res = null;
	switch (op) {
	case ADD: code.Emit(OpCodes.Add); res = items.StackItem(toType); break;
	case SUB: code.Emit(OpCodes.Sub); res = items.StackItem(toType); break;
	case MUL: code.Emit(OpCodes.Mul); res = items.StackItem(toType); break;
	case DIV: code.Emit(OpCodes.Div); res = items.StackItem(toType); break;
	case MOD: code.Emit(OpCodes.Rem); res = items.StackItem(toType); break;

	case OR:  code.Emit(OpCodes.Or);  res = items.StackItem(toType); break;
	case XOR: code.Emit(OpCodes.Xor); res = items.StackItem(toType); break;
	case AND: code.Emit(OpCodes.And); res = items.StackItem(toType); break;

	case LSL: code.Emit(OpCodes.Shl); res = items.StackItem(toType); break;
	case LSR: code.Emit(OpCodes.Shr); res = items.StackItem(toType); break;
	case ASR: code.Emit(OpCodes.Shr_Un); res = items.StackItem(toType); break;

	case ID: case EQ: // FIXME?: should ID be treated as EQ?
	    res =  items.CondItem(Test.Binary(Test.EQ, toType), null, null);
	    break;
	case NE:
	    res = items.CondItem(Test.Binary(Test.NE, toType), null, null);
	    break;
	case LT:
	    res = items.CondItem(Test.Binary(Test.LT_IS, toType), null, null);
	    break;
	case LE:
	    res = items.CondItem(Test.Binary(Test.LE_IS, toType), null, null);
	    break;
	case GT:
	    res = items.CondItem(Test.Binary(Test.GT_IS, toType), null, null);
	    break;
	case GE:
	    res = items.CondItem(Test.Binary(Test.GE_IS, toType), null, null);
	    break;
	default:
	    throw new ApplicationError(Debug.show(op));
	}

	return coerce(res, resType);
    }

    /*
     * Load function arguments on the stack.
     */
    private void loadArgs(Tree[] args, ParameterInfo[] params) {
	boolean tmpLastExpr = lastExpr; lastExpr = false;
	for (int i = 0; i < args.length; i++) {
	    MSILType toType = msilType(params[i].ParameterType); //msilType(args[i].type);
	    genLoad(args[i], toType);
	}
	lastExpr = tmpLastExpr;
    }

    /*
     * Tells if a non-static method is translated to a static one.
     */
    private boolean becomesStatic(Symbol sym) {
	MethodBase method = tc.getMethod(sym);
	return !sym.isStatic() && method.IsStatic();
    }

    /*
     * Generate code for method invocation
     */
    private Item invokeMethod(Symbol fun, Tree[] args, MSILType resType,
		      boolean virtualCall) {
	MethodBase method = tc.getMethod(fun);
	assert method != null : "Coudn't resolve method: " + Debug.show(fun);
	Item res = null;
	if (method.IsStatic()) {
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
	} else if (method.IsConstructor()) {
	    // used only for calls to super constructor
	    //emitThis();
	    loadArgs(args, method.GetParameters());
	    code.Emit(OpCodes.Call, (ConstructorInfo)method);
	    res = items.VoidItem();
	} else {
	    loadArgs(args, method.GetParameters());
	    if (enableTailCalls && lastExpr && method == currentMethod)
		code.Emit(OpCodes.Tailcall);
	    code.Emit(virtualCall ? OpCodes.Callvirt : OpCodes.Call,
		      (MethodInfo)method);
	    res = returnsVoid(method) ? items.VoidItem() : items.StackItem(resType);
	}
	return check(res);
    }

    /*
     * Returns the MSILType that corresponds to the given scala.symtab.Type
     */
    private MSILType msilType(scalac.symtab.Type type) {
	return msilType(tc.getType(type));
    }

    /*
     * Also used from class ItemFactory.
     */
    MSILType msilType(Type type) {
	return MSILType.fromType(type);
    }

    /*
     * Returns the MSILType from the type atribute of a tree node
     */
    private MSILType msilType(Tree t) {
	return msilType(t.type);
    }

    /*
     * Treat the scala boxed types as a primitive type
     */
    private MSILType primitiveType(scalac.symtab.Type type) {
	MSILType mtype = msilType(type);
	switch (mtype) {
	case REF(Type t):
	    if (t == tc.SCALA_BYTE)    return MSILType.I1;
	    if (t == tc.SCALA_SHORT)   return MSILType.I2;
	    if (t == tc.SCALA_INT)     return MSILType.I4;
	    if (t == tc.SCALA_LONG)    return MSILType.I8;
	    if (t == tc.SCALA_FLOAT)   return MSILType.R4;
	    if (t == tc.SCALA_DOUBLE)  return MSILType.R8;
	    if (t == tc.SCALA_CHAR)    return MSILType.CHAR;
	    if (t == tc.SCALA_UNIT)    return MSILType.VOID;
	    if (t == tc.SCALA_BOOLEAN) return MSILType.BOOL;
	    return msilType(t).asPrimitive();
	    //case NULL:
	case ARRAY(_):
	    throw Debug.abort("primitiveType: cannot convert " + mtype);
	default:
	    return mtype;
	}
    }

    /*
     * Provides a translation for some types when used as array elements.
     */
    private MSILType MSILArrayElemType(Type type) {
	MSILType mtype = msilType(type);
	switch (mtype) {
	case BOOL: return MSILType.I1;
	case CHAR: return MSILType.I4;
	default: return mtype;
	}
    }

    /*
     *
     */
    private Type getArrayElemType(Primitive p) {
	switch (p) {
	case NEW_ZARRAY: case ZARRAY_LENGTH: case ZARRAY_GET: case ZARRAY_SET:
	    return tc.BOOLEAN;
	case NEW_BARRAY: case BARRAY_LENGTH: case BARRAY_GET: case BARRAY_SET:
	    return tc.BYTE;
	case NEW_SARRAY: case SARRAY_LENGTH: case SARRAY_GET: case SARRAY_SET:
	    return tc.SHORT;
	case NEW_CARRAY: case CARRAY_LENGTH: case CARRAY_GET: case CARRAY_SET:
	    return tc.CHAR;
	case NEW_IARRAY: case IARRAY_LENGTH: case IARRAY_GET: case IARRAY_SET:
	    return tc.INT;
	case NEW_LARRAY: case LARRAY_LENGTH: case LARRAY_GET: case LARRAY_SET:
	    return tc.LONG;
	case NEW_FARRAY: case FARRAY_LENGTH: case FARRAY_GET: case FARRAY_SET:
	    return tc.FLOAT;
	case NEW_DARRAY: case DARRAY_LENGTH: case DARRAY_GET: case DARRAY_SET:
	    return tc.DOUBLE;
	case NEW_OARRAY: case OARRAY_LENGTH: case OARRAY_GET: case OARRAY_SET:
	    return tc.OBJECT;
	}
	throw Debug.abort("unknown primitive " + Debug.show(p));
    }

    /*
     * Emit code to load an array elemnt of the given type on the stack.
     */
    private Item emitLdelem(MSILType type) {
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

    /*
     * Emit code to strore the value at the top of the stack from the given type
     * to an array.
     */
    private Item emitStelem(MSILType type) {
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

    private Item.LiteralItem mkLiteral(MSILType type, Object value) {
	LiteralItem item = null;
	if (value instanceof Integer)
	    item = items.LiteralItem(AConstant.INT(((Number)value).intValue()));
	else if (value instanceof Long)
	    item = items.LiteralItem(AConstant.LONG(((Number)value).longValue()));
	else if (value instanceof Float)
	    item = items.LiteralItem(AConstant.FLOAT(((Number)value).floatValue()));
	else if (value instanceof Double)
	    item = items.LiteralItem(AConstant.DOUBLE(((Number)value).doubleValue()));
	else throw Debug.abort();
	item.type = type;
	return item;
    }

    private void emitLdarg(int slot) {
	assert slot >= 0;
	switch (slot) {
	case 0: code.Emit(OpCodes.Ldarg_0); break;
	case 1: code.Emit(OpCodes.Ldarg_1); break;
	case 2: code.Emit(OpCodes.Ldarg_2); break;
	case 3: code.Emit(OpCodes.Ldarg_3); break;
	default:
	    code.Emit(slot < 256 ? OpCodes.Ldarg_S : OpCodes.Ldarg, slot);
	}
    }

    /*
     * Load the value of an item on the stack.
     */
    private Item load(Item that) {
	switch (that) {
 	case VoidItem():
 	    return that;

	case StackItem():
	    return (StackItem) that;

	case LiteralItem(AConstant value):
	    return loadLiteral(value, that.type);

	case SelfItem():
	    emitThis();
	    return items.StackItem(that.type);

	case ArgItem(int slot):
	    emitLdarg(slot);
	    return items.StackItem(that.type);

	case LocalItem(LocalBuilder local):
	    code.Emit(OpCodes.Ldloc, local);
	    return items.StackItem(that.type);

	case StaticItem(FieldInfo field):
	    assert !field.IsLiteral() : field.toString();
	    code.Emit(OpCodes.Ldsfld, field);
	    return items.StackItem(that.type);

	case SelectItem(Item qual, FieldInfo field):
	    Item i = load(qual);
	    code.Emit(OpCodes.Ldfld, field);
	    return items.StackItem(that.type);

	case CondItem(Test test, Chain success, Chain failure):
	    load(test);
	    switch (test) {
	    case Or(_):
	    case And(_):
		Label exit = null;
		/*if (lastExpr)
		    code.Emit(OpCodes.Ret);
		    else */
		if (willReturn) {
		    exit = code.DefineLabel();
		    code.Emit(OpCodes.Br, exit);
		}
		if (failure != null) {
		    resolve(failure);
		    load(FALSE_ITEM);
		    if (success != null) {
			/*if (lastExpr)
			    code.Emit(OpCodes.Ret);
			    else*/
			if (willReturn)
			    code.Emit(OpCodes.Br, exit);
		    }
		}
		if (success != null) {
		    resolve(success);
		    load(TRUE_ITEM);
		}
		resolve(exit);
		break;
	    }
	    return items.StackItem(MSILType.BOOL);

	default:
	    throw new ApplicationError("load item: " + that);
	}
    }


    /*
     * Load the value of a test on the stack.
     */
    private void load(Test test) {
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

    /*
     * Load the value of an ALiteral on the stack.
     */
    private Item.StackItem loadLiteral(AConstant constant) {
        switch (constant) {
        case UNIT:
            return loadUnit();
        case BOOLEAN(boolean value):
            return loadBool(value);
        case BYTE(byte value):
            return loadI4(value);
        case SHORT(short value):
            return loadI4(value);
        case CHAR(char value):
            return loadI4(value);
        case INT(int value):
            return loadI4(value);
        case LONG(long value):
            return loadI8(value);
        case FLOAT(float value):
            return loadR4(value);
        case DOUBLE(double value):
            return loadR8(value);
        case STRING(String value):
            return loadString(value);
        case NULL:
            return loadNull();
        default:
            throw Debug.abort("illegal case", constant);
        }
    }

    /*
     * Load the value of an ALiteral coerced to the given type.
     */
    private Item loadLiteral(AConstant constant, MSILType ofType) {
	switch (ofType) {
	case I1:
	case I2:
	case I4:
	case CHAR: loadI4(constant.intValue()); return items.StackItem(MSILType.I4);
	case I8: loadI8(constant.longValue()); return items.StackItem(ofType);
	case R4: loadR4(constant.floatValue()); return items.StackItem(ofType);
	case R8: loadR8(constant.doubleValue()); return items.StackItem(ofType);
	case REF(Type t):
	    //System.out.println(constant + " => " + ofType);
	    //assert t == tc.STRING || ofType.isValueType() : "" + ofType;
	    return coerce(loadLiteral(constant), ofType);
	default:
	    return coerce(loadLiteral(constant), ofType);
	}
    }

    /*
     * Load boxed scala.Unit on the stack.
     */
    private Item.StackItem loadUnit() {
        code.Emit(OpCodes.Call, tc.RUNTIME_BOX_UNIT);
	return items.StackItem(msilType(tc.RUNTIME_BOX_UNIT.ReturnType));
    }

    /*
     * Load a boolean value on the stack.
     */
    private Item.StackItem loadBool(boolean value) {
        code.Emit(value ? OpCodes.Ldc_I4_1 : OpCodes.Ldc_I4_0);
        return items.StackItem(MSILType.BOOL);
    }

    /*
     * Load a 32-bit integer literal on the stack.
     */
    private Item.StackItem loadI4(int value) {
        switch (value) {
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
            if (value >= -128 && value <= 127)
                code.Emit(OpCodes.Ldc_I4_S, value);
            else
                code.Emit(OpCodes.Ldc_I4, value);
        }
        return items.StackItem(MSILType.I4);
    }

    /*
     * Load a 64-bit integer value on the stack.
     */
    private Item.StackItem loadI8(long value) {
        code.Emit(OpCodes.Ldc_I8, value);
        return items.StackItem(MSILType.I8);
    }

    /*
     * Load a single precision floating point value on the stack.
     */
    private Item.StackItem loadR4(float value) {
        code.Emit(OpCodes.Ldc_R4, value);
        return items.StackItem(MSILType.R4);
    }

    /*
     * Load a double precision floating point value on the stack.
     */
    private Item.StackItem loadR8(double value) {
        code.Emit(OpCodes.Ldc_R8, value);
        return items.StackItem(MSILType.R8);
    }

    /*
     * Load a string reference on the stack.
     */
    private Item.StackItem loadString(String value) {
        code.Emit(OpCodes.Ldstr, value);
        return items.StackItem(MSILType.STRING);
    }

    /*
     * Load the 'null' reference on the stack.
     */
    private Item.StackItem loadNull() {
        code.Emit(OpCodes.Ldnull);
        return items.StackItem(MSILType.NULL);
    }

    private void emitLdind(Type type) {
	if (type == tc.BYTE)
	    code.Emit(OpCodes.Ldind_I1);
	else if (type == tc.SHORT)
	    code.Emit(OpCodes.Ldind_I2);
	else if (type == tc.INT)
	    code.Emit(OpCodes.Ldind_I4);
	else if (type == tc.LONG)
	    code.Emit(OpCodes.Ldind_I8);
	else
	    throw Debug.abort("emitLdind failed", type);
    }


    /*
     * Store the value on the stack to the location specified by the item.
     */
    private Item store(Item that) {
	switch (that) {
	case ArgItem(int slot):
	    code.Emit(OpCodes.Starg, slot);
	    break;

	case LocalItem(LocalBuilder local):
	    code.Emit(OpCodes.Stloc, local);
	    break;

	case StaticItem(FieldInfo field):
	    assert !field.IsLiteral();
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


    /*
     * Discard the item.
     */
    private Item drop(Item that) {
	switch (that) {
	case VoidItem():
	case SelfItem():
	case LiteralItem(_):
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
	return items.VoidItem();
    }

    /*
     * Generate a condition corresponding to the given item.
     */
    private Item.CondItem mkCond(Item that) {
	switch (that) {
	case CondItem(_, _, _): return (Item.CondItem) that;
	default:
	    load(that);
	    return items.CondItem(Test.Bool(false), null, null);
	}
    }

    /*
     * Negate a condition.
     */
    private Item.CondItem negate(Item item) {
	Item.CondItem cond = mkCond(item);
	// swap the failure and success chains
	return items.CondItem(negate(cond.test), cond.success, cond.failure);
    }

    /*
     * Emit code to negate a boolean value on the stack.
     */
    private void negate_load() {
	code.Emit(OpCodes.Ldc_I4_1);
	code.Emit(OpCodes.Xor);
    }

    /*
     * Negate a test.
     */
    private Test negate(Test that) {
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

    /*
     * Emit code for the appropriate branch.
     */
    private Label branch(Test test, Chain chain) {
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

    /*
     * Merge two label chains.
     */
    private static Chain merge(Chain c1, Chain c2) {
	if (c1 == null) return c2;
	if (c2 == null) return c1;
	Chain c = c1;
	for(; c.next != null; c = c.next) ;
	c.next = c2;
	return c1;
    }

    /*
     * Resolve all labels int the chain to the current position in the code.
     */
    private void resolve(Chain c) {
	for (; c != null; c = c.next)
	    code.MarkLabel(c.label);
    }

    /*
     * Resolve a label to the current position in the code.
     */
    private void resolve(Label l) {
	if (l != null)
	    code.MarkLabel(l);
    }

    /*
     *
     */
    private static boolean negate_load(int opcode) {
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

    /*
     *
     */
    private static OpCode load(int opcode) {
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

    /*
     *
     */
    private static int negate(int opcode) {
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

    /*
     *
     */
    private static OpCode branch(int opcode) {
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

    //##########################################################################
} // class GenMSIL

/**
 * Represents the supported types of the CLR.
 */
final class MSILType {
    public case I1;
    public case I2;
    public case U2;
    public case I4;
    public case I8;
    public case R4;
    public case R8;
    public case BOOL;
    public case CHAR;
    public case VOID;
    public case NULL;
    public case REF(Type t) { assert t != null && !t.IsArray(); }
    public case ARRAY(MSILType t) { assert t != null; }

    private final static CLRPackageParser pp = CLRPackageParser.instance;

    public static final MSILType OBJECT = REF(pp.OBJECT);
    public static final MSILType STRING = REF(pp.STRING);

    //##########################################################################
    // factory methods

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
	case TypeTags.STRING: return STRING;
	default:
	    throw new ApplicationError("Unknown kind: " + kind);

	}
    }

    public static MSILType fromType(Type type) {
	if (type == pp.BYTE)   return I1;
	if (type == pp.SHORT)  return I2;
	if (type == pp.INT)    return I4;
	if (type == pp.LONG)   return I8;
	if (type == pp.FLOAT)  return R4;
	if (type == pp.DOUBLE) return R8;
	if (type == pp.CHAR)   return CHAR;
	if (type == pp.VOID)   return VOID;
	if (type == pp.BOOLEAN)return BOOL;
	if (type == pp.STRING) return STRING;
	if (type == pp.OBJECT) return OBJECT;
	if (type.IsArray())
	    return ARRAY(fromType(type.GetElementType()));
	return REF(type);

    }

    public static MSILType fromAConstant(AConstant value) {
        switch (value) {
        case UNIT:
            return VOID; // FIXME
        case BOOLEAN(_):
            return BOOL;
        case BYTE(_):
            return I1;
        case SHORT(_):
            return I2;
        case CHAR(_):
            return CHAR;
        case INT(_):
            return I4;
        case LONG(_):
            return I8;
        case FLOAT(_):
            return R4;
        case DOUBLE(_):
            return R8;
        case STRING(_):
            return STRING;
        case NULL:
            return NULL;
        case ZERO:
            return I4;
        default:
            throw Debug.abort("unknown case", value);
        }
    }

    //##########################################################################

    public boolean equals(Object that) {
	if (this == that)
	    return true;
	if (that == null || !(that instanceof MSILType))
	    return false;
	MSILType thatType = (MSILType)that;
	switch (this) {
	case REF(Type t1):
	    switch (thatType) {
	    case REF(Type t2):
		return t1.equals(t2);
	    default:
		return false;
	    }
	case ARRAY(MSILType t1):
	    switch (thatType) {
	    case ARRAY(MSILType t2):
		return t1.equals(t2);
	    default:
		return false;
	    }
	default:
	    return false;
	}
    }

    public boolean isReferenceType() {
	switch (this) {
	case REF(Type t):
	    return !t.IsValueType();
	case NULL:
	case ARRAY(_):
	    return true;
	default:
	    return false;
	}
    }

    public boolean isValueType() {
	return !isReferenceType();
    }

    public boolean isType(Type type) {
	return equals(fromType(type));
    }

    public MSILType asPrimitive() {
	switch (this) {
	case REF(Type t):
	    if (t.IsEnum()) return fromType(t.getUnderlyingType());
	    else return this;
	case NULL:
	case ARRAY(_):
	    throw Debug.abort(this);
	default: return this;
	}
    }

    /** Is this type is a subtype of that. */
    public boolean isSubtypeOf(MSILType that) {
	assert that != null;
	if (this == that
	    || (this.isReferenceType() && that.isType(pp.OBJECT)))
	    return true;
	if (this.isValueType() && that.isReferenceType())
	    return false;
	switch (this) {
	case BOOL: return that == I4;
	case REF(Type t1):
	    switch (that) {
	    case REF(Type t2):
		return t1.isSubtypeOf(t2);
	    default:
		return false;
	    }
	case ARRAY(MSILType t1):
	    switch (that) {
	    case ARRAY(MSILType t2):
		return t1.isSubtypeOf(t2);
	    default:
		return false;
	    }
	case NULL:
	    switch (that) {
	    case REF(_):
	    case ARRAY(_):
		return true;
	    default:
		return false;
	    }
	default:
	    return false;
	}
    }


    private static final MSILType [] ARITHM_PRECISION =
	new MSILType[] {I1, I2, CHAR, I4, I8, R4, R8};

    /**
     * @param t1
     * @param t2
     * @return the arithmetic coercion type for types t1 and t2
     */
    public static MSILType arithmCoercion(MSILType t1, MSILType t2) {
	if (t1 == t2) return t1;
	int i, j, n = ARITHM_PRECISION.length;
	for (i = 0; i < n; i++)
	    if (t1 == ARITHM_PRECISION[i])
		break;
	for (j = 0; j < n; j++)
	    if (t2 == ARITHM_PRECISION[j])
		break;
	int m = Math.max(i, j);
	if (m < n)
	    return ARITHM_PRECISION[m];
	else if (t1.isSubtypeOf(t2)) return t2;
	else if (t2.isSubtypeOf(t1)) return t1;
	else throw Debug.abort("cannot find coercion " + t1 + " => " + t2);
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
	case NULL: return "NULL";
        default: return getClass().getName();
        }
    }

    //##########################################################################
}


/**
 * The items used for deferred code generation.
 */
class Item {
    public MSILType type;

    public case VoidItem();
    public case StackItem();
    public case SelfItem();
    public case LiteralItem(AConstant value);
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
	case LiteralItem(AConstant value):
	    return "LiteralItem(" + value + "): " + type;
	case ArgItem( int slot):
	    return "ArgItem(" + slot + "): " + type;
	case LocalItem( LocalBuilder local):
	    return "LocalItem(" + local + "): " + type;
	case StaticItem( FieldInfo field):
	    return "StaticItem(" + field + "): " + type;
	case SelectItem(_, FieldInfo field):
	    return "SelectItem(" + field + "): " +type;
	case CondItem(Test test, Chain success, Chain failure):
	    return "CondItem(" + test + ", " + success
		+ ", " + failure + "): " + type;
	}
	return "??Item??";
    }
}


/**
 * Class implementing a chain (list) of labels.
 */
class Chain {
    Label label;
    Chain next;
    Chain(Label l, Chain n) { label = l; next = n; }
}


/**
 * Factory class for items.
 */
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
	assert item.type != null;
	return item;
    }
    public Item.SelfItem SelfItem(Type t) {
	Item.SelfItem item = Item.SelfItem();
	item.type = coder.msilType(t);
	assert item.type != null;
	return item;
    }
    public Item.LiteralItem LiteralItem(AConstant value) {
	Item.LiteralItem item = Item.LiteralItem(value);
	item.type = MSILType.fromAConstant(value);
	assert item.type != null;
	return item;
    }
    public Item.ArgItem ArgItem(MSILType type, int slot) {
	Item.ArgItem item = Item.ArgItem(slot);
	item.type = type;
	assert item.type != null;
	return item;
    }
    public Item.LocalItem LocalItem(LocalBuilder local) {
	Item.LocalItem item = Item.LocalItem(local);
	item.type = coder.msilType(local.LocalType);
	assert item.type != null;
	return item;
    }
    public Item.StaticItem StaticItem(FieldInfo field) {
	assert field.IsStatic();
	Item.StaticItem item = Item.StaticItem(field);
	item.type = coder.msilType(field.FieldType);
	assert item.type != null;
	return item;
    }
    public Item.SelectItem SelectItem(Item qualifier, FieldInfo field) {
	assert !field.IsStatic();
	Item.SelectItem item =  Item.SelectItem(qualifier, field);
	item.type = coder.msilType(field.FieldType);
	assert item.type != null;
	return item;
    }
    public Item.CondItem CondItem(Test test, Chain success, Chain failure) {
	Item.CondItem item = Item.CondItem(test, success, failure);
	item.type = MSILType.BOOL;
	return item;
    }
}

/**
 * Class representing the possible tests in conditional item
 */
class Test {

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
