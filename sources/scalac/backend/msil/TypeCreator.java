/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.backend.msil;

import scalac.Global;
import scalac.Phase;
import scalac.CompilationUnit;
import scalac.ApplicationError;
import scalac.ast.Tree;
import scalac.ast.Traverser;
import scalac.util.Debug;
import scalac.util.Name;
import scalac.util.Names;
import scalac.util.SourceRepresentation;
import scalac.symtab.Kinds;
import scalac.symtab.TypeTags;
import scalac.symtab.Symbol;
import scalac.symtab.Scope;
import scalac.symtab.Modifiers;
import scalac.symtab.Definitions;
import scalac.symtab.classfile.CLRTypes;
import scala.tools.util.Position;
import scala.tools.util.SourceFile;

import Tree.*;

import ch.epfl.lamp.compiler.msil.*;
import ch.epfl.lamp.compiler.msil.emit.*;

import java.io.File;
import java.io.IOException;
import java.util.Map;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Set;
import java.util.HashSet;
import java.util.LinkedHashSet;

/**
 * Creates System.Reflection objects corresponding to
 * Scala symbols
 *
 * @author Nikolay Mihaylov
 * @version 1.1
 */

final class TypeCreator {

    //private final GenMSIL gen;
    private final Global global;
    private final Definitions defs;

    private final ArrayList typeBuilders = new ArrayList();

    // for every method of an object give the corresponding static method
    // of the accompanying class
    private final Map/*<Symbol, MethodBuilder>*/ syms2staticMethods = new HashMap();

    private final Map types2symbols;
    private final Map symbols2types;
    private final Map symbols2fields;
    private final Map symbols2methods;
    private final Map symbols2moduleFields;

    public static final String MODULE_S = "$MODULE";

    public final Type BYTE;
    public final Type CHAR;
    public final Type SHORT;
    public final Type INT;
    public final Type LONG;
    public final Type FLOAT;
    public final Type DOUBLE;
    public final Type BOOLEAN;
    public final Type VOID;
    public final Type ENUM;

    public final Type OBJECT;
    public final Type STRING;
    public final Type STRING_ARRAY;

    private final Type MONITOR;

    public final MethodInfo CONCAT_OBJECT_OBJECT;
    public final MethodInfo OBJECT_EQUALS;
    public final MethodInfo MONITOR_ENTER;
    public final MethodInfo MONITOR_EXIT;

//     private final MethodInfo MONITOR_PULSE;
//     private final MethodInfo MONITOR_PULSE_ALL;
//     private final MethodInfo MONITOR_WAIT;
//     private final MethodInfo MONITOR_WAIT_TIMEOUT;

    public final Type SCALA_BYTE;
    public final Type SCALA_SHORT;
    public final Type SCALA_INT;
    public final Type SCALA_LONG;
    public final Type SCALA_FLOAT;
    public final Type SCALA_DOUBLE;
    public final Type SCALA_CHAR;
    public final Type SCALA_BOOLEAN;
    public final Type SCALA_UNIT;

    public final MethodInfo RUNTIME_BOX_UNIT ;

    public Symbol SYM_SUBSTRING_INT_INT;
    public MethodInfo SUBSTRING_INT_INT;
    public Symbol SYM_COMPARE_TO_IGNORE_CASE;
    public MethodInfo COMPARE_TO_IGNORE_CASE;

    public ConstructorInfo SCALA_SYMTAB_ATTR_CONSTR;

    private scalac.symtab.Type MAIN_METHOD_TYPE;

    private final CLRTypes ti;

    private final Phase backPhase;

    //##########################################################################

    TypeCreator(Global global, GenMSILPhase phase) {
	this.global = global;
	this.defs = global.definitions;
        this.backPhase = global.PHASE.ADDINTERFACES.phase();

	ti = CLRTypes.instance();

	types2symbols = phase.types2symbols;
	symbols2types = phase.symbols2types;
	symbols2fields = phase.symbols2fields;
	symbols2methods = phase.symbols2methods;
	symbols2moduleFields = phase.symbols2moduleFields;

	BYTE    = ti.BYTE;
	CHAR    = ti.CHAR;
	SHORT   = ti.SHORT;
	INT     = ti.INT;
	LONG    = ti.LONG;
	FLOAT   = ti.FLOAT;
	DOUBLE  = ti.DOUBLE;
	BOOLEAN = ti.BOOLEAN;
	VOID    = ti.VOID;
	ENUM    = ti.ENUM;

	OBJECT = ti.OBJECT;
	STRING = ti.STRING;
	STRING_ARRAY = Type.GetType("System.String[]");

	MONITOR = Type.GetType("System.Threading.Monitor");

	final Type[] sObject1 = new Type[] {OBJECT};

	//CONCAT_OBJECT = STRING.GetMethod("Concat", new Type[] {OBJECT});
	//CONCAT_STRING_STRING = STRING.GetMethod("Concat", new Type[] {STRING, STRING});
	CONCAT_OBJECT_OBJECT =
	    STRING.GetMethod("Concat", new Type[] {OBJECT, OBJECT});
	OBJECT_EQUALS = OBJECT.GetMethod("Equals", sObject1);
// 	MONITOR_PULSE = MONITOR.GetMethod("Pulse", sObject1);
// 	MONITOR_PULSE_ALL = MONITOR.GetMethod("PulseAll", sObject1);
// 	MONITOR_WAIT = MONITOR.GetMethod("Wait", sObject1);
// 	MONITOR_WAIT_TIMEOUT = MONITOR.GetMethod("Wait", new Type[] {OBJECT, INT});
	MONITOR_ENTER = MONITOR.GetMethod("Enter", sObject1);
	MONITOR_EXIT = MONITOR.GetMethod("Exit", sObject1);

	SCALA_BYTE    = getType("scala.Byte");
	SCALA_SHORT   = getType("scala.Short");
	SCALA_INT     = getType("scala.Int");
	SCALA_LONG    = getType("scala.Long");
	SCALA_FLOAT   = getType("scala.Float");
	SCALA_DOUBLE  = getType("scala.Double");
	SCALA_CHAR    = getType("scala.Char");
	SCALA_BOOLEAN = getType("scala.Boolean");
	SCALA_UNIT    = getType("scala.Unit");

 	RUNTIME_BOX_UNIT = getType("scala.runtime.RunTime")
 	    .GetMethod("box_uvalue", Type.EmptyTypes);
    }

    private boolean initialized = false;

    /*
     * Called from GenMSILPhase
     */
    public void init() {
	if (initialized)
	    return;
	final Symbol JOBJECT = defs.OBJECT_CLASS; //defs.getClass("java.lang.Object");
	final Symbol JSTRING = defs.STRING_CLASS;


	// initialize type mappings
	map(defs.ANY_CLASS, OBJECT);
	map(defs.ANYREF_CLASS, OBJECT);
	map(JOBJECT, OBJECT);
	map(JSTRING, STRING);

	// constants useful for method mappings
 	final scalac.symtab.Type UNBOXED_LONG =
 	    new scalac.symtab.Type.UnboxedType(TypeTags.LONG);
 	final scalac.symtab.Type UNBOXED_INT =
 	    new scalac.symtab.Type.UnboxedType(TypeTags.INT);
	final scalac.symtab.Type UNBOXED_CHAR =
	    new scalac.symtab.Type.UnboxedType(TypeTags.CHAR);

	final scalac.symtab.Type[] jEmpty = scalac.symtab.Type.EMPTY_ARRAY;
	final scalac.symtab.Type[] jString1 = new scalac.symtab.Type[]
	    {defs.STRING_TYPE()};
	final scalac.symtab.Type[] jInt1 = new scalac.symtab.Type[]
	    {UNBOXED_INT};
	final scalac.symtab.Type[] jInt2 = new scalac.symtab.Type[]
	    {UNBOXED_INT, UNBOXED_INT};
	final scalac.symtab.Type[] jStringInt = new scalac.symtab.Type[]
	    {defs.STRING_TYPE(), UNBOXED_INT};
	final scalac.symtab.Type[] jChar2 = new scalac.symtab.Type[]
	    {UNBOXED_CHAR, UNBOXED_CHAR};

	final Type[] sObject1 = new Type[] {OBJECT};
	final Type[] sString1 = new Type[] {STRING};
	final Type[] sString2 = new Type[] {STRING, STRING};
	final Type[] sChar1   = new Type[] {CHAR};
	final Type[] sCharInt2 = new Type[] {CHAR, INT};

	final Type ObjectImpl = Type.GetType("com.ms.vjsharp.lang.ObjectImpl");

	// map methods of java.lang.Object
	translateMethod(JOBJECT, "equals", OBJECT, "Equals");
	translateMethod(JOBJECT, "hashCode", OBJECT, "GetHashCode");
	translateMethod(JOBJECT, "toString", OBJECT, "ToString");
	translateMethod(JOBJECT, "finalize", OBJECT, "Finalize");
	translateMethod(JOBJECT, "wait", jEmpty, MONITOR, "Wait", sObject1);
	translateMethod(JOBJECT, "wait", new scalac.symtab.
			Type[] {UNBOXED_LONG}, // defs.LONG_TYPE
			MONITOR, "Wait",
			new Type[] {OBJECT, INT});
	translateMethod(JOBJECT, "notify", jEmpty, MONITOR, "Pulse", sObject1);
	translateMethod(JOBJECT, "notifyAll", jEmpty, MONITOR, "PulseAll", sObject1);
	//translateMethod(JOBJECT, "getClass", jEmpty, ObjectImpl, "getClass", sObject1);

	// map methods of java.lang.String
	//translateMethod(JSTRING, "equals",    STRING, "Equals");
	//translateMethod(JSTRING, "toString",  STRING, "ToString");
	translateMethod(JSTRING, "compareTo", STRING, "CompareTo");
	translateMethod(JSTRING, "length",    STRING, "get_Length");
	translateMethod(JSTRING, "charAt",    STRING, "get_Chars");
	translateMethod(JSTRING, "concat",  jString1, STRING, "Concat", sString2);
 	translateMethod(JSTRING, "indexOf", jInt1, STRING, "IndexOf", sChar1);
 	translateMethod(JSTRING, "indexOf", jInt2, STRING, "IndexOf", sCharInt2);
 	translateMethod(JSTRING, "indexOf", jString1, STRING, "IndexOf");
 	translateMethod(JSTRING, "indexOf", jStringInt, STRING, "IndexOf");
 	translateMethod(JSTRING, "lastIndexOf", jInt1, STRING, "LastIndexOf", sChar1);
 	translateMethod(JSTRING, "lastIndexOf", jInt2, STRING, "LastIndexOf", sCharInt2);
 	translateMethod(JSTRING, "lastIndexOf", jString1, STRING, "LastIndexOf");
 	translateMethod(JSTRING, "lastIndexOf", jStringInt, STRING, "LastIndexOf");
	translateMethod(JSTRING, "toLowerCase", jEmpty, STRING, "ToLower");
	translateMethod(JSTRING, "toUpperCase", jEmpty, STRING, "ToUpper");
	translateMethod(JSTRING, "startsWith",  jString1, STRING, "StartsWith");
	translateMethod(JSTRING, "endsWith",    jString1, STRING, "EndsWith");
	translateMethod(JSTRING, "substring",   jInt1, STRING, "Substring");
	translateMethod(JSTRING, "intern",      jEmpty, STRING, "Intern", sString1);
	translateMethod(JSTRING, "replace",     jChar2, STRING, "Replace");
	translateMethod(JSTRING, "toCharArray", STRING, "ToCharArray");

// 	translateMethod(defs.getModule("java.lang.Byte").moduleClass()
// 			, "parseByte", jString1, BYTE, "Parse");
// 	translateMethod(defs.getModule("java.lang.Short").moduleClass()
// 			, "parseShort", jString1, SHORT, "Parse");
// 	translateMethod(defs.getModule("java.lang.Integer").moduleClass()
// 			, "parseInt", jString1, INT, "Parse");
// 	translateMethod(defs.getModule("java.lang.Long").moduleClass()
// 			, "parseLong", jString1, LONG, "Parse");
// 	translateMethod(defs.getModule("java.lang.Float").moduleClass()
// 			, "parseFloat", jString1, FLOAT, "Parse");
// 	translateMethod(defs.getModule("java.lang.Double").moduleClass()
// 			, "parseDouble", jString1, DOUBLE, "Parse");

	SYM_SUBSTRING_INT_INT = lookupMethod(JSTRING, "substring", jInt2);
	SUBSTRING_INT_INT =
	    STRING.GetMethod("Substring", new Type[]{INT,INT});
	SYM_COMPARE_TO_IGNORE_CASE =
	    lookupMethod(JSTRING, "compareToIgnoreCase",  jString1);
	COMPARE_TO_IGNORE_CASE =
	    STRING.GetMethod("Compare", new Type[]{STRING, STRING, BOOLEAN});
	initialized = true;

	SCALA_SYMTAB_ATTR_CONSTR = ti.SCALA_SYMTAB_ATTR.GetConstructors()[0];

        scalac.symtab.Type argument = defs.array_TYPE(defs.STRING_TYPE());
        scalac.symtab.Type result = defs.void_TYPE();
        Symbol formal = Symbol.NONE.newTerm // !!! should be newVParam
            (Position.NOPOS, Modifiers.PARAM, Name.fromString("args"));
        formal.setInfo(argument);
        MAIN_METHOD_TYPE =
	    scalac.symtab.Type.MethodType(new Symbol[] {formal}, result);
    } // init()

    /*
     * Looks up the method with the corresponding signature
     */
    private Symbol lookupMethod(Symbol clazz, String name,
			scalac.symtab.Type[] paramTypes)
    {
	Symbol[] methods = clazz.members().
	    lookup(Name.fromString(name)).alternativeSymbols();
	search:
	for (int i = 0; i < methods.length; i++) {
	    switch (methods[i].info()) {
	    case MethodType(Symbol[] vparams, _):
		if (paramTypes.length != vparams.length)
		    continue;
		for (int j = 0; j < vparams.length; j++) {
		    if (!paramTypes[j].isSameAs(vparams[j].info()))
			continue search;
		}
		return methods[i];
	    default:
		continue;
	    }
	}
	return null;
    } // Symbol lookupMethod(...)

    /*
     * Format the signature of a method for better diagnostic printing.
     */
    private static String methodSignature(Symbol sym) {
	switch (sym.info()) {
	case MethodType(Symbol[] vparams, scalac.symtab.Type result):
	    StringBuffer s = new StringBuffer();
	    s.append(result); s.append(' ');
	    s.append(Debug.show(sym.owner())); s.append('.');
	    s.append(sym.name.toString());s.append('(');
	    for (int i = 0; i < vparams.length; i++) {
		if (i > 0) s.append(", ");
		//s.append(Debug.show(vparams[i].info()));
		s.append(vparams[i].info());
	    }
	    s.append(")");
	    return s.toString();
	default:
	    return "Symbol doesn't have a method type: " + Debug.show(sym);
	}
    }

    private static String methodSignature(Type[] params) {
	StringBuffer s = new StringBuffer();
	s.append('(');
	for (int i = 0; i < params.length; i++) {
	    if (i > 0)
		s.append(", ");
	    s.append(params[i]);
	}
	s.append(')');
	return s.toString();
    }

    /**
     * Create a mapping from method with symbol 'sym'
     * to the method newClazz.newName(params)
     */
    private void mapMethod(Symbol sym, Type newClazz, String name, Type[] params) {
	MethodInfo method = newClazz.GetMethod(name, params);
	assert method != null : "Cannot find translation for: "
	    + methodSignature(sym) + "->" + newClazz
	    + "::" + name + methodSignature(params);
	symbols2methods.put(sym, method);
    }

    /**
     * Create mapping between the specified two methods only
     */
    private void translateMethod(Symbol clazz, String name,
				 scalac.symtab.Type[] paramTypes,
			 Type newClazz, String newName, Type[] newParamTypes)
    {
	Symbol sym = lookupMethod(clazz, name, paramTypes);
	assert sym != null : "Cannot find method: " + name + " in class " +
	    Debug.show(clazz) + "; scope = " + Debug.show(clazz.members());
	mapMethod(sym, newClazz, newName, newParamTypes);
    }

    /**
     * Lookup the method and create mapping for all overloaded alternatives
     * to the corresponding methods in 'newClazz'
     */
    private void translateMethod(Symbol clazz, String name,
				 Type newClazz, String newName)
    {
	Symbol sym = clazz.lookup(Name.fromString(name));
	assert sym != null : "Cannot find method: " + name;
	translateMethod(sym, newClazz, newName);
    }

    /**
     *
     */
    private void translateMethod(Symbol clazz, String name,
			 scalac.symtab.Type[] paramTypes,
			 Type newClazz, String newName)
    {
	Type[] newParamTypes = new Type[paramTypes.length];
	for (int i = 0; i < paramTypes.length; i++)
	    newParamTypes[i] = getType(paramTypes[i]);
	translateMethod(clazz, name, paramTypes,
			newClazz, newName, newParamTypes);
    }

    /*
     * Create a mapping for the two methods.
     */
    private void translateMethod(Symbol sym, Type newClazz, String newName) {
	switch (sym.info()) {
	case MethodType(Symbol[] vparams, scalac.symtab.Type result):
	    Type[] params = new Type[vparams.length];
	    for (int i = 0; i < params.length; i++)
		params[i] = getType(vparams[i]);
	    mapMethod(sym, newClazz, newName, params);
	    break;
	case OverloadedType(Symbol[] alts, _):
	    for (int i = 0; i < alts.length; i++)
		translateMethod(alts[i], newClazz, newName);
	    return;
	default:
	    global.fail(Debug.show(sym) + " : " + Debug.show(sym.info()));
	}
    }

    //##########################################################################
    // tree treversal

    private static final Name MAIN_N = Name.fromString("main");

    private final Set types2create = new LinkedHashSet();

    private String assemName;

    private File outDir;

    private Symbol entryPoint;

    private boolean moreThanOneEntryPoint;
    public boolean moreThanOneEntryPoint() {
	return moreThanOneEntryPoint;
    }

    private CompilationUnit entryPointUnit;

    private int entryPointPos;

    /** - collects the symbols of all classes defined in the program
     *  - collects all entry points
     *  - gives the name of the new assembly
     */
    public void collectSymbols(CompilationUnit[] units) {
	types2create.clear();
	entryPoint = null;
	new CollectSymbols().traverse(units);

	// the assembly name supplied with the -o option (or null)
	assemName = global.args.assemname.value;

        if (assemName == null) {
            // assembly name not supplied with the -o option
            if (entryPoint != null) {
                // assume the name of the object defining the first entry point
                assemName = entryPoint.owner().name.toString();
            } else {
                // assume the name of the first source file
                assemName = entryPointUnit.source.getFile().getName();
                assert assemName.endsWith(".scala") : assemName;
                assemName = assemName.substring(0, assemName.length() - 6);
            }
        } else {
            File f = new File(assemName);
            outDir = f.getParentFile();
            assemName = f.getName();
        }
        if (outDir == null)
            outDir = new File(".");
    }

    /** Determines if the given symbol is an entry point:
     *    - the name is "main"
     *    - belongs to a top-level class
     *    - the type is Array[String] => Unit
     */
    public boolean isEntryPoint(Symbol main) {
	return main.name == MAIN_N
	    && main.owner().isModuleClass()
	    && main.owner().owner().isPackageClass()
	    && MAIN_METHOD_TYPE.isSameAs(main.info());
    }

    /** A traverser that collects the symbols of all classes defined
     *  in the program and all entry points.
     */
    private final class CollectSymbols extends Traverser {
	private CompilationUnit currUnit;
	public void traverse(CompilationUnit unit) {
	    currUnit = unit;
	    if (entryPointUnit == null)
		entryPointUnit = unit;
	    super.traverse(unit);
	}
	public void traverse(Tree tree) {
	    switch (tree) {
	    case ClassDef(_, _, _, _, _, Template impl):
		types2create.add(tree.symbol());
		traverse(impl);
		return;
	    case DefDef(_, _, _, _, _, _):
		Symbol sym = tree.symbol();
		if (isEntryPoint(sym)) {
		    if (entryPoint == null) {
			entryPoint = sym;
 			entryPointUnit = currUnit;
			entryPointPos = tree.pos;
		    } else
			moreThanOneEntryPoint = true;
		}
		return;
	    case ValDef(_, _, _, _):
		return;
	    default:
		super.traverse(tree);
	    }
	}
    }

    //##########################################################################

    // the Assembly the program is compiled into.
    private AssemblyBuilder msilAssembly;

    // the main module of the assembly
    private ModuleBuilder msilModule;

    /** Create the output assembly
     */
    void initAssembly() {
	AssemblyName an = new AssemblyName();
	an.Name = assemName;
	msilAssembly = AssemblyBuilder.DefineDynamicAssembly(an);
	msilModule = msilAssembly.DefineDynamicModule
	    (an.Name, an.Name + (entryPoint == null ? ".dll" : ".exe"));

	for (Iterator classes = types2create.iterator(); classes.hasNext();) {
	    Symbol clazz = (Symbol)classes.next();
	    createType(clazz);
	}
    }

    /** Finilize the code generation. Called from GenMSILPhase
     *  after processing all compilation units.
     */
    public void saveAssembly() {
	if (entryPoint != null) {
	    MethodInfo mainMethod = (MethodInfo)getMethod(entryPoint);
	    assert mainMethod != null : Debug.show(entryPoint);
	    FieldInfo moduleField = getModuleField(entryPoint.owner());
	    assert moduleField != null : Debug.show(entryPoint.owner());
	    MethodBuilder main = msilModule.DefineGlobalMethod
		("Main", MethodAttributes.Public | MethodAttributes.Static,
		 VOID, new Type[] {STRING_ARRAY} );
	    main.DefineParameter(0, 0, "args");
	    msilAssembly.SetEntryPoint(main);
	    ILGenerator code = main.GetILGenerator();
 	    if (global.args.debuginfo.value) {
		String fname = SourceRepresentation
		    .escape(entryPointUnit.source.getFile().getPath());
		code.setPosition(Position.line(entryPointPos), fname);
	    }

	    code.Emit(OpCodes.Ldsfld, moduleField);
	    code.Emit(OpCodes.Ldarg_0);
	    code.Emit(OpCodes.Callvirt, mainMethod);
	    code.Emit(OpCodes.Ret);
	}
	createTypes();
        File fassem = new File(outDir, assemName + ".il");
        String assemblyFilename = fassem.getPath();
	try {
	    msilAssembly.Save(assemblyFilename);
	    if (moreThanOneEntryPoint())
		global.warning("Setting " + Debug.show(entryPoint) +
			       " as an entry point");
	} catch (IOException e) {
	    global.error("Could not save " + assemblyFilename);
	}
    }

    /**
     * Finalizes ('bakes') the newly created types
     */
    private void createTypes() {
	Iterator iter = typeBuilders.iterator();
	while (iter.hasNext())
	    ((TypeBuilder)iter.next()).CreateType();
    }

    //##########################################################################
    /**
     * Creates bidirectional mapping from symbols to types.
     */
    private Type map(Symbol sym, Type type) {
	symbols2types.put(sym, type);
	if (sym.isClass())
	    types2symbols.put(type, sym);
	return type;
    }

    /**
     * Return the System.Type object with the given name.
     */
    private Type getType(String name) {
	return ti.getType(name);
    }

    /**
     * Return the System.Type object corresponding to the type of the symbol
     */
    public Type getType(Symbol sym) {
	if (sym == null) return null; // FIXME: assert sym != null ?
	if (sym == defs.ANY_CLASS || sym == defs.ANYREF_CLASS || sym == defs.OBJECT_CLASS)
	    return OBJECT;
	if (sym == defs.STRING_CLASS)
	    return STRING;

	Type type = (Type) symbols2types.get(sym);
	if (type != null) {
	    if (sym.isExternal()) {
		assert !(type instanceof TypeBuilder)
		    : Debug.show(sym) + " -> " + type.toString();
		return type;
	    } else if (types2create.contains(sym) && type instanceof TypeBuilder)
		return type;
	} else if (types2create.contains(sym))
	    type = createType(sym);
	if (type != null)
	    return type;

	type = (Type)ti.getMember(sym);
	if (type != null)
	    return map(sym, type);

	if (sym.isClass()) {
	    final Symbol owner = sym.owner();
	    if (owner.isClass()) {
		Type ownerType = getType(owner);
		assert ownerType != null : Debug.show(owner);
		type = ownerType.GetNestedType(sym.name.toString());
	    } else {
		String name = global.primitives.getCLRClassName(sym);
		type = getType(sym.isModuleClass() && !sym.isJava()
			       ? name + "$" : name);
	    }
	} else {
	    type = getType(sym.info());
	}
	if (type != null)
	    return map(sym, type);

	throw Debug.abort("Type resolution failed for " + Debug.show(sym));
    }

    /** Retrieve the System.Type from the scala type.
     */
    public Type getType(scalac.symtab.Type type) {
	switch (type) {
	case ThisType(Symbol s):
	    return getType(s);
 	case TypeRef(_, Symbol s, _):
	    return getType(s);
	case CompoundType(_, _):
	    return getType(type.symbol());
	case UnboxedType(int kind):
	    return getTypeFromKind(kind);
	case UnboxedArrayType(scalac.symtab.Type elemtp):
	    return ti.mkArrayType(getType(elemtp));
	case NoType:
	    return VOID;
	default:
	    throw Debug.abort(Debug.show(type));
	}
    }

    /** Retrieves the primitive datatypes given their kind
     */
    private Type getTypeFromKind(int kind) {
	switch (kind) {
	case TypeTags.CHAR:    return CHAR;
	case TypeTags.BYTE:    return BYTE;
	case TypeTags.SHORT:   return SHORT;
	case TypeTags.INT:     return INT;
	case TypeTags.LONG:    return LONG;
	case TypeTags.FLOAT:   return FLOAT;
	case TypeTags.DOUBLE:  return DOUBLE;
	case TypeTags.BOOLEAN: return BOOLEAN;
	case TypeTags.UNIT:    return VOID;
	case TypeTags.STRING:  return STRING;
	default:
	    throw new ApplicationError("Unknown kind: " + kind);
	}
    }

    public Type createType(Symbol clazz) {
	try { return createType0(clazz); }
	catch (Error e) {
	    throw Debug.abort(Debug.show(clazz), e);
	}
    }

    /**
     * Creates the TypeBuilder for a class.
     */
    public Type createType0(Symbol clazz) {
	assert types2create.contains(clazz) : Debug.show(clazz);
	Type type = (Type)symbols2types.get(clazz);
	if (type != null && type instanceof TypeBuilder)
	    return type;

	TypeBuilder staticType = null;
	final Symbol owner = clazz.owner();
	final String staticTypeName = owner.isClass()
	    ? clazz.nameString()
	    : global.primitives.getCLRClassName(clazz);
	final String typeName =
	    staticTypeName + (clazz.isModuleClass() ? "$" : "");
	final scalac.symtab.Type classType = clazz.info();
	switch (classType) {
	case CompoundType(scalac.symtab.Type[] baseTypes, _):
	    Type superType = null;
	    Type[] interfaces = null;
	    int inum = baseTypes.length;
	    if (clazz.isInterface()) {
		int baseIndex = 0;
		if (baseTypes[0].symbol() == defs.ANY_CLASS) {
		    --inum;
		    baseIndex = 1;
		}
		interfaces = new Type[inum];
		for (int i = 0; i < inum; i++) {
		    assert baseTypes[i + baseIndex].symbol().isInterface();
		    interfaces[i] = getType(baseTypes[i + baseIndex].symbol());
		}
	    } else {
		superType = getType(baseTypes[0].symbol());
		assert inum > 0;
		interfaces = new Type[inum - 1];
		for (int i = 1; i < inum; i++)
		    interfaces[i - 1] = getType(baseTypes[i].symbol());
	    }

	    type = (TypeBuilder) symbols2types.get(clazz);
	    if (type != null)
		return type;

	    if (owner.isPackageClass()) {  // i.e. top level class
		type = msilModule.DefineType
		    (typeName, translateTypeAttributes(clazz.flags, false),
		     superType, interfaces);
		if (clazz.isModuleClass()) {
		    Symbol module = owner.members().lookup(clazz.name.toTermName());
		    Symbol linkedClass = module.linkedClass();

 		    if (linkedClass == null || linkedClass.info().isError()) {
			staticType = msilModule.DefineType
 			    (staticTypeName,
 			     translateTypeAttributes(clazz.flags, false),
 			     OBJECT, Type.EmptyTypes);
 		    }
		}
	    } else {
		final Type outerType = (Type) getType(owner);
		// check if the type have not been created by
		// the (possible) creation of the outer type
		type = (TypeBuilder) symbols2types.get(clazz);
		if (type != null)
		    return type;

		assert outerType instanceof TypeBuilder : Debug.show(clazz);
		type = ((TypeBuilder)outerType).DefineNestedType
		    (typeName, translateTypeAttributes(clazz.flags, true),
		     superType, interfaces);
	    }
	    break;

	default:
	    global.fail("Symbol does not have a CompoundType: " +
			Debug.show(clazz));
	}
	typeBuilders.add(type);
	map(clazz, type);
	if (clazz.isModuleClass() && staticType != null) {
	    syms2staticTypes.put(clazz, staticType);
	}
	for (Scope.SymbolIterator syms = clazz.members().iterator();
	     syms.hasNext(); )
	    {
		Symbol member = syms.next();
		if (member.isMethod()) {
		    MethodBase m = createMethod(member);
		    if (staticType != null && m.IsPublic() && !m.IsConstructor()) {
			MethodBase sm = createMethod(staticType, member, true);
			syms2staticMethods.put(member, sm);
		    }
		}
		else if (!member.isClass() && !member.isModule()
			 && !member.isType())
		    createField(member);
	    }

	// adapted from Erasure; creates abstract method declarations
	// for the interface methods for which the class doesn't provide
	// implementation (requirement of the CLR)
	if (clazz.isClass() && !clazz.isInterface()) {
	    Set ifaces = new HashSet(getInterfacesOf(clazz));
	    Symbol svper = clazz.parents()[0].symbol();
	    ifaces.removeAll(getInterfacesOf(svper));
	    for (Iterator i = ifaces.iterator(); i.hasNext(); ) {
		Symbol iface = (Symbol)i.next();
		for (Scope.SymbolIterator members =
			 iface.members().iterator();
		     members.hasNext(); )
		    {
			Symbol method = members.next();
			if (!method.isTerm() || !method.isDeferred()) continue;
			Symbol overriding =
			    method.overridingSymbol(clazz.info(), true);
			if (overriding != method) continue;
			Symbol overridden = method
			    .overriddenSymbol(clazz.parents()[0], clazz, true);
			if (overridden.isNone()) {
			    Symbol newMethod = method.cloneSymbol(clazz);
			    newMethod.flags = (newMethod.flags & ~Modifiers.JAVA)
				| Modifiers.SYNTHETIC | Modifiers.DEFERRED;
			    createMethod(newMethod);
			}
		    }
	    }
	}

	return type;
    } // createType()


    private final Map interfaces/*<Symbol,Set<Symbol>>*/ = new HashMap();

    /** Adapted from Erasure. Returns a set of the interfaces
     *  implemented by the class.
     */
    private Set getInterfacesOf(Symbol clasz) {
        assert clasz.isClass(): Debug.show(clasz);
        Set set = (Set)interfaces.get(clasz);
        if (set == null) {
            set = new HashSet();
            interfaces.put(clasz, set);
            scalac.symtab.Type parents[] = clasz.parents();
            for (int i = 0; i < parents.length; i++)
                set.addAll(getInterfacesOf(parents[i].symbol()));
            if (clasz.isInterface()) set.add(clasz);
        }
        return set;
    }

    //##########################################################################
    // find/create methods

    public MethodBase getMethod(Symbol sym) {
	MethodBase method = null;
	try {
	    method = getMethod0(sym);
	} catch (Throwable e) {
	    printMapping(sym, symbols2methods);
	    throw Debug.abort(e);
	}
	return method;
    }

    private static String dumpSymbol(Symbol sym) {
	return Debug.show(sym) + " : " + Integer.toHexString(sym.flags);
    }
    private void printMapping(Symbol sym, Map map) {
	System.out.println("For symbol " + dumpSymbol(sym));
	for (Iterator entries = map.entrySet().iterator();
	     entries.hasNext(); )
	    {
		Map.Entry entry = (Map.Entry)entries.next();
		Symbol sentry = (Symbol)entry.getKey();
		if (sentry.owner() == sym.owner()
		    && sentry.name == sym.name)
		    {
			System.out.println("Existing mapping "
					   + dumpSymbol(sentry) + " => "
					   + entry.getValue());
		    }
	    }
	System.out.println("Scope of owner: " + sym.owner().members());
    }

    /**
     * Returns the MethodBase object corresponding to the symbol.
     */
    public MethodBase getMethod0(Symbol sym) {
	MethodBase method = (MethodBase) symbols2methods.get(sym);
	if (method != null)
	    return method;
	MemberInfo m = ti.getMember(sym);
	if (m != null && m instanceof MethodBase) {
	    method = (MethodBase) m;
	} else {
	    // force the creation of the declaring type
	    Type owner = getType(sym.owner());
	    assert owner != null : Debug.show(sym);
	    method = (MethodBase) symbols2methods.get(sym);
	    if (method != null)
		return method;
	    switch (sym.info()) {
	    case MethodType(Symbol[] vparams, scalac.symtab.Type result):
		Type[] params = new Type[vparams.length];
		for (int i = 0; i < params.length; i++)
		    params[i] = getType(vparams[i]);
		if (sym.isInitializer()) {
		    // The owner of a constructor is the outer class
		    // so get the result type of the constructor
		    method = owner.GetConstructor(params);
		    assert method != null : "cannot find " + owner
			+ "::.ctor" + methodSignature(params);
		} else {
		    method = owner instanceof TypeBuilder
			? findMethod(sym.owner(), sym)
			: owner.GetMethod(getMethodName(sym.name, params), params);
		}
		break;
	    default:
		global.fail("Symbol doesn't have a method type: " + Debug.show(sym));
	    }
	    assert method != null
		: Debug.show(owner) + " => Cannot find method: " + methodSignature(sym);
	}
	symbols2methods.put(sym, method);
	return method;
    }

    private String getMethodName(Name name, Type[] params) {
        if (name == Names.finalize && params.length == 0)
            return "Finalize";
        if (name == Names.toString && params.length == 0)
            return "ToString";
        if (name == Names.hashCode && params.length == 0)
            return "GetHashCode";
        if (name == Names.equals && params.length == 1 && params[0] == OBJECT)
            return "Equals";
        return name.toString();
    }

    private MethodBase findMethod(Symbol owner, Symbol member) {
	Symbol[] ms = owner.lookup(member.name).alternativeSymbols();
	for (int i = 0; i < ms.length; i++)
	    if (member.info().isSameAs(ms[i].info())) {
		MethodBase m = getMethod(ms[i]);
		if (m != null)
		    return m;
	    }
	throw Debug.abort("Couldn't find mapping for " + Debug.show(member));
    }

    private MethodBase createMethod(Symbol sym) {
	MethodBase method =
	    createMethod((TypeBuilder)getType(sym.owner()), sym, false);
	symbols2methods.put(sym, method);
	return method;
    }


    /**
     * Create the method corresponding to the symbol.

     * @param isStatic - forces the created method to be static
     */
    private MethodBase createMethod(TypeBuilder type, Symbol sym, boolean isStatic)
    {
	MethodBase method = null;
	switch (sym.info()) {
	case MethodType(Symbol[] vparams, scalac.symtab.Type result):
	    short attr = translateMethodAttributes(sym);
	    if (isStatic)
		attr = (short)((attr & ~MethodAttributes.Virtual
				& ~MethodAttributes.Final)
			       | MethodAttributes.Static);
	    Phase bkpCurrent = global.currentPhase;
	    global.currentPhase = backPhase;
	    Name name = sym.name; String sname = name.toString();
	    switch (sym.info()) {
	    case PolyType(Symbol[] tparams, _):
		if (tparams.length == 0) {
		    name = Name.fromString("get_" + name);
		}
		break;
	    default:
		if (sname.endsWith("_$eq")) {
		    name = Name.fromString("set_" + sname
					   .substring(0, sname.length() - 4));
		}
	    }
	    global.currentPhase = bkpCurrent;

            return createMethod(type, name, sym.info(), attr);
	default:
	    throw Debug.abort("Symbol doesn't have a method type: "
			      + Debug.show(sym));
	}
    }


    /**
     * Helper method to createMethod(Symbol)
     */
    private MethodBase createMethod(TypeBuilder type, Name name,
			    scalac.symtab.Type symType, short attr)
    {
	MethodBase method = null;
	switch (symType) {
	case MethodType(Symbol[] vparams, scalac.symtab.Type result):
	    Type[] params = new Type[vparams.length];
	    for (int i = 0; i < params.length; i++)
		params[i] = getType(vparams[i]);

	    if (name == Names.CONSTRUCTOR) {
		ConstructorBuilder constructor = type.DefineConstructor
		    (attr, CallingConventions.Standard, params);

		for (int i = 0; i < vparams.length; i++)
		    constructor.DefineParameter
			(i, 0/*ParameterAttributes.In*/, vparams[i].name.toString());
		return constructor;
	    } else {
		final String sname = getMethodName(name, params);
		MethodBuilder methodBuilder = type.DefineMethod
		    (sname, attr, getType(result), params);

		for (int i = 0; i < vparams.length; i++)
		    methodBuilder.DefineParameter
			(i, 0/*ParameterAttributes.In*/, vparams[i].name.toString());
		return methodBuilder;
	    }
	default:
	    throw Debug.abort("Method type expected: " + Debug.show(symType));
	}
    }

    //##########################################################################
    // find/create fields

    /** Returns the FieldInfo object corresponing to the symbol
     */
    public FieldInfo getField(Symbol sym) {
	FieldInfo field = (FieldInfo) symbols2fields.get(sym);
	if (field != null) return field;
	MemberInfo m = ti.getMember(sym);
	if (m != null && m instanceof FieldInfo) {
	    field = (FieldInfo)m;
	} else {
	    Type owner = getType(sym.owner());
	    field = owner.GetField(sym.name.toString());
	    if (field == null) {
		System.out.println("Fields of class " + owner);
		int bindingFlags = BindingFlags.DeclaredOnly
		    | BindingFlags.Instance | BindingFlags.Static
		    | BindingFlags.Public | BindingFlags.NonPublic;
		FieldInfo[] fields = owner.GetFields(bindingFlags);
		for (int i = 0; i < fields.length; i ++)
		    System.out.println("\t" + fields[i]);
	    }
	}
	assert field != null : "Cannot find field: " + Debug.show(sym);
	symbols2fields.put(sym, field);
	return field;
    }

    /*
     *
     */
    public FieldInfo createField(Symbol sym) {
	TypeBuilder owner = (TypeBuilder) getType(sym.owner());
	FieldInfo field = (FieldInfo) symbols2fields.get(sym);
	if (field != null) {
	    assert field instanceof FieldBuilder;
	    return field;
	}
	field = owner.DefineField(sym.name.toString(), getType(sym.type()),
				  translateFieldAttributes(sym));
	Object o = symbols2fields.put(sym, field);
	assert o == null : "Cannot re-define field: " + Debug.show(sym);
	return field;
    }

    /*
     *
     */
    private FieldInfo getModuleField(Type type) {
	Symbol sym = (Symbol) types2symbols.get(type);
	assert sym != null;
	return getModuleField(sym);
    }

    /**
     *
     */
    public MethodBuilder getStaticObjectMethod(Symbol sym) {
	assert sym.owner().isModuleClass() : Debug.show(sym);
	MethodBuilder method = (MethodBuilder)syms2staticMethods.get(sym);
	assert sym != null : Debug.show(sym);
	return method;
    }

    private Map syms2staticTypes = new HashMap();

    public TypeBuilder getStaticType(Symbol moduleClass) {
	getType(moduleClass); // force the creation of the Type
	TypeBuilder staticType = (TypeBuilder)syms2staticTypes.get(moduleClass);
// 	assert staticType != null : Debug.show(moduleClass)
// 	    + " : " + syms2staticTypes;
	return staticType;
    }

    /*
     *
     */
    private Symbol getTypeSymbol(scalac.symtab.Type type) {
	switch (type) {
	case TypeRef(_, Symbol s, _):
	    return s;
	default:
	    throw new ApplicationError("Cannot get symbol for: "
				       + Debug.show(type));
	}
    }

    /**
     * @return the field descriptor of the object instance
     */
    public FieldInfo getModuleField(Symbol sym) {
	assert sym.isModule() || sym.isModuleClass() : Debug.show(sym);
	FieldInfo moduleField = (FieldInfo) symbols2moduleFields.get(sym);
	if (moduleField == null) {
	    Symbol s = getTypeSymbol(sym.type());
	    if (sym != s) {
		moduleField = getModuleField(s);
	    } else {
		Type type = getType(sym);
		if (type instanceof TypeBuilder) {
		    TypeBuilder module = (TypeBuilder) type;
		    moduleField = module.DefineField
			(MODULE_S,
			 module,
			 (short)(FieldAttributes.Public
				 | FieldAttributes.InitOnly
				 | FieldAttributes.Static));
		} else {
		    moduleField = type.GetField(MODULE_S);
		    assert moduleField != null;
		}
	    }
	    symbols2moduleFields.put(sym, moduleField);
	}
	assert moduleField != null : Debug.show(sym);
	return moduleField;
    }

    //##########################################################################
    // translate Scala modifiers into attributes

    /** Translates Scala modifiers into TypeAttributes
     */
    private static int translateTypeAttributes(int mods, boolean nested) {
	int attr = TypeAttributes.AutoLayout | TypeAttributes.AnsiClass;

	if (Modifiers.Helper.isInterface(mods))
	    attr |= TypeAttributes.Interface;
	else
	    attr |= TypeAttributes.Class;

	if (Modifiers.Helper.isAbstract(mods))
	    attr |= TypeAttributes.Abstract;
	if (Modifiers.Helper.isFinal(mods))
	    attr |= TypeAttributes.Sealed;

	if (nested) {
	    if (Modifiers.Helper.isPrivate(mods))
		attr |= TypeAttributes.NestedPrivate;
	    else if (Modifiers.Helper.isProtected(mods))
		attr |= TypeAttributes.NestedFamORAssem;
	    else
		attr |= TypeAttributes.NestedPublic;
	} else {
	    if (Modifiers.Helper.isPrivate(mods))
		attr |= TypeAttributes.NotPublic;
	    else
		attr |= TypeAttributes.Public;
	}

	return attr;
    }

    /*
     * Translates Scala modifiers into FieldAttributes
     */
    private static short translateFieldAttributes(Symbol field) {
	int mods = field.flags;
	int attr = 0;

	if (Modifiers.Helper.isFinal(mods))
	    attr |= FieldAttributes.InitOnly;
	if (Modifiers.Helper.isPrivate(mods))
	    attr |= FieldAttributes.Private;
	else if (Modifiers.Helper.isProtected(mods))
	    attr |= FieldAttributes.FamORAssem;
	else
	    attr |= FieldAttributes.Public;

	if (field.owner().isJava() && field.owner().isModuleClass())
	    attr |= FieldAttributes.Static;

	return (short)attr;
    }

    /*
     * Translates Scala modifiers into MethodAttributes
     */
    private static short translateMethodAttributes(Symbol method)
    {
        int mods = method.flags;
        if (method.owner().isInterface())
            mods |= Modifiers.DEFERRED;
	int attr = MethodAttributes.HideBySig;
	if (!method.isInitializer()) {
	    attr |= MethodAttributes.Virtual;
 	    if (Modifiers.Helper.isFinal(mods))
 		attr |= MethodAttributes.Final;
	    if (Modifiers.Helper.isAbstract(mods))
		attr |= MethodAttributes.Abstract;
	}

	if (Modifiers.Helper.isPrivate(mods))
	    attr |= MethodAttributes.Private;
	else if (Modifiers.Helper.isProtected(mods))
	    attr |= MethodAttributes.Family;
	else
	    attr |= MethodAttributes.Public;

	return (short)attr;
    }

    //##########################################################################

} // class TypeCreator
