/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.backend.msil;

import scalac.Global;
import scalac.ApplicationError;
import scalac.ast.Tree;
import scalac.util.Debug;
import scalac.util.Name;
import scalac.util.Names;
import scalac.symtab.Kinds;
import scalac.symtab.TypeTags;
import scalac.symtab.Symbol;
import scalac.symtab.Scope;
import scalac.symtab.Modifiers;
import scalac.symtab.Definitions;
import scalac.symtab.classfile.CLRPackageParser;

import Tree.*;

import ch.epfl.lamp.compiler.msil.*;
import ch.epfl.lamp.compiler.msil.emit.*;

import java.util.Map;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Set;
import java.util.LinkedHashSet;

/**
 * Creates System.Reflection objects corresponding to
 * Scala symbols
 *
 * @author Nikolay Mihaylov
 * @version 1.1
 */

final class TypeCreator {

    private final GenMSIL gen;
    private final Global global;
    private final Definitions defs;

    private final ArrayList typeBuilders = new ArrayList();

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

    private final MethodInfo MONITOR_PULSE;
    private final MethodInfo MONITOR_PULSE_ALL;
    private final MethodInfo MONITOR_WAIT;
    private final MethodInfo MONITOR_WAIT_TIMEOUT;

    public Type SCALA_BYTE;
    public Type SCALA_SHORT;
    public Type SCALA_INT;
    public Type SCALA_LONG;
    public Type SCALA_FLOAT;
    public Type SCALA_DOUBLE;
    public Type SCALA_CHAR;
    public Type SCALA_BOOLEAN;
    public Type SCALA_UNIT;

    public MethodInfo RUNTIME_BOX_UNIT = null;

    public Symbol SYM_SUBSTRING_INT_INT;
    public MethodInfo SUBSTRING_INT_INT;
    public Symbol SYM_COMPARE_TO_IGNORE_CASE;
    public MethodInfo COMPARE_TO_IGNORE_CASE;

    public ConstructorInfo SCALA_SYMTAB_ATTR_CONSTR;

    private final CLRPackageParser ti;

    //##########################################################################

    TypeCreator(Global global, GenMSIL gen, GenMSILPhase phase) {
	this.global = global;
	this.gen = gen;
	this.defs = global.definitions;

	ti = CLRPackageParser.instance;

	types2symbols = phase.types2symbols;
	symbols2types = phase.symbols2types;
	symbols2fields = phase.symbols2fields;
	symbols2methods = phase.symbols2methods;
	symbols2moduleFields = phase.symbols2moduleFields;

	BYTE    = Type.GetType("System.SByte");
	CHAR    = Type.GetType("System.Char");
	SHORT   = Type.GetType("System.Int16");
	INT     = Type.GetType("System.Int32");
	LONG    = Type.GetType("System.Int64");
	FLOAT   = Type.GetType("System.Single");
	DOUBLE  = Type.GetType("System.Double");
	BOOLEAN = Type.GetType("System.Boolean");
	VOID    = Type.GetType("System.Void");
	ENUM    = Type.GetType("System.Enum");

	OBJECT = Type.GetType("System.Object");
	STRING = Type.GetType("System.String");
	STRING_ARRAY = Type.GetType("System.String[]");

	MONITOR = Type.GetType("System.Threading.Monitor");

	final Type[] sObject1 = new Type[] {OBJECT};

	//CONCAT_OBJECT = STRING.GetMethod("Concat", new Type[] {OBJECT});
	//CONCAT_STRING_STRING = STRING.GetMethod("Concat", new Type[] {STRING, STRING});
	CONCAT_OBJECT_OBJECT =
	    STRING.GetMethod("Concat", new Type[] {OBJECT, OBJECT});
	OBJECT_EQUALS = OBJECT.GetMethod("Equals", sObject1);
	MONITOR_PULSE = MONITOR.GetMethod("Pulse", sObject1);
	MONITOR_PULSE_ALL = MONITOR.GetMethod("PulseAll", sObject1);
	MONITOR_WAIT = MONITOR.GetMethod("Wait", sObject1);
	MONITOR_WAIT_TIMEOUT = MONITOR.GetMethod("Wait", new Type[] {OBJECT, INT});
	MONITOR_ENTER = MONITOR.GetMethod("Enter", sObject1);
	MONITOR_EXIT = MONITOR.GetMethod("Exit", sObject1);
    }

    private boolean initialized = false;

    /*
     * Called from GenMSIL
     */
    public void init() {
	if (initialized)
	    return;

	final Symbol JOBJECT = defs.JAVA_OBJECT_CLASS;
	final Symbol JSTRING = defs.JAVA_STRING_CLASS;

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
	translateMethod(JOBJECT, "getClass", jEmpty, ObjectImpl, "getClass", sObject1);

	// map methods of java.lang.String
	translateMethod(JSTRING, "equals",    STRING, "Equals");
	translateMethod(JSTRING, "toString",  STRING, "ToString");
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

	translateMethod(defs.getClass(Name.fromString("java.lang.Byte")).module()
			, "parseByte", jString1, BYTE, "Parse");
	translateMethod(defs.getClass(Name.fromString("java.lang.Short")).module()
			, "parseShort", jString1, SHORT, "Parse");
	translateMethod(defs.getClass(Name.fromString("java.lang.Integer")).module()
			, "parseInt", jString1, INT, "Parse");
	translateMethod(defs.getClass(Name.fromString("java.lang.Long")).module()
			, "parseLong", jString1, LONG, "Parse");
	translateMethod(defs.getClass(Name.fromString("java.lang.Float")).module()
			, "parseFloat", jString1, FLOAT, "Parse");
	translateMethod(defs.getClass(Name.fromString("java.lang.Double")).module()
			, "parseDouble", jString1, DOUBLE, "Parse");

	SYM_SUBSTRING_INT_INT = lookupMethod(JSTRING, "substring", jInt2);
	SUBSTRING_INT_INT =
	    STRING.GetMethod("Substring", new Type[]{INT,INT});
	SYM_COMPARE_TO_IGNORE_CASE =
	    lookupMethod(JSTRING, "compareToIgnoreCase",  jString1);
	COMPARE_TO_IGNORE_CASE =
	    STRING.GetMethod("Compare", new Type[]{STRING, STRING, BOOLEAN});
	initialized = true;

	Type scalaSymtab = Type.GetType("scala.support.SymtabAttribute");
	SCALA_SYMTAB_ATTR_CONSTR = scalaSymtab.GetConstructors()[0];
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
	    s.append(sym.name.toString()); s.append('(');
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

    /**
     * Create a mapping from method with symbol 'sym'
     * to the method newClazz.newName(params)
     */
    private void mapMethod(Symbol sym, Type newClazz, String name, Type[] params) {
	MethodInfo method = newClazz.GetMethod(name, params);
	assert method != null : "Cannot find translation for: "
	    + methodSignature(sym);
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
	    Debug.show(clazz);
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
	    global.fail("" + Debug.show(sym.info()));
	}
    }

    /**
     * Finalizes ('bakes') the newly created types
     */
    public void createTypes() {
	Iterator iter = typeBuilders.iterator();
	while (iter.hasNext())
	    ((TypeBuilder)iter.next()).CreateType();
    }

    /**
     * Creates bidirectional mapping from symbols to types.
     */
    private void map(Symbol sym, Type type) {
	symbols2types.put(sym, type);
	if (sym.isClass())
	    types2symbols.put(type, sym);
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
	if (sym == null) return null;
	Type type = (Type) symbols2types.get(sym);
	if (type != null)
	    return type;
	MemberInfo m = ti.getMember(sym);
	if (m != null && m instanceof Type)
	    type = (Type)m;
 	else if (sym.isJava()) {
// 	if (sym.isExternal()) {
	    type = getType(sym.fullNameString());
	}
	if (type == null) {
	    final Symbol owner = sym.owner();
	    switch (sym.info()) {
	    case CompoundType(_, _):
		if (sym.owner().isClass()) {
		    Type outer = getType(sym.owner());
		    if (outer == null)
			throw new ApplicationError("Cannot find type: "
						   + Debug.show(owner));
		    type = (Type) symbols2types.get(sym);
		    if (type != null)
			return type;
		    if (outer instanceof TypeBuilder)
			return createType(sym);
		    String name = sym.nameString()
			+ (sym.isModuleClass() ? "$" : "");
		    type = outer.GetNestedType(name);
		} else {
		    String fullname = sym.type().symbol().fullNameString() +
			(sym.isModuleClass() ? "$" : "");
		    type = getType(fullname);
		}
		if (type == null) {
		    if (!sym.isExternal())
			type = createType(sym);
		    else
			global.fail("Cannot find type " + Debug.show(sym) +
			    "; use the '-r' option to specify its assembly");
		}
		break;

 	    case UnboxedArrayType(scalac.symtab.Type elemtp):
		type = getType(sym.info());
		break;

	    default:
		type = getType(sym.info());
	    }
	}
	assert type != null : "Unable to find type: " + Debug.show(sym);
	map(sym, type);
	return type;
    }

    /**
     * Retrieve the System.Type from the scala type.
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
	    global.fail("getType: " + Debug.show(type));
	}
	return null;
    }

    /**
     * Creates the TypeBuilder for a class.
     */
    public Type createType(Symbol clazz) {
	assert !clazz.isExternal() : "Can not create type " + Debug.show(clazz);
	Type type = (Type)symbols2types.get(clazz);
	assert type == null : "Type " + type +
	    " already defined for symbol: " + Debug.show(clazz);


	final Symbol owner = clazz.owner();
	final String typeName =
	    (owner.isClass() ? clazz.nameString() : clazz.fullNameString()) +
	    (clazz.isModuleClass() ? "$" : "");
	final ModuleBuilder module = gen.getCurrentModule();

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
	    if (owner.isRoot() || owner.isPackage()) {  // i.e. top level class
		type = module.DefineType
		    (typeName, translateTypeAttributes(clazz.flags, false),
		     superType, interfaces);
	    } else {
		final Type outerType = (Type) getType(owner);
		// check if the type have not been created by
		// the (possible) creation of the outer type
		type = (Type) symbols2types.get(clazz);
		if (type != null)
		    return type;
		if (outerType instanceof TypeBuilder)
		    type = ((TypeBuilder)outerType).DefineNestedType
			(typeName, translateTypeAttributes(clazz.flags, true),
			 superType, interfaces);
		else return outerType.GetNestedType(typeName);
	    }
	    break;

	default:
	    global.fail("Symbol does not have a CompoundType: " +
			Debug.show(clazz));
	}
	typeBuilders.add(type);
	map(clazz, type);

	// create the members of the class but not nested classes
	// they will be created when the tree is being traversed (at latest)
	for (Scope.SymbolIterator syms = clazz.members().iterator();
	     syms.hasNext(); )
	    {
		Symbol[] members = syms.next().alternativeSymbols();
		for (int i = 0; i < members.length; i++) {
		    if (members[i].isMethod() /*&& !members[i].isModule()*/) {
			createMethod(members[i]);
		    }
		    else if (!members[i].isClass()
			     && !members[i].isModule()
			     && !members[i].isType())
			{
			    createField(members[i]);
			}
		}
	    }


	return type;
    } // createType()


    /**
     * Returns the MethodBase object corresponding to the symbol.
     */
    public MethodBase getMethod(Symbol sym) {
	MethodBase method = (MethodBase) symbols2methods.get(sym);
	if (method != null)
	    return method;
	MemberInfo m = ti.getMember(sym);
	if (m != null && m instanceof MethodBase) {
	    method = (MethodBase) m;
	}
	else {
	    // force the creation of the declaring type
	    Type owner = getType(sym.owner());
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
		    Type type = getType(sym.owner());
		    method = type.GetConstructor(params);
		} else {
		    String name = sym.name.toString();
		    if (sym.name == Names.toString) name = "ToString";
		    else if (sym.name == Names.hashCode) name = "GetHashCode";
		    else if (sym.name == Names.equals) name = "Equals";
		    Type type = getType(sym.owner());
		    method = type.GetMethod(name, params);
		}
		break;
	    default:
		global.fail("Symbol doesn't have a method type: " + Debug.show(sym));
	    }
	}
	assert method != null : "Cannot find method: " + methodSignature(sym);
	symbols2methods.put(sym, method);
	return method;
    }


    /**
     * Create the method corresponding to the symbol.
     */
    private MethodBase createMethod(Symbol sym) {
	MethodBase method = null;
	switch (sym.info()) {
	case MethodType(Symbol[] vparams, scalac.symtab.Type result):
            TypeBuilder type = (TypeBuilder)getType(sym.owner());
            method = createMethod
                (type, sym.name, sym.info(), translateMethodAttributes(sym));
	    break;
	default:
	    assert false : "Symbol doesn't have a method type: " + Debug.show(sym);
	}
	assert method != null;
	symbols2methods.put(sym, method);
	return method;
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
		method = constructor;
	    } else {
		String sname;
		if (name == Names.toString) sname = "ToString";
		else if (name == Names.equals) sname = "Equals";
		else if (name == Names.hashCode) sname = "GetHashCode";
		else  sname = name.toString();

		MethodBuilder methodBuilder = type.DefineMethod
		    (sname, attr, getType(result), params);

		for (int i = 0; i < vparams.length; i++)
		    methodBuilder.DefineParameter
			(i, 0/*ParameterAttributes.In*/, vparams[i].name.toString());
		method =  methodBuilder;
	    }
	    break;
	default:
	    assert false : "Method type expected: " + Debug.show(symType);
	}

	return method;
    }


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
	FieldInfo moduleField = null;
	if (sym.isModule() || sym.isModuleClass()) {
	    moduleField = (FieldInfo) symbols2moduleFields.get(sym);
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
	} else {
	}
	return moduleField;
    }

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
	//else if (Modifiers.Helper.isProtected(mods))
	//  attr |= MethodAttributes.FamORAssem;
	else
	    attr |= MethodAttributes.Public;

	return (short)attr;
    }

    /*
     * Retrieves the primitive datatypes given their kind
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

} // class TypeCreator
