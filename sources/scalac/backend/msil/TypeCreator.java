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

    final private GenMSIL gen;
    final private Global global;
    final private Definitions defs;

    final private ArrayList typeBuilders = new ArrayList();

    final private Map types2symbols;
    final private Map symbols2types;
    final private Map symbols2fields;
    final private Map symbols2methods;
    final private Map symbols2moduleFields;

    static final String MODULE_S = "$MODULE";

    final Type BYTE;
    final Type CHAR;
    final Type SHORT;
    final Type INT;
    final Type LONG;
    final Type FLOAT;
    final Type DOUBLE;
    final Type BOOLEAN;
    final Type VOID;

    final Type OBJECT;
    final Type STRING;
    final Type STRING_ARRAY;

    final Type MONITOR;

//     static final MethodInfo CONCAT_OBJECT;
//     static final MethodInfo CONCAT_STRING_STRING;
    final MethodInfo CONCAT_OBJECT_OBJECT;
    final MethodInfo OBJECT_EQUALS;
    final MethodInfo MONITOR_PULSE;
    final MethodInfo MONITOR_PULSE_ALL;
    final MethodInfo MONITOR_WAIT;
    final MethodInfo MONITOR_WAIT_TIMEOUT;
    final MethodInfo MONITOR_ENTER;
    final MethodInfo MONITOR_EXIT;

    Type SCALA_BYTE;
    Type SCALA_SHORT;
    Type SCALA_INT;
    Type SCALA_LONG;
    Type SCALA_FLOAT;
    Type SCALA_DOUBLE;
    Type SCALA_CHAR;
    Type SCALA_BOOLEAN;
    Type SCALA_UNIT;

    FieldInfo RUNTIME_UNIT_VAL = null;

    Symbol SYM_SUBSTRING_INT_INT;
    MethodInfo SUBSTRING_INT_INT;
    Symbol SYM_COMPARE_TO_IGNORE_CASE;
    MethodInfo COMPARE_TO_IGNORE_CASE;

    ConstructorInfo SCALA_SYMTAB_ATTR_CONSTR;

    final CLRPackageParser ti;

    //##########################################################################

    TypeCreator(GenMSIL gen, GenMSILPhase phase) {
	this.gen = gen;
	this.global = gen.global;
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
    void init() {
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

	RUNTIME_UNIT_VAL = getType("scala.runtime.RunTime").GetField("UNIT_VAL");

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

    // looks up a method according to the signature
    Symbol lookupMethod(Symbol clazz, String name,
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

    static String methodSignature(Symbol sym) {
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
	    return "Symbol doesn't have a method type: " + dumpSym(sym);
	}
    }

    String paramList(scalac.symtab.Type[] paramTypes) {
	if (paramTypes.length == 0)
	    return "()";
	StringBuffer s = new StringBuffer("(");
	for (int i = 0; i < paramTypes.length; i++) {
 	    if (i > 0)
		s.append(", ");
 	    s.append(paramTypes[i]);
 	}
 	s.append(")");
	return s.toString();
    }

    /**
     * Create a mapping from method with symbol 'sym'
     * to the method newClazz.newName(params)
     */
    void mapMethod(Symbol sym, Type newClazz, String name, Type[] params) {
	MethodInfo method = newClazz.GetMethod(name, params);
	assert method != null : "Cannot find translation for: "
	    + methodSignature(sym);
	symbols2methods.put(sym, method);
    	//log("translateMethod: " + methodSignature(sym) + " -> " + method);
    }

    /**
     * create mapping between the specified two methods only
     */
    void translateMethod(Symbol clazz, String name,
			 scalac.symtab.Type[] paramTypes,
			 Type newClazz, String newName, Type[] newParamTypes)
    {
	Symbol sym = lookupMethod(clazz, name, paramTypes);
	assert sym != null : "Cannot find method: " + name + " in class " +
	    dumpSym(clazz);
	mapMethod(sym, newClazz, newName, newParamTypes);
    }

    /**
     * Lookup the method and create mapping for all overloaded alternatives
     * to the corresponding methods in 'newClazz'
     */
    void translateMethod(Symbol clazz, String name,
		    Type newClazz, String newName)
    {
	Symbol sym = clazz.lookup(Name.fromString(name));
	assert sym != null : "Cannot find method: " + name;
	translateMethod(sym, newClazz, newName);
    }

    /**
     */
    void translateMethod(Symbol clazz, String name,
			 scalac.symtab.Type[] paramTypes,
			 Type newClazz, String newName)
    {
	Type[] newParamTypes = new Type[paramTypes.length];
	for (int i = 0; i < paramTypes.length; i++)
	    newParamTypes[i] = getType(paramTypes[i]);
	translateMethod(clazz, name, paramTypes,
			newClazz, newName, newParamTypes);
    }

    // create a mapping for the two methods
    void translateMethod(Symbol sym, Type newClazz, String newName) {
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

    /** Finilizes ('bakes') the newly created types
     */
    public void createTypes() {
	Iterator iter = typeBuilders.iterator();
	while (iter.hasNext())
	    ((TypeBuilder)iter.next()).CreateType();
    }


    /**
     * creates bidirectional mapping from symbols to types
     */
    public void map(Symbol sym, Type type) {
	symbols2types.put(sym, type);
	if (sym.isClass())
	    types2symbols.put(type, sym);
    }

    Symbol getSymbol(Type t) {
	return (Symbol) types2symbols.get(t);
    }

    /**
     * Return the System.Type object with the given name
     */
    Type getType(String name) {
	return ti.getType(name);
    }


    /**
     * Return the System.Type object corresponding to the type of the symbol
     */
    Type getType(Symbol sym) {
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
			throw new RuntimeException("Cannot find type: "
						   + dumpSym(owner));
		    type = (Type) symbols2types.get(sym);
		    if (type != null)
			return type;
		    if (outer instanceof TypeBuilder)
			return createType(sym);
		    String name = sym.nameString()
			+ (sym.isModuleClass() ? "$" : "");
		    //log("getType: looking up nested type " + outer + "." + name);
		    type = outer.GetNestedType(name);
		} else {
		    String fullname = sym.type().symbol().fullNameString() +
			(sym.isModuleClass() ? "$" : "");
		    type = getType(fullname);
		}
		if (type == null)
		    type = createType(sym);
		break;

 	    case UnboxedArrayType(scalac.symtab.Type elemtp):
		type = getType(sym.info());
		break;

	    default:
		//log("getType: Going through the type: " + dumpSym(sym));
		type = getType(sym.info());
	    }
	}
	assert type != null : "Unable to find type: " + dumpSym(sym);
	map(sym, type);
	return type;
    }

    /** Retrieve the MSIL Type from the scala type
     */
    public Type getType(scalac.symtab.Type type) {
	//log("getType: " + Debug.show(type));
	switch (type) {
	case CompoundType(_, _):
	    return getType(type.symbol());

	case UnboxedType(int kind):
	    return getTypeFromKind(kind);

 	case TypeRef(_, Symbol s, _):
	    return getType(s);

	case UnboxedArrayType(scalac.symtab.Type elemtp):
	    // force the creation of the type
	    return ti.mkArrayType(getType(elemtp));
	case NoType:
	    return VOID;

	default:
	    global.fail("getType: " + Debug.show(type));
	}
	return null;
    }


    /** Creates a  TypeBuilder object corresponding to the symbol
     */
    public Type createType(Symbol clazz) {
	Type type = (Type)symbols2types.get(clazz);
	assert type == null : "Type " + type +
	    " already defined for symbol: " + dumpSym(clazz);


	//log("TypeCreator.createType(): creating type for " + dumpSym(clazz));
	final Symbol owner = clazz.owner();
	final String typeName =
	    (owner.isClass() ? clazz.nameString() : clazz.fullNameString()) +
	    (clazz.isModuleClass() ? "$" : "");
	final ModuleBuilder module = gen.currModule;

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


    /** Returns the MethodBase object corresponding to the symbol
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
	    //System.err.println("getMethod2: resolving " + dumpSym(sym));
	    //System.err.println("getMethod2: sym.owner() = " + dumpSym(sym.owner()));
	    //System.err.println("getMethod2: method owner = " + owner);
	    switch (sym.info()) {
	    case MethodType(Symbol[] vparams, scalac.symtab.Type result):
		Type[] params = new Type[vparams.length];
		for (int i = 0; i < params.length; i++)
		    params[i] = getType(vparams[i]);
		if (sym.isInitializer()) {
		    // The owner of a constructor is the outer class
		    // so get the result type of the constructor
		    //log("Resolving constructor: " + dumpSym(sym));
		    Type type = getType(sym.owner());
		    method = type.GetConstructor(params);
		} else {
		    String name = sym.name.toString();
		    if (sym.name == Names.toString) name = "ToString";
		    else if (sym.name == Names.hashCode) name = "GetHashCode";
		    else if (sym.name == Names.equals) name = "Equals";
		    //log("Resolving method " + dumpSym(sym));
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
	//log("getMethod2: method found: " + method);
	return method;
    }


    /** create the method corresponding to the symbol
     */
    MethodBase createMethod(Symbol sym) {
	final Symbol owner = sym.owner();
	MethodBase method = null;
	//log("createMethod: " + dumpSym(sym));
	//log("createMethod: sym.owner() = " + dumpSym(sym.owner()));
	switch (sym.info()) {
	case MethodType(Symbol[] vparams, scalac.symtab.Type result):
	    if (sym.isInitializer()) {
		//log("constructor symbol: " + dumpSym(sym));
		//TypeBuilder type = (TypeBuilder) getType(result);
		TypeBuilder type = (TypeBuilder) getType(sym.owner());
		method = createMethod(type, sym.name, sym.info(), sym.flags);
	    } else {
		int flags = ( owner.isJava() && owner.isModuleClass() ) ?
		    sym.flags | Modifiers.STATIC :
		    sym.flags;
		if (owner.isInterface())
		    flags |= Modifiers.DEFERRED;

		TypeBuilder type = (TypeBuilder)getType(owner);
		method = createMethod(type, sym.name, sym.info(), flags);
	    }
	    break;
	default:
	    assert false : "Symbol doesn't have a method type: " + dumpSym(sym);
	}
	assert method != null;
	symbols2methods.put(sym, method);
	return method;
    }


    /**
     */
    MethodBase createMethod(TypeBuilder type, Name name,
			    scalac.symtab.Type symType, int flags)
    {
	MethodBase method = null;
	switch (symType) {
	case MethodType(Symbol[] vparams, scalac.symtab.Type result):
	    Type[] params = new Type[vparams.length];
	    for (int i = 0; i < params.length; i++)
		params[i] = getType(vparams[i]);

	    if (name == Names.CONSTRUCTOR) {
		ConstructorBuilder constructor = type.DefineConstructor
		    (translateMethodAttributes(flags, true),
		     CallingConventions.Standard, params);

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
		    (sname, translateMethodAttributes(flags, false),
		     getType(result), params);

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
	    //log("getField: resolving symbol: " + dumpSym(sym));
	    //log("-->symbol.type() = " + Debug.show(sym.type()));
	    //log("-->symbol.info()" + Debug.show(sym.info()));
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
	assert field != null : "Cannot find field: " + dumpSym(sym);
	symbols2fields.put(sym, field);
	return field;
    }

    /**
     */
    public FieldInfo createField(Symbol sym) {
	//FieldBuilder field;
	//log("createField: resolving symbol: " + dumpSym(sym));
	//log("-->symbol.type() = " + Debug.show(sym.type()));
	//log("-->symbol.info()" + Debug.show(sym.info()));
	TypeBuilder owner = (TypeBuilder) getType(sym.owner());
	FieldInfo field = (FieldInfo) symbols2fields.get(sym);
	if (field != null) {
	    assert field instanceof FieldBuilder;
	    return field;
	}
	int flags = ( sym.owner().isJava() && sym.owner().isModuleClass() ) ?
	    sym.flags | Modifiers.STATIC : sym.flags;
	field = owner.DefineField(sym.name.toString(), getType(sym.type()),
				  translateFieldAttributes(flags));
	Object o = symbols2fields.put(sym, field);
	assert o == null : "Cannot re-define field: " + dumpSym(sym);
	return field;
    }


    /**
     */
    FieldInfo getModuleField(Type type) {
	Symbol sym = (Symbol) types2symbols.get(type);
	assert sym != null;
	return getModuleField(sym);
    }

    private Symbol getTypeSymbol(scalac.symtab.Type type) {
	switch (type) {
	case TypeRef(_, Symbol s, _):
	    return s;
	default:
	    logErr("Strange type: " + Debug.show(type));
	}
	return null;
    }


    /**
     * @return the field descriptor of the object instance
     */
    FieldInfo getModuleField(Symbol sym) {
	FieldInfo moduleField = null;
	if (sym.isModule() || sym.isModuleClass()) {
	    moduleField = (FieldInfo) symbols2moduleFields.get(sym);
	    if (moduleField == null) {
		//log("TypeCreator.getModuleField - " + dumpSym(sym));
		//log("\t-->type = " + Debug.show(sym.type()));
		//log("\t-->info = " + Debug.show(sym.info()));
		Symbol s = getTypeSymbol(sym.type());
		if (sym != s) {
		    //log("getModuleField: going through: " + dumpSym(s));
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
	    //throw new ApplicationError("getModuleField: not a module: " +
	    //dumpSym(sym));
	}
	return moduleField;
    }


    /** Translates Scala modifiers into TypeAttributes
     */
    public static int translateTypeAttributes(int mods, boolean nested) {
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


    /** Translates Scala modifiers into FieldAttributes
     */
    public static short translateFieldAttributes(int mods) {
	int attr = 0;

	if (Modifiers.Helper.isFinal(mods))
	    attr |= FieldAttributes.InitOnly;
	if (Modifiers.Helper.isPrivate(mods))
	    attr |= FieldAttributes.Private;
	else if (Modifiers.Helper.isProtected(mods))
	    attr |= FieldAttributes.FamORAssem;
	else
	    attr |= FieldAttributes.Public;

	if (Modifiers.Helper.isStatic(mods))
	    attr |= FieldAttributes.Static;

	return (short)attr;
    }


    /** Translates Scala modifiers into MethodAttributes
     */
    public static short translateMethodAttributes(int mods, boolean constructor)
    {
	int attr = MethodAttributes.HideBySig;
	if (!constructor) {
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

    /** Retrieves the primitive datatypes given their kind
     */
    final Type getTypeFromKind(int kind) {
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

    static String dumpSym(Symbol sym) {
	if (sym == null) return "<null>";
	if (sym == Symbol.NONE) return "NoSymbol";
	return "symbol = " + Debug.show(sym) +
	    " symbol.name = " + sym.name +
	    "; owner = " + Debug.show(sym.owner()) +
	    //"; type = " + Debug.show(sym.type()) +
	    "; info = " + Debug.show(sym.info()) +
	    "; kind = " + sym.kind +
	    "; flags = " + Integer.toHexString(sym.flags);
    }

    void log(String message) {
	System.out.println(message);
	//log(1, message);
        //global.reporter.printMessage(message);
    }

    void logErr(String message) {
	System.err.println(message);
    }

} // class TypeCreator
