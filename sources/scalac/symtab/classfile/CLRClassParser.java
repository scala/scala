/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2003, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab.classfile;

import scalac.Global;
import scalac.atree.AConstant;
import scalac.symtab.Symbol;
import scalac.symtab.SymbolLoader;
import scalac.symtab.Scope;
import scalac.symtab.Modifiers;
import scalac.symtab.Type.*;
import scalac.util.Name;
import scalac.util.Names;
import scalac.util.Debug;

import scala.tools.util.Position;
import ch.epfl.lamp.compiler.msil.*;

import java.util.Set;
import java.util.HashSet;
import java.util.Arrays;
import java.util.Iterator;

public class CLRClassParser extends SymbolLoader {

    //##########################################################################

    private static Name[] ENUM_CMP_NAMES = new Name[]
	{ Names.EQ, Names.NE, Names.LT, Names.LE, Names.GT, Names.GE };

    private static Name[] ENUM_BIT_LOG_NAMES = new Name[]
	{ Names.OR, Names.AND, Names.XOR };

    private static JavaTypeFactory make;

    private static final CLRPackageParser clrParser =
	CLRPackageParser.instance();

    private final Type type;

    public CLRClassParser(Global global, Type type) {
	super(global);
	this.type = type;
    }

    private Symbol clazz;
    private Scope members;
    private Symbol staticsClass;
    private Scope statics;
    scalac.symtab.Type clazzType;

    protected String doComplete(Symbol root) {
	clazz = root;
	clazz.owner().initialize(); //???

	if (make == null)
	    make = new JavaTypeCreator(global.definitions);

	clazz.flags = translateAttributes(type);
	Type[] ifaces = type.getInterfaces();
	scalac.symtab.Type[] baseTypes = new scalac.symtab.Type[ifaces.length+1];
	baseTypes[0] = type.BaseType == null ? make.anyType()
	    : getCLRType(type.BaseType);
	for (int i = 0; i < ifaces.length; i++)
	    baseTypes[i + 1] = getCLRType(ifaces[i]);
	members = new Scope();
	statics = new Scope();
	clazz.setInfo(scalac.symtab.Type.compoundType(baseTypes, members, clazz));
        Symbol staticsModule = clazz.linkedModule();
	staticsClass = staticsModule.moduleClass();
        assert staticsClass.isModuleClass(): Debug.show(staticsClass);
        scalac.symtab.Type staticsInfo = scalac.symtab.Type.compoundType
            (scalac.symtab.Type.EMPTY_ARRAY, statics, staticsClass);
        staticsClass.setInfo(staticsInfo);
        staticsModule.setInfo(make.classType(staticsClass));
        clazzType = make.classType(clazz);

	// import nested types
	Type[] nestedTypes = type.getNestedTypes();
	for (int i = 0; i < nestedTypes.length; i++) {
	    Type ntype = nestedTypes[i];
	    if (ntype.IsNestedPrivate() || ntype.IsNestedAssembly()
		|| ntype.IsNestedFamANDAssem())
		continue;
	    int j = ntype.FullName.lastIndexOf('.');
	    String n = (j < 0 ? ntype.FullName : ntype.FullName.substring(j + 1))
		.replace('+', '#');
	    Name classname = Name.fromString(n).toTypeName();
	    Name aliasname = Name.fromString(ntype.Name).toTypeName();
	    // put the class at the level of its outermost class
	    // the owner of a class is always its most specific package
	    CLRClassParser loader = new CLRClassParser(global, ntype);
	    Symbol nclazz =
		clazz.owner().newLoadedClass(JAVA, classname, loader, null);
	    clrParser.map(nclazz, ntype);
	    // create an alias in the module of the outer class
	    Symbol alias = staticsClass.newTypeAlias(Position.NOPOS,
                translateAttributes(ntype), aliasname, make.classType(nclazz));
	    statics.enterNoHide(alias);
	}

	FieldInfo[] fields = type.getFields();
	for (int i = 0; i < fields.length; i++) {
	    if (fields[i].IsPrivate() || fields[i].IsAssembly()
		|| fields[i].IsFamilyAndAssembly())
		continue;
	    int mods = translateAttributes(fields[i]);
	    Name name = Name.fromString(fields[i].Name);
	    scalac.symtab.Type fieldType = getCLRType(fields[i].FieldType);
	    if (fields[i].IsLiteral() && !fields[i].FieldType.IsEnum())
		fieldType = make.constantType(
                    getConstant(fieldType.symbol(), fields[i].getValue()));
	    Symbol owner = fields[i].IsStatic() ? staticsClass : clazz;
	    Symbol field = owner.newField(Position.NOPOS, mods, name);
	    field.setInfo(fieldType);
	    (fields[i].IsStatic() ? statics : members).enterOrOverload(field);
	    clrParser.map(field, fields[i]);
	}

	Set methodsSet = new HashSet(Arrays.asList(type.getMethods()));

	PropertyInfo[] props = type.getProperties();
	for (int i = 0; i < props.length; i++) {
	    scalac.symtab.Type proptype = getCLSType(props[i].PropertyType);
	    if (proptype == null)
		continue;

	    MethodInfo getter = props[i].GetGetMethod(true);
	    MethodInfo setter = props[i].GetSetMethod(true);
	    if (getter == null || getter.IsPrivate() ||
		getter.IsAssembly() || getter.IsFamilyAndAssembly())
		continue;
	    assert props[i].PropertyType == getter.ReturnType;
	    Name n;
	    Symbol method;
	    scalac.symtab.Type mtype;

	    ParameterInfo[] gparams = getter.GetParameters();
	    if (gparams.length == 0) {
		n = Name.fromString(props[i].Name);
		mtype =
		    scalac.symtab.Type.PolyType(Symbol.EMPTY_ARRAY, proptype);
	    } else {
		n = Names.apply;
		mtype = methodType(getter, getter.ReturnType);
	    }
	    Symbol owner = getter.IsStatic() ? staticsClass : clazz;
	    int mods = translateAttributes(getter);
	    method = owner.newMethod(Position.NOPOS, mods, n);
	    setParamOwners(mtype, method);
	    method.setInfo(mtype);
	    (getter.IsStatic() ? statics : members).enterOrOverload(method);
	    clrParser.map(method, getter);
	    assert methodsSet.contains(getter) : "" + getter;
	    methodsSet.remove(getter);

	    if (setter == null || setter.IsPrivate() ||
		setter.IsAssembly() || setter.IsFamilyAndAssembly())
		continue;
	    ParameterInfo[] sparams = setter.GetParameters();
	    assert getter.IsStatic() == setter.IsStatic();
	    assert setter.ReturnType == clrParser.VOID;
	    assert sparams.length == gparams.length + 1 : "" + getter + "; " + setter;

	    if (gparams.length == 0)
		n = Name.fromString(n.toString() + Names._EQ);
	    else n = Names.update;

	    mods = translateAttributes(setter);
	    method = owner.newMethod(Position.NOPOS, mods, n);
	    mtype = methodType(setter, global.definitions.UNIT_TYPE());
	    setParamOwners(mtype, method);
	    method.setInfo(mtype);
	    (setter.IsStatic() ? statics : members).enterOrOverload(method);
	    clrParser.map(method, setter);
	    assert methodsSet.contains(setter) : "" + setter;
	    methodsSet.remove(setter);
	}

	for (Iterator i = methodsSet.iterator(); i.hasNext(); ) {
	    MethodInfo method = (MethodInfo)i.next();
	    if ((clrParser.getSymbol(method) != null) || method.IsPrivate()
		|| method.IsAssembly() || method.IsFamilyAndAssembly())
		continue;
	    createMethod(method);
	}

	// for enumerations introduce comparison and bitwise logical operations;
	// the backend should recognize and replace them with comparison or
	// bitwise logical operations on the primitive underlying type
	if (type.IsEnum()) {
	    scalac.symtab.Type[] argTypes = new scalac.symtab.Type[] {clazzType};
	    int mods = Modifiers.JAVA | Modifiers.FINAL;
	    for (int i = 0; i < ENUM_CMP_NAMES.length; i++) {
		scalac.symtab.Type enumCmpType =
		    make.methodType(argTypes,
				    global.definitions.boolean_TYPE(),
				    scalac.symtab.Type.EMPTY_ARRAY);
		Symbol enumCmp = clazz.newMethod
		    (Position.NOPOS, mods, ENUM_CMP_NAMES[i]);
		setParamOwners(enumCmpType, enumCmp);
		enumCmp.setInfo(enumCmpType);
		members.enterOrOverload(enumCmp);
	    }
	    for (int i = 0; i < ENUM_BIT_LOG_NAMES.length; i++) {
		scalac.symtab.Type enumBitLogType = make.methodType
		    (argTypes, clazzType, scalac.symtab.Type.EMPTY_ARRAY);
		Symbol enumBitLog = clazz.newMethod
		    (Position.NOPOS, mods, ENUM_BIT_LOG_NAMES[i]);
		setParamOwners(enumBitLogType, enumBitLog);
		enumBitLog.setInfo(enumBitLogType);
		members.enterOrOverload(enumBitLog);
	    }
	}

	ConstructorInfo[] constrs = type.getConstructors();
	for (int i = 0; i < constrs.length; i++) {
	    if (constrs[i].IsStatic() || constrs[i].IsPrivate()
		|| constrs[i].IsAssembly() || constrs[i].IsFamilyAndAssembly())
		continue;
	    createConstructor(constrs[i]);
	}

	Symbol constr = clazz.primaryConstructor();
	if (!constr.isInitialized()) {
	    constr.setInfo(scalac.symtab.Type.MethodType
				(Symbol.EMPTY_ARRAY, clazzType));
	    if ((clazz.flags & Modifiers.INTERFACE) == 0)
		constr.flags |= Modifiers.PRIVATE;
	}

	return type + " from assembly " + type.Assembly;
    }


    private void createConstructor(ConstructorInfo constructor) {
	scalac.symtab.Type mtype = methodType(constructor, clazzType);
	if (mtype == null)
	    return;
	Symbol constr = clazz.primaryConstructor();
	int mods = translateAttributes(constructor);
	if (constr.isInitialized()) {
	    constr = clazz.newConstructor(Position.NOPOS, mods);
	    clazz.addConstructor(constr);
	} else {
	    constr.flags = mods;
	}
	setParamOwners(mtype, constr);
	constr.setInfo(mtype);
	clrParser.map(constr, constructor);
    }

    private void createMethod(MethodInfo method) {
	scalac.symtab.Type mtype =
	    methodType(method, method.ReturnType);
	if (mtype == null)
	    return;
	int mods = translateAttributes(method);
	Symbol owner = method.IsStatic() ? staticsClass : clazz;
	Symbol methodSym =
	    owner.newMethod(Position.NOPOS, mods, getName(method));
	setParamOwners(mtype, methodSym);
	methodSym.setInfo(mtype);
	(method.IsStatic() ? statics : members).enterOrOverload(methodSym);
	clrParser.map(methodSym, method);
    }

    private Name getName(MethodInfo method) {
        final String name = method.Name;
        if (method.IsStatic()) return Name.fromString(name);
        final ParameterInfo[] params = method.GetParameters();
	if (name.equals("GetHashCode") && params.length == 0)
            return Names.hashCode;
	if (name.equals("ToString") && params.length == 0)
            return Names.toString;
	if (name.equals("Finalize") && params.length == 0)
            return Names.finalize;
	if (name.equals("Equals") && params.length == 1
            && params[0].ParameterType == clrParser.OBJECT)
            return Names.equals;
	return Name.fromString(name);
    }

    //##########################################################################

    private String initializeJavaLangObject(Symbol sym) {
	MethodInfo[] methods = type.getMethods();
	return "java.lang.Object from internal data";
    }

    private String initializeJavaLangString(Symbol sym) {
	return "java.lang.String from internal data";
    }

    //##########################################################################

    private Type[] getParamTypes(MethodBase method) {
	ParameterInfo[] params = method.GetParameters();
	Type[] paramTypes = new Type[params.length];
	for (int i = 0; i < params.length; i++)
	    paramTypes[i] = params[i].ParameterType;
	return paramTypes;
    }

    private scalac.symtab.Type methodType(MethodBase method, Type rettype) {
	scalac.symtab.Type rtype = getCLSType(rettype);
	return rtype == null ? null : methodType(method, rtype);
    }

    /** Return a method type for the given method. */
    private scalac.symtab.Type methodType(MethodBase method,
					    scalac.symtab.Type rettype)
    {
	return methodType(getParamTypes(method), rettype);
    }

    /** Return a method type for the provided argument types and return type. */
    private scalac.symtab.Type methodType(Type[] argtypes,
					  scalac.symtab.Type rettype)
    {
	scalac.symtab.Type[] ptypes = new scalac.symtab.Type[argtypes.length];
	for (int i = 0; i < argtypes.length; i++) {
	    ptypes[i] = getCLSType(argtypes[i]);
	    if (ptypes[i] == null)
		return null;
	}
	return make.methodType(ptypes, rettype, scalac.symtab.Type.EMPTY_ARRAY);
    }

    private void setParamOwners(scalac.symtab.Type type, Symbol owner) {
	switch (type) {
	case PolyType(Symbol[] params, scalac.symtab.Type restype):
	    for (int i = 0; i < params.length; i++) params[i].setOwner(owner);
	    setParamOwners(restype, owner);
	    return;
	case MethodType(Symbol[] params, scalac.symtab.Type restype):
	    for (int i = 0; i < params.length; i++) params[i].setOwner(owner);
	    setParamOwners(restype, owner);
	    return;
	}
    }

    //##########################################################################

    private scalac.symtab.Type getClassType(Type type) {
	assert type != null;
	scalac.symtab.Type res =
	    make.classType(type.FullName.replace('+', '.'));
	if (res.isError())
	    global.error("unknown class reference " + type.FullName);
	return res;
    }

    private scalac.symtab.Type getCLSType(Type type) {
	if (type == clrParser.BYTE || type == clrParser.USHORT
	    || type == clrParser.UINT || type == clrParser.ULONG
	    || type.IsNotPublic() || type.IsNestedPrivate()
	    || type.IsNestedAssembly() || type.IsNestedFamANDAssem()
	    || type.IsPointer()
	    || (type.IsArray() && getCLSType(type.GetElementType()) == null))
	    return null;
	//Symbol s = clrParser.getSymbol(type);
	//scalac.symtab.Type t = s != null ? make.classType(s) : getCLRType(type);
	return getCLRType(type);
    }

    private scalac.symtab.Type getCLRType(Type type) {
	if (type == clrParser.OBJECT)
	    return make.objectType();
	if (type == clrParser.STRING)
	    return make.stringType();
	if (type == clrParser.VOID)
	    return make.voidType();
	if (type == clrParser.BOOLEAN)
	    return make.booleanType();
	if (type == clrParser.CHAR)
	    return make.charType();
	if (type == clrParser.BYTE || type == clrParser.UBYTE)
	    return make.byteType();
	if (type == clrParser.SHORT || type == clrParser.USHORT)
	    return make.shortType();
	if (type == clrParser.INT || type == clrParser.UINT)
	    return make.intType();
	if (type == clrParser.LONG || type == clrParser.ULONG)
	    return make.longType();
	if (type == clrParser.FLOAT)
	    return make.floatType();
	if (type == clrParser.DOUBLE)
	    return make.doubleType();
	if (type.IsArray())
	    return make.arrayType(getCLRType(type.GetElementType()));
	Symbol s = clrParser.getSymbol(type);
	return s != null ? make.classType(s) : getClassType(type);
    }

    public AConstant getConstant(Symbol base, Object value) {
        if (base == global.definitions.BOOLEAN_CLASS)
            return AConstant.BOOLEAN(((Number)value).intValue() != 0);
        if (base == global.definitions.BYTE_CLASS)
            return AConstant.BYTE(((Number)value).byteValue());
        if (base == global.definitions.SHORT_CLASS)
            return AConstant.SHORT(((Number)value).shortValue());
        if (base == global.definitions.CHAR_CLASS)
            return AConstant.CHAR(((Character)value).charValue());
        if (base == global.definitions.INT_CLASS)
            return AConstant.INT(((Number)value).intValue());
        if (base == global.definitions.LONG_CLASS)
            return AConstant.LONG(((Number)value).longValue());
        if (base == global.definitions.FLOAT_CLASS)
            return AConstant.FLOAT(((Number)value).floatValue());
        if (base == global.definitions.DOUBLE_CLASS)
            return AConstant.DOUBLE(((Number)value).doubleValue());
        if (base == global.definitions.STRING_CLASS)
            return AConstant.STRING((String)value);
    	throw Debug.abort("illegal value", Debug.show(value, base));
    }

    private static int translateAttributes(Type type) {
	int mods = Modifiers.JAVA;
	if (type.IsNotPublic() || type.IsNestedPrivate()
	    || type.IsNestedAssembly() || type.IsNestedFamANDAssem())
	    mods |= Modifiers.PRIVATE;
	else if (type.IsNestedFamily() || type.IsNestedFamORAssem())
	    mods |= Modifiers.PROTECTED;
	if (type.IsAbstract())
	    mods |= Modifiers.ABSTRACT;
	if (type.IsSealed())
	    mods |= Modifiers.FINAL;
	if (type.IsInterface())
	    mods |= Modifiers.INTERFACE | Modifiers.TRAIT | Modifiers.ABSTRACT;

	return mods;
    }

    private static int translateAttributes(FieldInfo field) {
	int mods = Modifiers.JAVA;
	if (field.IsPrivate() || field.IsAssembly() || field.IsFamilyAndAssembly())
	    mods |= Modifiers.PRIVATE;
	else if (field.IsFamily() || field.IsFamilyOrAssembly())
	    mods |= Modifiers.PROTECTED;
	if (field.IsInitOnly())
	    mods |= Modifiers.FINAL;
	else
	    mods |= Modifiers.MUTABLE;

	return mods;
    }

    private static int translateAttributes(MethodBase method) {
	int mods = Modifiers.JAVA;
	if (method.IsPrivate() || method.IsAssembly() || method.IsFamilyAndAssembly())
	    mods |= Modifiers.PRIVATE;
	else if (method.IsFamily() || method.IsFamilyOrAssembly())
	    mods |= Modifiers.PROTECTED;
	if (method.IsAbstract())
	    mods |= Modifiers.DEFERRED;

	return mods;
    }
}
