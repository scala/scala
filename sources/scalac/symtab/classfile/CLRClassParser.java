/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2003, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab.classfile;

import scalac.Global;
import scalac.symtab.Symbol;
import scalac.symtab.TermSymbol;
import scalac.symtab.ClassSymbol;
import scalac.symtab.Scope;
import scalac.symtab.Modifiers;
import scalac.util.Name;
import scalac.util.Names;
import scalac.util.Debug;

import ch.epfl.lamp.util.Position;
import ch.epfl.lamp.compiler.msil.*;

public class CLRClassParser extends ClassParser {

    protected JavaTypeFactory make;

    protected final CLRPackageParser importer;

    public CLRClassParser(Global global, CLRPackageParser  importer) {
	super(global);
	this.importer = importer;
    }

    protected void doComplete(Symbol clazz) {
	try { doComplete0(clazz); }
	catch (AssertionError e) {
	    System.err.println("While processing " + Debug.show(clazz));
	    throw e;
	}
    }

    protected void doComplete0(Symbol clazz) {
	clazz.owner().initialize(); //???

	if (make == null)
	    make = new JavaTypeCreator(global.definitions);

	Type type = Type.GetType(clazz.fullNameString());
// 	System.out.println("completing class " + clazz.fullNameString()
// 			   + " from type " + type);
	clazz.flags = translateAttributes(type);
	Type[] ifaces = type.GetInterfaces(); // FIXME: ensure only the declared interfaces are taken into account
	scalac.symtab.Type[] baseTypes = new scalac.symtab.Type[ifaces.length+1];
	baseTypes[0] = type.BaseType == null ? global.definitions.ANYREF_TYPE()
	    : getCLRType(type.BaseType);
	for (int i = 0; i < ifaces.length; i++)
	    baseTypes[i + 1] = getCLRType(ifaces[i]);
	Scope members = new Scope();
	Scope statics = new Scope();
	scalac.symtab.Type classType =
	    scalac.symtab.Type.compoundType(baseTypes, members, clazz);
	clazz.setFirstInfo(classType);
	Symbol staticsClass = clazz.module().moduleClass();
	if (staticsClass.isModuleClass()) {
	    scalac.symtab.Type staticsInfo = scalac.symtab.Type.compoundType
		(scalac.symtab.Type.EMPTY_ARRAY, statics, staticsClass);
	    staticsClass.setFirstInfo(staticsInfo);
	    clazz.module().setInfo(scalac.symtab.Type.TypeRef
				   (staticsClass.owner().thisType(),
				    staticsClass, scalac.symtab.Type.EMPTY_ARRAY));
	}
        scalac.symtab.Type ctype = clazz.typeConstructor();
	// read field information
	int bindingFlags = BindingFlags.DeclaredOnly
	    | BindingFlags.Instance | BindingFlags.Static
	    | BindingFlags.Public | BindingFlags.NonPublic;
	FieldInfo[] fields = type.GetFields(bindingFlags);
	for (int i = 0; i < fields.length; i++) {
	    int mods = translateAttributes(fields[i]);
	    Name name = Name.fromString(fields[i].Name);
	    scalac.symtab.Type fieldType = getCLRType(fields[i].FieldType);
	    Symbol owner = fields[i].IsStatic ? staticsClass : clazz;
	    Symbol field = new TermSymbol(Position.NOPOS, name, owner, mods);
	    field.setFirstInfo(fieldType);
	    (fields[i].IsStatic ? statics : members).enterOrOverload(field);
	    importer.map(field, fields[i]);
	}

	PropertyInfo[] props = type.GetProperties(bindingFlags);
	for (int i = 0; i < props.length; i++) {
	    MethodInfo getter = props[i].GetGetMethod(true);
	    MethodInfo setter = props[i].GetSetMethod(true);
	    if (getter == null || getter.GetParameters().length > 0)
		continue;
	    assert props[i].PropertyType == getter.ReturnType;
	    scalac.symtab.Type proptype = getCLSType(props[i].PropertyType);
	    if (proptype == null)
		continue;
// 	    if (type.FullName.equals("System.Collections.ArrayList"))
// 		System.out.println("getter found: " + getter);
	    Name n = Name.fromString(props[i].Name);
	    scalac.symtab.Type mtype =
		scalac.symtab.Type.PolyType(Symbol.EMPTY_ARRAY, proptype);
	    int mods = translateAttributes(getter);
	    Symbol owner = getter.IsStatic ? staticsClass : clazz;
	    Symbol method = new TermSymbol(Position.NOPOS, n, owner, mods);
	    setParamOwners(mtype, method);
	    method.setFirstInfo(mtype);
	    (getter.IsStatic ? statics : members).enterOrOverload(method);
	    importer.map(method, getter);

	    if (setter == null)
		continue;
	    assert getter.IsStatic == setter.IsStatic;
	    assert setter.ReturnType == importer.VOID;
	    mtype = methodType(setter, getCLSType(importer.VOID));
	    if (mtype == null)
		continue;
	    n = n.append(Names._EQ);
	    mods = translateAttributes(setter);
	    method = new TermSymbol(Position.NOPOS, n, owner, mods);
	    setParamOwners(mtype, method);
	    method.setFirstInfo(mtype);
	    (setter.IsStatic ? statics : members).enterOrOverload(method);
	    importer.map(method, setter);
	}

	MethodInfo[] methods = type.GetMethods(bindingFlags);
	for (int i = 0; i < methods.length; i++) {
	    if (importer.getSymbol(methods[i]) != null)
		continue;
	    scalac.symtab.Type rettype = getCLSType(methods[i].ReturnType);
	    if (rettype == null)
		continue;
	    scalac.symtab.Type mtype = methodType(methods[i], rettype);
	    if (mtype == null)
		continue;
	    String name = methods[i].Name;
	    Name n;
	    if (name.equals("GetHashCode")) n = Names.hashCode;
	    else if (name.equals("Equals")) n = Names.equals;
	    else if (name.equals("ToString")) n = Names.toString;
	    else n = Name.fromString(name);
	    int mods = translateAttributes(methods[i]);
	    Symbol owner = methods[i].IsStatic ? staticsClass : clazz;
	    Symbol method = new TermSymbol(Position.NOPOS, n, owner, mods);
	    setParamOwners(mtype, method);
	    method.setFirstInfo(mtype);
	    (methods[i].IsStatic ? statics : members).enterOrOverload(method);
	    importer.map(method, methods[i]);
	}

	ConstructorInfo[] constrs = type.GetConstructors(bindingFlags);
	for (int i = 0; i < constrs.length; i++) {
	    if (constrs[i].IsStatic || constrs[i].IsPrivate
		|| constrs[i].IsAssembly || constrs[i].IsFamilyAndAssembly)
		continue;
	    scalac.symtab.Type mtype = methodType(constrs[i], ctype);
	    if (mtype == null)
		continue;
	    Symbol constr = clazz.primaryConstructor();
	    if (constr.isInitialized()) constr = clazz.addConstructor();
	    int mods = translateAttributes(methods[i]);
	    TermSymbol.newConstructor(clazz, mods).copyTo(constr);
	    setParamOwners(mtype, constr);
	    constr.setFirstInfo(mtype);
//  	    System.out.println(clazz.allConstructors() + ": "
//  			       + clazz.allConstructors().info());
	    importer.map(constr, constrs[i]);
	}

	Symbol constr = clazz.primaryConstructor();
	if (!constr.isInitialized()) {
	    constr.setFirstInfo(scalac.symtab.Type.MethodType
				(Symbol.EMPTY_ARRAY, ctype));
	    if ((clazz.flags & Modifiers.INTERFACE) == 0)
		constr.flags |= Modifiers.PRIVATE;
	}

	// import nested types
	Type[] nestedTypes = type.GetNestedTypes();
	for (int i = 0; i < nestedTypes.length; i++) {
	    Name n = Name.fromString(nestedTypes[i].Name).toTypeName();
	    ClassSymbol nclazz = new ClassSymbol(n, clazz, this);
	    nclazz.allConstructors().setInfo(staticsParser(nclazz));
	    nclazz.module().setInfo(staticsParser(nclazz));
	    members.enter(nclazz);
	    Scope.Entry e = members.lookupEntry(clazz.module().name);
	    if (e != Scope.Entry.NONE)
		members.unlink(e);
	    members.enter(nclazz.module());
	}
    }

    /** Return a method type for */
    protected scalac.symtab.Type methodType(MethodBase method,
					    scalac.symtab.Type rettype) {
	ParameterInfo[] params = method.GetParameters();
	scalac.symtab.Type[] ptypes = new scalac.symtab.Type[params.length];
	for (int j = 0; j < params.length; j++) {
	    ptypes[j] = getCLSType(params[j].ParameterType);
	    if (ptypes[j] == null)
		return null;
	}
	return make.methodType(ptypes, rettype, scalac.symtab.Type.EMPTY_ARRAY);
    }

    protected void setParamOwners(scalac.symtab.Type type, Symbol owner) {
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

    protected scalac.symtab.Type getClassType(Type type) {
	assert type != null;
	scalac.symtab.Type res = make.classType(Name.fromString(type.FullName));
	if (res == scalac.symtab.Type.ErrorType)
	    global.error("unknown class reference " + type.FullName);
	return res;
    }

    protected scalac.symtab.Type getCLSType(Type type) {
	if (type == importer.BYTE || type == importer.USHORT
	    || type == importer.UINT || type == importer.ULONG
	    || type.IsPointer
	    || (type.IsArray && getCLSType(type.GetElementType()) == null))
	    return null;
	return getCLRType(type);
    }

    protected scalac.symtab.Type getCLRType(Type type) {
	if (type == importer.OBJECT)
	    return global.definitions.JAVA_OBJECT_TYPE();
	if (type == importer.STRING)
	    return global.definitions.JAVA_STRING_TYPE();
	if (type == importer.VOID)
	    return make.voidType();
	if (type == importer.BOOLEAN)
	    return make.booleanType();
	if (type == importer.CHAR)
	    return make.charType();
	if (type == importer.BYTE || type == importer.UBYTE)
	    return make.byteType();
	if (type == importer.SHORT || type == importer.USHORT)
	    return make.shortType();
	if (type == importer.INT || type == importer.UINT)
	    return make.intType();
	if (type == importer.LONG || type == importer.ULONG)
	    return make.longType();
	if (type == importer.FLOAT)
	    return make.floatType();
	if (type == importer.DOUBLE)
	    return make.doubleType();
	if (type.IsArray)
	    return make.arrayType(getCLRType(type.GetElementType()));
	return getClassType(type);
    }

    protected static int translateAttributes(Type type) {
	int mods = 0;
	if (type.IsNotPublic || type.IsNestedPrivate
	    || type.IsNestedAssembly || type.IsNestedFamANDAssem)
	    mods |= Modifiers.PRIVATE;
	else if (type.IsNestedFamily || type.IsNestedFamORAssem)
	    mods |= Modifiers.PROTECTED;
	if (type.IsAbstract)
	    mods |= Modifiers.ABSTRACT;
	if (type.IsSealed)
	    mods |= Modifiers.FINAL;
	if (type.IsInterface)
	    mods |= Modifiers.INTERFACE | Modifiers.TRAIT | Modifiers.ABSTRACT;

	return mods | Modifiers.JAVA;
    }

    protected static int translateAttributes(FieldInfo field) {
	int mods = 0;
	if (field.IsPrivate || field.IsAssembly || field.IsFamilyAndAssembly)
	    mods |= Modifiers.PRIVATE;
	else if (field.IsFamily || field.IsFamilyOrAssembly)
	    mods |= Modifiers.PROTECTED;
	if (field.IsInitOnly)
	    mods |= Modifiers.FINAL;
	else
	    mods |= Modifiers.MUTABLE;

	return mods | Modifiers.JAVA;
    }

    protected static int translateAttributes(MethodInfo method) {
	int mods = 0;
	if (method.IsPrivate || method.IsAssembly || method.IsFamilyAndAssembly)
	    mods |= Modifiers.PRIVATE;
	else if (method.IsFamily || method.IsFamilyOrAssembly)
	    mods |= Modifiers.PROTECTED;
	if (method.IsAbstract)
	    mods |= Modifiers.DEFERRED;

	return mods | Modifiers.JAVA;
    }
}
