/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2003, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab.classfile;

import scalac.Global;
import scalac.symtab.Symbol;
import scalac.symtab.SymbolLoader;
import scalac.symtab.TermSymbol;
import scalac.symtab.ClassSymbol;
import scalac.symtab.AliasTypeSymbol;
import scalac.symtab.Scope;
import scalac.symtab.Modifiers;
import scalac.symtab.Type.*;
import scalac.util.Name;
import scalac.util.Names;
import scalac.util.Debug;

import ch.epfl.lamp.util.Position;
import ch.epfl.lamp.compiler.msil.*;

public class CLRClassParser extends SymbolLoader {

    protected JavaTypeFactory make;

    protected final CLRPackageParser importer;

//     private static final int bindingFlags = BindingFlags.DeclaredOnly
// 	| BindingFlags.Instance | BindingFlags.Static
// 	| BindingFlags.Public | BindingFlags.NonPublic;

    public CLRClassParser(Global global, CLRPackageParser  importer) {
	super(global);
	this.importer = importer;
    }

    protected String doComplete(Symbol clazz) {
	try { return doComplete0(clazz); }
	catch (Throwable e) {
	    System.err.println("\nWhile processing " + Debug.show(clazz));
	    e.printStackTrace();
	    System.exit(1);
            return null; // !!!
	}
    }

    protected String doComplete0(Symbol clazz) {
	clazz.owner().initialize(); //???

	if (make == null)
	    make = new JavaTypeCreator(global.definitions);

	Type type = (Type)importer.getMember(clazz);
	if (type == null)
	    type = Type.GetType(global.primitives.getCLRClassName(clazz));
	clazz.flags = translateAttributes(type);
	Type[] ifaces = type.getInterfaces();
	scalac.symtab.Type[] baseTypes = new scalac.symtab.Type[ifaces.length+1];
	baseTypes[0] = type.BaseType == null ? global.definitions.ANYREF_TYPE()
	    : getCLRType(type.BaseType);
	for (int i = 0; i < ifaces.length; i++)
	    baseTypes[i + 1] = getCLRType(ifaces[i]);
	Scope members = new Scope();
	Scope statics = new Scope();
	scalac.symtab.Type classType =
	    scalac.symtab.Type.compoundType(baseTypes, members, clazz);
	clazz.setInfo(classType);
	Symbol staticsClass = clazz.module().moduleClass();
	if (staticsClass.isModuleClass()) {
	    scalac.symtab.Type staticsInfo = scalac.symtab.Type.compoundType
		(scalac.symtab.Type.EMPTY_ARRAY, statics, staticsClass);
	    staticsClass.setInfo(staticsInfo);
	    clazz.module().setInfo(scalac.symtab.Type.typeRef
				   (staticsClass.owner().thisType(),
				    staticsClass, scalac.symtab.Type.EMPTY_ARRAY));
	}
        scalac.symtab.Type ctype = make.classType(clazz);

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
	    ClassSymbol nclazz = new ClassSymbol(classname, clazz.owner(), this);
	    importer.map(nclazz, ntype);
	    // create an alias in the module of the outer class
	    AliasTypeSymbol alias =
		new AliasTypeSymbol(Position.NOPOS, aliasname, clazz.module(),
				    translateAttributes(ntype));
	    nclazz.allConstructors().setInfo(this);
	    nclazz.module().setInfo(this);
	    //
	    alias.setInfo(scalac.symtab.Type
			  .typeRef(clazz.owner().thisType(),
				   nclazz, scalac.symtab.Type.EMPTY_ARRAY));
	    alias.allConstructors()
		.setInfo(MethodType(Symbol.EMPTY_ARRAY, nclazz.info()));
// 	    statics.enter(nclazz);
// 	    Scope.Entry e = statics.lookupEntry(clazz.module().name);
// 	    if (e != Scope.Entry.NONE)
// 		statics.unlink(e);
// 	    statics.enter(nclazz.module());

 	    Scope.Entry e = statics.lookupEntry(alias.name); // Why is this ???
 	    if (e != Scope.Entry.NONE)
 		statics.unlink(e);
	    statics.enter(alias);
	}

	// read field information
	FieldInfo[] fields = type.getFields();
	for (int i = 0; i < fields.length; i++) {
	    if (fields[i].IsPrivate() || fields[i].IsAssembly()
		|| fields[i].IsFamilyAndAssembly())
		continue;
	    int mods = translateAttributes(fields[i]);
	    Name name = Name.fromString(fields[i].Name);
	    scalac.symtab.Type fieldType = getCLRType(fields[i].FieldType);
	    if (fields[i].IsLiteral())
		fieldType = make.constantType(fieldType, fields[i].getValue());
	    Symbol owner = fields[i].IsStatic() ? staticsClass : clazz;
	    Symbol field = new TermSymbol(Position.NOPOS, name, owner, mods);
	    field.setInfo(fieldType);
	    (fields[i].IsStatic() ? statics : members).enterOrOverload(field);
	    importer.map(field, fields[i]);
	}

	//PropertyInfo[] props = type.GetProperties(bindingFlags);
	PropertyInfo[] props = type.getProperties();
	for (int i = 0; i < props.length; i++) {
	    MethodInfo getter = props[i].GetGetMethod(true);
	    MethodInfo setter = props[i].GetSetMethod(true);
	    if (getter == null || getter.GetParameters().length > 0)
		continue;
	    assert props[i].PropertyType == getter.ReturnType;
	    scalac.symtab.Type proptype = getCLSType(props[i].PropertyType);
	    if (proptype == null)
		continue;
	    Name n = Name.fromString(props[i].Name);
	    scalac.symtab.Type mtype =
		scalac.symtab.Type.PolyType(Symbol.EMPTY_ARRAY, proptype);
	    int mods = translateAttributes(getter);
	    Symbol owner = getter.IsStatic() ? staticsClass : clazz;
	    Symbol method = new TermSymbol(Position.NOPOS, n, owner, mods);
	    setParamOwners(mtype, method);
	    method.setInfo(mtype);
	    (getter.IsStatic() ? statics : members).enterOrOverload(method);
	    importer.map(method, getter);

	    if (setter == null)
		continue;
	    assert getter.IsStatic() == setter.IsStatic();
	    assert setter.ReturnType == importer.VOID;
	    mtype = methodType(setter, getCLSType(importer.VOID));
	    if (mtype == null)
		continue;
	    n = n.append(Names._EQ);
	    mods = translateAttributes(setter);
	    method = new TermSymbol(Position.NOPOS, n, owner, mods);
	    setParamOwners(mtype, method);
	    method.setInfo(mtype);
	    (setter.IsStatic() ? statics : members).enterOrOverload(method);
	    importer.map(method, setter);
	}

	//MethodInfo[] methods = type.GetMethods(bindingFlags);
	MethodInfo[] methods = type.getMethods();
	for (int i = 0; i < methods.length; i++) {
	    if ((importer.getSymbol(methods[i]) != null)
		|| methods[i].IsPrivate()
		|| methods[i].IsAssembly()
		|| methods[i].IsFamilyAndAssembly())
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
	    Symbol owner = methods[i].IsStatic() ? staticsClass : clazz;
	    Symbol method = new TermSymbol(Position.NOPOS, n, owner, mods);
	    setParamOwners(mtype, method);
	    method.setInfo(mtype);
	    (methods[i].IsStatic() ? statics : members).enterOrOverload(method);
	    importer.map(method, methods[i]);
	}

	//ConstructorInfo[] constrs = type.GetConstructors(bindingFlags);
	ConstructorInfo[] constrs = type.getConstructors();
	for (int i = 0; i < constrs.length; i++) {
	    if (constrs[i].IsStatic() || constrs[i].IsPrivate()
		|| constrs[i].IsAssembly() || constrs[i].IsFamilyAndAssembly())
		continue;
	    scalac.symtab.Type mtype = methodType(constrs[i], ctype);
	    if (mtype == null)
		continue;
	    Symbol constr = clazz.primaryConstructor();
	    if (constr.isInitialized()) constr = clazz.addConstructor();
	    int mods = translateAttributes(constrs[i]);
	    TermSymbol.newConstructor(clazz, mods).copyTo(constr);
	    setParamOwners(mtype, constr);
	    constr.setInfo(mtype);
//  	    System.out.println(clazz.allConstructors() + ": "
//  			       + clazz.allConstructors().info());
	    importer.map(constr, constrs[i]);
	}

	Symbol constr = clazz.primaryConstructor();
	if (!constr.isInitialized()) {
	    constr.setInfo(scalac.symtab.Type.MethodType
				(Symbol.EMPTY_ARRAY, ctype));
	    if ((clazz.flags & Modifiers.INTERFACE) == 0)
		constr.flags |= Modifiers.PRIVATE;
	}

	return "assembly [" + type.Assembly.GetName().Name + "]" + type;
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
	scalac.symtab.Type res =
	    make.classType(Name.fromString(type.FullName.replace('+', '.')));
	if (res == scalac.symtab.Type.ErrorType)
	    global.error("unknown class reference " + type.FullName);
	return res;
    }

    protected scalac.symtab.Type getCLSType(Type type) {
	if (type == importer.BYTE || type == importer.USHORT
	    || type == importer.UINT || type == importer.ULONG
	    || type.IsPointer()
	    || (type.IsArray() && getCLSType(type.GetElementType()) == null))
	    return null;
	//Symbol s = importer.getSymbol(type);
	//scalac.symtab.Type t = s != null ? make.classType(s) : getCLRType(type);
	return getCLRType(type);
    }

    protected scalac.symtab.Type getCLRType(Type type) {
	if (type == importer.OBJECT)
	    return make.objectType();
	if (type == importer.STRING)
	    return make.stringType();
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
	if (type.IsArray())
	    return make.arrayType(getCLRType(type.GetElementType()));
	Symbol s = importer.getSymbol(type);
	return s != null ? make.classType(s) : getClassType(type);
    }

    protected static int translateAttributes(Type type) {
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

    protected static int translateAttributes(FieldInfo field) {
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

    protected static int translateAttributes(MethodBase method) {
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
