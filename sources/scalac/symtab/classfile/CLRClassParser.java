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
import scalac.symtab.SymbolOrigin;
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

    private static final CLRTypes clrTypes = CLRTypes.instance();

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

    private final Scope tvars = new Scope();

    protected String doComplete(Symbol root) {
	clazz = root;
	clazz.owner().initialize(); //???

	if (make == null)
	    make = new JavaTypeCreator(global.definitions);

	clazz.flags = translateAttributes(type);
	Type[] ifaces = type.getInterfaces();
	scalac.symtab.Type[] baseTypes = new scalac.symtab.Type[ifaces.length+1];
	baseTypes[0] = type.BaseType() != null ? getCLRType(type.BaseType())
            :  (type.IsInterface() ? make.objectType() : make.anyType());
	for (int i = 0; i < ifaces.length; i++)
	    baseTypes[i + 1] = getCLRType(ifaces[i]);
	members = new Scope();
	statics = new Scope();
        scalac.symtab.Type clazzInfo =
            scalac.symtab.Type.compoundType(baseTypes, members, clazz);
	clazz.setInfo(clazzInfo);
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
	    Name classname = Name.fromString(ntype.Name).toTypeName();
	    CLRClassParser loader = new CLRClassParser(global, ntype);
            SymbolOrigin origin = SymbolOrigin.CLRAssembly(ntype.Assembly());
	    Symbol nclazz = staticsClass.newLoadedClass
                (JAVA, classname, loader, statics, origin);
	    clrTypes.map(nclazz, ntype);
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
            parseMeta(field, fields[i], fieldType);
	    (fields[i].IsStatic() ? statics : members).enterOrOverload(field);
	    clrTypes.map(field, fields[i]);
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
	    int mods = translateAttributes(getter);
            createMethod(n, mods, mtype, getter, getter.IsStatic());
	    assert methodsSet.contains(getter) : "" + getter;
	    methodsSet.remove(getter);

	    if (setter == null || setter.IsPrivate() ||
		setter.IsAssembly() || setter.IsFamilyAndAssembly())
		continue;
	    ParameterInfo[] sparams = setter.GetParameters();
	    assert getter.IsStatic() == setter.IsStatic();
	    assert setter.ReturnType == clrTypes.VOID;
	    assert sparams.length == gparams.length + 1 : "" + getter + "; " + setter;

	    if (gparams.length == 0)
		n = Name.fromString(n.toString() + Names._EQ);
	    else n = Names.update;

	    mods = translateAttributes(setter);
	    mtype = methodType(setter, global.definitions.UNIT_TYPE());
            createMethod(n, mods, mtype, setter, setter.IsStatic());
	    assert methodsSet.contains(setter) : "" + setter;
	    methodsSet.remove(setter);
	}

	for (Iterator i = methodsSet.iterator(); i.hasNext(); ) {
	    MethodInfo method = (MethodInfo)i.next();
	    if ((clrTypes.getSymbol(method) != null) || method.IsPrivate()
		|| method.IsAssembly() || method.IsFamilyAndAssembly())
		continue;
	    createMethod(method);
	}

	// Create symbols related to delegate types
	if(clrTypes.isDelegateType(type)) {
	    createDelegateView();
	    createDelegateChainers();
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
                createMethod(ENUM_CMP_NAMES[i], mods, enumCmpType, null, false);
	    }
	    for (int i = 0; i < ENUM_BIT_LOG_NAMES.length; i++) {
		scalac.symtab.Type enumBitLogType = make.methodType
		    (argTypes, clazzType, scalac.symtab.Type.EMPTY_ARRAY);
                createMethod
                    (ENUM_BIT_LOG_NAMES[i], mods, enumBitLogType, null, false);
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

        parseMeta(clazz, type, clazzInfo);

	return type + " from assembly " + type.Assembly();
    }

    private scalac.symtab.Type parseMeta(Symbol sym,
                                         ICustomAttributeProvider member,
                                         scalac.symtab.Type defaultType)
    {
        if (member !=null && member.IsDefined(clrTypes.PICO_META_ATTR, false)) {
            Object[] attrs =
                member.GetCustomAttributes(clrTypes.PICO_META_ATTR, false);
            assert attrs.length == 1 : "attrs.length = " + attrs.length;
            String meta =
                (String)((Attribute)attrs[0]).getConstructorArguments()[0];
            defaultType = new MetaParser
                (meta, tvars, sym, defaultType, clazz, clazzType, make).parse();
        }
        sym.setInfo(defaultType);
        return defaultType;
    }

    private void createConstructor(ConstructorInfo constr) {
	scalac.symtab.Type mtype = methodType(constr, clazzType);
	if (mtype == null)
	    return;
	Symbol constrSym = clazz.primaryConstructor();
	int mods = translateAttributes(constr);
	if (constrSym.isInitialized()) {
	    constrSym = clazz.newConstructor(Position.NOPOS, mods);
	    clazz.addConstructor(constrSym);
	} else {
	    constrSym.flags = mods;
	}
	setParamOwners(mtype, constrSym);
        parseMeta(constrSym, constr, mtype);
	clrTypes.map(constrSym, constr);
    }

    private void createMethod(MethodInfo method) {
	scalac.symtab.Type mtype =
	    methodType(method, method.ReturnType);
	if (mtype == null)
	    return;
	int mods = translateAttributes(method);
        createMethod(getName(method), mods, mtype, method, method.IsStatic());
    }

    // Create static view methods within the delegate and the function type
    // with the following signatures:
    // def MyDelegate.view(MyDelegate): FunctionX[InvokeArgs..., InvokeRet];
    // def FunctionX.view(FunctionX[InvokeArgs..., InvokeRet]): MyDelegate;
    private void createDelegateView() {
	// Extract the parameter and return types of the Invoke method
	MethodInfo invoke = (MethodInfo)type.GetMember("Invoke")[0];
	scalac.symtab.Type invokeRetType = getCLRType(invoke.ReturnType);
	scalac.symtab.Type invokeParamTypes[] =
	    new scalac.symtab.Type[invoke.GetParameters().length];
	for(int j = 0; j < invoke.GetParameters().length; j++)
	    invokeParamTypes[j] =
		getCLRType(invoke.GetParameters()[j].ParameterType);
	scalac.symtab.Type funType =
	    global.definitions.FUNCTION_TYPE(invokeParamTypes, invokeRetType);

	// FORWARD MAPPING (Delegate => Function)
	scalac.symtab.Type viewParamTypes[] = { getCLRType(type) };
	scalac.symtab.Type viewRetType = funType;
	scalac.symtab.Type viewMethodType = make.methodType(
		viewParamTypes,
		viewRetType,
		scalac.symtab.Type.EMPTY_ARRAY);

        createMethod(Names.view, Modifiers.JAVA, viewMethodType, null, true);

	// REVERSE MAPPING (Function => Delegate)
	viewParamTypes = new scalac.symtab.Type[]{ funType };
	viewRetType = getCLRType(type);
	viewMethodType = make.methodType(
		viewParamTypes,
		viewRetType,
		scalac.symtab.Type.EMPTY_ARRAY);

        createMethod(Names.view, Modifiers.JAVA, viewMethodType, null, true);
    }

    private void createDelegateChainers() {
        int mods = Modifiers.JAVA | Modifiers.FINAL;
        Type[] args = new Type[]{type};

        createMethod(Names.PLUSEQ, mods, args, clrTypes.VOID,
                     clrTypes.DELEGATE_COMBINE, false);
        createMethod(Names.MINUSEQ, mods, args, clrTypes.VOID,
                     clrTypes.DELEGATE_REMOVE, false);
        createMethod
            (Names.PLUS, mods, args, type, clrTypes.DELEGATE_COMBINE, false);
        createMethod
            (Names.MINUS, mods, args, type, clrTypes.DELEGATE_REMOVE, false);
    }

    private Symbol createMethod(Name name, int mods, Type[] args,
                                Type retType, MethodInfo method, boolean statik)
    {
        return createMethod(name, mods, args, getCLSType(retType), method, statik);
    }
    private Symbol createMethod(Name name, int mods, Type[] args,
                                scalac.symtab.Type retType,
                                MethodInfo method,
                                boolean statik)
    {
	scalac.symtab.Type mtype = methodType(args, retType);
 	assert mtype != null : name;
        return createMethod(name, mods, mtype, method, statik);
    }
    private Symbol createMethod(Name name, int mods, scalac.symtab.Type mtype,
                                MethodInfo method, boolean statik)
    {
	Symbol methodSym = (statik ? staticsClass: clazz)
            .newMethod(Position.NOPOS, mods, name);
	setParamOwners(mtype, methodSym);
        parseMeta(methodSym, method, mtype); // sets the type to mtype if no meta
	(statik ? statics : members).enterOrOverload(methodSym);
        if (method != null)
            clrTypes.map(methodSym, method);
        return methodSym;
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
            && params[0].ParameterType == clrTypes.OBJECT)
            return Names.equals;
        // TODO: check if the type implements ICloneable?
        if (name.equals("Clone") && params.length == 0)
            return Names.clone;
	// Pretend that delegates have a 'apply' method instead of the 'Invoke'
	// method. This is harmless because the latter one can't be called
	// directly anyway.
	if (name.equals("Invoke")
            && clrTypes.isDelegateType(method.DeclaringType))
	    return Names.apply;
	return Name.fromString(name);
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
	if (/*type == clrTypes.BYTE ||*/ type == clrTypes.USHORT
	    || type == clrTypes.UINT || type == clrTypes.ULONG
	    || type.IsNotPublic() || type.IsNestedPrivate()
	    || type.IsNestedAssembly() || type.IsNestedFamANDAssem()
	    || type.IsPointer()
	    || (type.IsArray() && getCLSType(type.GetElementType()) == null))
	    return null;
	//Symbol s = clrTypes.getSymbol(type);
	//scalac.symtab.Type t = s != null ? make.classType(s) : getCLRType(type);
	return getCLRType(type);
    }

    private scalac.symtab.Type getCLRType(Type type) {
	if (type == clrTypes.OBJECT)
	    return make.objectType();
	if (type == clrTypes.STRING)
	    return make.stringType();
	if (type == clrTypes.VOID)
	    return make.voidType();
	if (type == clrTypes.BOOLEAN)
	    return make.booleanType();
	if (type == clrTypes.CHAR)
	    return make.charType();
	if (type == clrTypes.BYTE || type == clrTypes.UBYTE)
	    return make.byteType();
	if (type == clrTypes.SHORT || type == clrTypes.USHORT)
	    return make.shortType();
	if (type == clrTypes.INT || type == clrTypes.UINT)
	    return make.intType();
	if (type == clrTypes.LONG || type == clrTypes.ULONG)
	    return make.longType();
	if (type == clrTypes.FLOAT)
	    return make.floatType();
	if (type == clrTypes.DOUBLE)
	    return make.doubleType();
	if (type.IsArray())
	    return make.arrayType(getCLRType(type.GetElementType()));
	Symbol s = clrTypes.getSymbol(type);
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
