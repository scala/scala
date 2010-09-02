/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */


package ch.epfl.lamp.compiler.msil;

import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Arrays;

/**
 * Represents type declarations: class types, interface types, array types,
 * value types, and enumeration types.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public abstract class Type extends MemberInfo {

    //##########################################################################
    // public static members

    /** Empty array of type Type. */
    public static final Type[] EmptyTypes = new Type[0];

    /** Separates names in the namespace of the Type. */
    public static final char Delimiter = '.';

    //##########################################################################
    // public properties

    /** The fully qualified name of the Type. */
    public final String FullName;

    /** The namespace of the Type. */
    public final String Namespace;

    /** The type from which the current Type directly inherits. */
    public final Type BaseType() {
        initBaseType();
        return baseType;
    }
    protected Type baseType;

    /** The attributes associated with the Type. */
    public final int Attributes;

    /** The sssembly that the type is declared in. */
    public final Assembly Assembly() { return Module.Assembly; }

    /** The module (the EXE/DLL) in which the current Type is defined. */
    public final Module Module;

    public final int MemberType() {
        return DeclaringType == null
            ? MemberTypes.TypeInfo : MemberTypes.NestedType;
    }

    //##########################################################################
    // internal members

    // Fields declared by this class
    protected FieldInfo[] fields;

    // Methods declared by this class
    protected MethodInfo[] methods;

    // Constructors of this class
    protected ConstructorInfo[] constructors;

    // Properties of the class
    protected PropertyInfo[] properties;

    // Events of the class
    protected EventInfo[] events;

    // Interfaces implemented by this class
    protected Type[] interfaces;

    // Nested types declared by this class
    protected Type[] nestedTypes;

    // holds the element type of array, pointer and byref types
    private final Type elemType;

    // the underlying type of an enumeration. null if the type is not enum.
    protected Type underlyingType;

    private int auxAttr;

    //##########################################################################
    // Map with all the types known so far and operations on it

    private static final Map types = new HashMap();

    protected static Type getType(String name) {
	return (Type) types.get(name);
    }

    protected static Type addType(Type t) {
	Type oldType = (Type) types.put(t.FullName, t);
// 	if (oldType != null)
// 	    throw new RuntimeException("The type: [" + t.Assembly + "]" + t
// 				       + " replaces the type: [" +
// 				       oldType.Assembly + "]" + oldType);
 	return t;
    }

    //##########################################################################

    /** The main constructor. */
    protected Type(Module module,
		   int attr,
		   String fullName,
		   Type baseType,
		   Type[] interfaces,
		   Type declType,
		   int auxAttr,
		   Type elemType)
    {
	super(fullName.lastIndexOf(Delimiter) < 0 ? fullName :
	      fullName.substring(fullName.lastIndexOf(Delimiter) + 1,
				 fullName.length()),
	      declType);

	Module = module;
	Attributes = attr;
	this.baseType = baseType;
	if (DeclaringType == null) {
	    FullName = fullName;
	    int i = FullName.lastIndexOf(Delimiter);
	    Namespace = (i < 0) ? "" : FullName.substring(0,i);
	} else {
	    FullName = declType.FullName + "+" + fullName;
	    Namespace = DeclaringType.Namespace;
	}

	this.interfaces = interfaces;
	this.elemType = elemType;
	this.auxAttr = auxAttr;
    }

    public final boolean IsAbstract() {
	return (Attributes & TypeAttributes.Abstract) != 0;

    }
    public final boolean IsPublic() {
	return (Attributes & TypeAttributes.VisibilityMask)
	    == TypeAttributes.Public;
    }

    public final boolean IsNotPublic() {
	return (Attributes & TypeAttributes.VisibilityMask)
	    == TypeAttributes.NotPublic;
    }

    public final boolean IsNestedPublic() {
	return (Attributes & TypeAttributes.VisibilityMask)
	    == TypeAttributes.NestedPublic;
    }

    public final boolean IsNestedPrivate() {
	return (Attributes & TypeAttributes.VisibilityMask)
	    == TypeAttributes.NestedPrivate;
    }

    public final boolean IsNestedFamily() {
	return (Attributes & TypeAttributes.VisibilityMask)
	    == TypeAttributes.NestedFamily;
    }

    public final boolean IsNestedAssembly() {
	return (Attributes & TypeAttributes.VisibilityMask)
	    == TypeAttributes.NestedAssembly;
    }

    public final boolean IsNestedFamORAssem() {
	return (Attributes & TypeAttributes.VisibilityMask)
	    == TypeAttributes.NestedFamORAssem;
    }

    public final boolean IsNestedFamANDAssem() {
	return (Attributes & TypeAttributes.VisibilityMask)
	    == TypeAttributes.NestedFamANDAssem;
    }

    public final boolean IsSealed() {
	return (Attributes & TypeAttributes.Sealed) != 0;
    }

    public final boolean IsSpecialName() {
	return (Attributes & TypeAttributes.SpecialName) != 0;
    }

    public final boolean IsClass() {
	return  (Attributes & TypeAttributes.ClassSemanticsMask)
	    == TypeAttributes.Class;
    }

    public final boolean IsInterface(){
	return  (Attributes & TypeAttributes.ClassSemanticsMask)
	    == TypeAttributes.Interface;
    }

    public final boolean IsAutoLayout() {
        return  (Attributes & TypeAttributes.LayoutMask)
	    == TypeAttributes.AutoLayout;
    }
    public final boolean IsExplictitLayout() {
        return  (Attributes & TypeAttributes.LayoutMask)
	    == TypeAttributes.ExplicitLayout;
    }
    public final boolean IsLayoutSequential() {
        return  (Attributes & TypeAttributes.LayoutMask)
	    == TypeAttributes.SequentialLayout;
    }

    public final boolean IsImport() {
	return (Attributes & TypeAttributes.Import) != 0;
    }
    public final boolean IsSerializable() {
	return (Attributes & TypeAttributes.Serializable) != 0;
    }

    public final boolean IsAnsiClass() {
	return (Attributes & TypeAttributes.StringFormatMask)
	    == TypeAttributes.AnsiClass;
    }

    public final boolean IsUnicodeClass() {
	return (Attributes & TypeAttributes.StringFormatMask)
	    == TypeAttributes.UnicodeClass;
    }
    public final boolean IsAutoClass() {
	return (Attributes & TypeAttributes.StringFormatMask)
	    == TypeAttributes.AutoClass;
    }

    public final boolean IsArray() {
	return (auxAttr & AuxAttr.Array) != 0;
    }
    public final boolean IsByRef() {
	return (auxAttr & AuxAttr.ByRef) != 0;
    }
    public final boolean IsPointer() {
	return (auxAttr & AuxAttr.Pointer) != 0;
    }
    public final boolean IsPrimitive() {
	return (auxAttr & AuxAttr.Primitive) != 0;
    }
    public final boolean IsValueType() {
	return BaseType() == VALUE_TYPE() || IsEnum();
    }
    public final boolean IsEnum() {
	return BaseType() == ENUM();
    }

    public final boolean HasElementType() {
	return IsArray() || IsPointer() || IsByRef();
    }

    //public final boolean IsCOMObject;
    //public final boolean IsContextful;
    //public final boolean IsMarshalByRef;

    protected Type(Module module,
		   int attr,
		   String fullName,
		   Type baseType,
		   Type[] interfaces,
		   Type declType,
		   int auxAttr)
    {
	this(module, attr, fullName, baseType, interfaces,
	     declType, auxAttr, null);
    }

    //##########################################################################

    static final class PrimitiveType extends Type {
	public PrimitiveType(Module module,
			     int attributes,
			     String fullName,
			     Type baseType,
			     Type[] interfaces,
			     Type declType,
			     int auxAttr,
			     Type elemType)
	{
	    super(module, attributes, fullName,
		  baseType, interfaces, declType, auxAttr, elemType);
	}
    }

    protected static final class AuxAttr {
	public static final int None      = 0x0000;
	public static final int Array     = 0x0001;
	public static final int ByRef     = 0x0002;
	public static final int Pointer   = 0x0008;
	public static final int Primitive = 0x0010;
    }

    /***/
    public static Type mkArray(Type elemType, int rank) {
	StringBuffer arrSig = new StringBuffer("[");
 	for (int i = 0; i < rank; i++) {
 	    if (i > 0) arrSig.append(',');
 	}
	arrSig.append(']');
	Type array = getType(elemType.FullName + arrSig);
	if (array != null)
	    return array;
	array = new PrimitiveType(elemType.Module,
				  TypeAttributes.Public
				  | TypeAttributes.Sealed
				  | TypeAttributes.Serializable,
				  elemType.FullName + arrSig,
				  ARRAY(), EmptyTypes, null,
				  AuxAttr.Array, elemType);
	return addType(array);
    }

    /***/
    public static Type mkPtr(Type elemType) {
	String name = elemType.FullName + "*";
	Type type = getType(name);
	if (type != null) return type;
	type = new PrimitiveType(elemType.Module,
				 TypeAttributes.NotPublic,
				 name, null, EmptyTypes, null,
				 AuxAttr.Pointer, elemType);
	return addType(type);
    }

    //##########################################################################
    // public methods

    /**
     * Return the type with the specified signature parameters.
     * For example, the fully qualified name for a class might look like this:
     * TopNamespace.SubNameSpace.ContainingClass+NestedClass,MyAssembly
     */
    public static Type GetType(String fullName) {
	Type type = getType(fullName);
	if (type != null) return type;

	// check if it's an array type; TODO: make array type handling more robust
	int i = fullName.lastIndexOf('[');
	int j = fullName.lastIndexOf(']');
	if (i >= 0)
	    if (j > i && j == (fullName.length() - 1)) {
		String elementTypeName = fullName.substring(0, i);
		Type elementType = GetType(elementTypeName);
		if (elementType == null)
		    throw new RuntimeException
			("Unknown element type '" + elementTypeName +
			 "' for the array type: " + fullName);
		int rank = j - i;
		for (int k = i + 1; k < j; k++) {
		    if (fullName.charAt(k) != ',')
			throw new RuntimeException
			    ("Malformed type name: " + fullName);
		}
		return mkArray(elementType, rank);
	    } else
		throw new RuntimeException("Malformed type name: " + fullName);

	// check if it's a pointer type
	if (fullName.charAt(fullName.length() - 1) == '*')
	    return addType
		(mkPtr(GetType(fullName.substring(0, fullName.length()-1))));

	// check if it's a nested class
	i = fullName.lastIndexOf('+');
	if (i > 0) {
	    if (i == 0 || i == (fullName.length() - 1))
		throw new RuntimeException("malformedTypeName");
	    Type enclosing = GetType(fullName.substring(0, i));
	    return enclosing == null ? null
		: enclosing.GetNestedType(fullName.substring(i + 1));
	}

	//System.out.println("Looking for type: " + fullName + " (" + fullName.length() + ")");
	// try in the assemblies
	Iterator assems = ch.epfl.lamp.compiler.msil.Assembly.
	    assemblies.values().iterator();
	while (type == null && assems.hasNext()) {
	    Assembly assem = ((Assembly) assems.next());
	    type = assem.GetType(fullName);
	    //System.out.println("\tin assemby " + assem + " -> " + type);
	}

	Type type2 = getType(fullName);
	if (type == type2) return type;
	return type == null ? null : addType(type);
    }

    /**
     * @return the type of the object encompassed or referenced to
     * by the current array, pointer or reference type.
     */
    public Type GetElementType() {
	return elemType;
    }

    /**
     * @return the type underlying an enumeration type.
     */
    public Type getUnderlyingType() {
	if (!IsEnum()) return null;
	// this would force the loading of the underlying type from the
	// the type of the value__ field of the enumeration
	initFields();
	return underlyingType;
    }

    //##########################################################################
    // GetField/s/

    /** Searches for the field with the specified name. */
    public FieldInfo GetField(String name) {
	initFields();
	for (int i = 0; i < fields.length; i++)
	    if (fields[i].Name.equals(name) && !fields[i].IsPrivate())
		return fields[i];
	return null;
    }

    /**
     */
    public FieldInfo GetField(String name, int bindingFlags) {
	FieldInfo[] fields = this.GetFields(bindingFlags);
	for (int i = 0; i < fields.length; i++)
	    if (name.equals(fields[i].Name))
		return fields[i];
	return null;
    }

    /** Gets the fields of the current Type. */
    public FieldInfo[] GetFields() {
	return GetFields(BindingFlags.Instance | BindingFlags.Public);
    }

    /**
     */
    public FieldInfo[] GetFields(int bindingFlags) {
 	initFields();
	final FieldInfo[] fields =
	    getAllFields((bindingFlags & BindingFlags.DeclaredOnly) != 0);
	final boolean getInstance = (bindingFlags & BindingFlags.Instance) != 0;
	final boolean getStatic = (bindingFlags & BindingFlags.Static) != 0;
	final boolean getPublic = (bindingFlags & BindingFlags.Public) != 0;
	final boolean getNonPublic =
	    (bindingFlags & BindingFlags.NonPublic) != 0;

	int cnt = 0;
	for (int i = 0; i < fields.length; i++) {
	    FieldInfo field = fields[i];
	    boolean accessible = (getPublic && field.IsPublic())
		|| (getNonPublic && !field.IsPublic());
	    if (accessible
		// strip off the private fields up the hierarchy
		&& ((field.DeclaringType == this)
		    || ((field.DeclaringType != this) && !field.IsPrivate()))
		&& ((getInstance && !field.IsStatic())
		    || ((getStatic && field.IsStatic()) &&
			(field.DeclaringType == this
			 || (bindingFlags & BindingFlags.FlattenHierarchy) != 0))
		    )
		)
		fields[cnt++] = field;
	}
	FieldInfo [] resFields = new FieldInfo[cnt];
	System.arraycopy(fields, 0, resFields, 0, cnt);
	return resFields;
    }

    protected FieldInfo[] getAllFields(boolean declaredOnly) {
	initFields();
	FieldInfo [] inherited = BaseType() == null || declaredOnly
	    ? FieldInfo.EMPTY_ARRAY
	    : BaseType().getAllFields(declaredOnly);
	FieldInfo[] allFields =
	    new FieldInfo[inherited.length + this.fields.length];
	System.arraycopy(inherited, 0, allFields, 0, inherited.length);
	System.arraycopy(this.fields, 0,
			 allFields, inherited.length, this.fields.length);
	return allFields;
    }

    //##########################################################################
    // GetConstructor/s/

    /** Searches for a public instance constructor whose parameters
     *  match the types in the specified array. */
    public ConstructorInfo GetConstructor(Type[] paramTypes) {
	initMethods();
	for (int i = 0; i < constructors.length; i++) {
	    if (equalParameters(constructors[i].GetParameters(), paramTypes))
		return constructors[i];
	}
	return null;
    }

    /** Returns all public instance constructors defined for the current Type.*/
    public ConstructorInfo[] GetConstructors() {
	return GetConstructors(BindingFlags.Instance | BindingFlags.Public);
    }

    /***/
    public ConstructorInfo[] GetConstructors(int bindingFlags) {
	initMethods();
	final boolean getInstance = (bindingFlags & BindingFlags.Instance) != 0;
	final boolean getStatic = (bindingFlags & BindingFlags.Static) != 0;
	final boolean getPublic = (bindingFlags & BindingFlags.Public) != 0;
	final boolean getNonPublic =
	    (bindingFlags & BindingFlags.NonPublic) != 0;

	ConstructorInfo[] constrs =
	    new ConstructorInfo[this.constructors.length];
	int cnt = 0;
	for (int i = 0; i < this.constructors.length; i++) {
	    ConstructorInfo constr = this.constructors[i];
	    boolean accessible = (getPublic && constr.IsPublic())
		|| (getNonPublic && !constr.IsPublic());
	    if (accessible
		&& ((getInstance && !constr.IsStatic())
		    || (getStatic && constr.IsStatic())))
		constrs[cnt++] = constr;
	}
	ConstructorInfo [] resConstrs = new ConstructorInfo[cnt];
	System.arraycopy(constrs, 0, resConstrs, 0, cnt);
	return resConstrs;
    }

    //##########################################################################
    // GetMethod/s/

    /** Searches for the specified public method whose parameters
     *  match the specified argument types. */
    public MethodInfo GetMethod(String name, Type[] paramTypes) {
        return GetMethod(name, paramTypes, null);
    }

    public MethodInfo GetMethod(String name, Type[] paramTypes, Type retType) {
	initMethods();
	MethodInfo method = findMethod(methods, name, paramTypes, retType);
	if (method != null)
	    return method;
	if (BaseType() != null) {
	    method = BaseType().GetMethod(name, paramTypes, retType);
	    if (method != null)
		return method;
	}
// 	StringBuffer str = new StringBuffer(name);
// 	str.append('(');
// 	for (int i = 0; i < paramTypes.length; i++) {
// 	    if (i > 0) str.append(", ");
// 	    str.append(paramTypes[i]);
// 	}
// 	str.append(')');
// 	System.out.println("Cannot find method " + str + ":");
// 	System.out.println("Methods of class " + this);
// 	for (int i = 0; i < methods.length; i++)
// 	    System.out.println("\t" + methods[i]);
	return null;
    }

    /**
     */
    protected static MethodInfo findMethod(MethodInfo[] methods,
					   String name,
					   Type[] paramTypes,
                                           Type retType)
    {
	for (int i = 0; i < methods.length; i++)
	    if (name.equals(methods[i].Name)
		&& equalParameters(methods[i].GetParameters(), paramTypes)
                && (retType == null || methods[i].ReturnType == retType))
		return methods[i];
	return null;
    }

    /**
     */
    protected static boolean equalParameters(ParameterInfo[] params,
					    Type[] paramTypes)
    {
	if (params.length != paramTypes.length)
	    return false;
	for (int i = 0; i < params.length; i++) {
// 	    System.out.println(params[i].ParameterType + " == " + paramTypes[i]
// 			       + " = " + (params[i].ParameterType == paramTypes[i]));
	    if (params[i].ParameterType != paramTypes[i])
		return false;
	}
	return true;
    }

    /**
     */
    public MethodInfo GetMethod(String name, Type[] paramTypes, int bindingFlags) {
	MethodInfo[] methods = GetMethods(bindingFlags);
	MethodInfo method =  findMethod(methods, name, paramTypes, null);
	if (method == null) {
	    StringBuffer str = new StringBuffer(name);
	    str.append('(');
	    for (int i = 0; i < paramTypes.length; i++) {
		if (i > 0) str.append(", ");
		str.append(paramTypes[i]);
	    }
	    str.append(')');
	    System.out.println("Cannot find method " + str + ":");
	    System.out.println("Methods of class " + this);
	    for (int i = 0; i < methods.length; i++)
		System.out.println("\t" + methods[i]);
	}
	return method;
    }

    /** Returns all public methods of the current Type. */
    public MethodInfo[] GetMethods() {
	return GetMethods(BindingFlags.Instance | BindingFlags.Public);
    }

    /**
     */
    public MethodInfo[] GetMethods(int bindingFlags) {
	initMethods();
	final MethodInfo[] methods =
	    getAllMethods((bindingFlags & BindingFlags.DeclaredOnly) != 0);
	//System.out.println("" + this + ".GetMethods(int) -> " + methods.length);
	final boolean getInstance = (bindingFlags & BindingFlags.Instance) != 0;
	final boolean getStatic = (bindingFlags & BindingFlags.Static) != 0;
	final boolean getPublic = (bindingFlags & BindingFlags.Public) != 0;
	final boolean getNonPublic =
	    (bindingFlags & BindingFlags.NonPublic) != 0;

	int cnt = 0;
	for (int i = 0; i < methods.length; i++) {
	    MethodInfo method = methods[i];
	    boolean accessible = (getPublic && method.IsPublic())
		|| (getNonPublic && !method.IsPublic());
	    if (accessible
		// strip off the private methods up the hierarchy
		&& ((method.DeclaringType == this)
		    || ((method.DeclaringType != this) && !method.IsPrivate()))
		&& ((getInstance && !method.IsStatic())
		    || ((getStatic && method.IsStatic()) &&
			(method.DeclaringType == this
			 || (bindingFlags & BindingFlags.FlattenHierarchy) != 0))
		    )
		)
		methods[cnt++] = method;
	}
	MethodInfo [] resMethods = new MethodInfo[cnt];
	System.arraycopy(methods, 0, resMethods, 0, cnt);
	return resMethods;
    }

    protected MethodInfo[] getAllMethods(boolean declaredOnly) {
	initMethods();
	MethodInfo[] inherited = BaseType() == null || declaredOnly
	    ? MethodInfo.EMPTY_ARRAY
	    : BaseType().getAllMethods(declaredOnly);
	MethodInfo[] allMethods =
	    new MethodInfo[inherited.length + this.methods.length];
	System.arraycopy(inherited, 0, allMethods, 0, inherited.length);
	System.arraycopy(this.methods, 0,
			 allMethods, inherited.length, this.methods.length);
	return allMethods;
    }

    //##########################################################################
    // GetProperty/ies/

    /** Returns all public properties of the current Type.
     */
    public PropertyInfo[] GetProperties() {
	initProperties();
	return (PropertyInfo[]) properties.clone();
    }

    /** Returns the properties of the current class
     *  that satisfy the binding constrints.
     */
    public PropertyInfo[] GetProperties(int bindingFlags) {
	initProperties();
	return (PropertyInfo[]) properties.clone();
    }

    /** Returns the public property with the given name.
     */
    public PropertyInfo GetProperty(String name) {
	initProperties();
	for (int i = 0; i < properties.length; i++)
	    if (name.equals(properties[i].Name))
		return properties[i];
	return null;
    }

    /** Returns the property with the given name
     *  that satisfies the binding constraints.
     */
    public PropertyInfo GetProperty(String name, int bindingFlags) {
	throw new RuntimeException("Method not implemented yet");
    }

    //##########################################################################
    // GetEvent(s)

    public EventInfo[] GetEvents() {
        initEvents();
        return (EventInfo[]) events.clone();
    }

    //##########################################################################
    // GetNestedType/s/

    /** Searches for nested type with the specified name. */
    public Type GetNestedType(String name) {
	initNestedTypes();
	for (int i = 0; i < nestedTypes.length; i++)
	    if (nestedTypes[i].Name.equals(name))
		return nestedTypes[i];
	return null;
    }

    /** Returns all types nested within the current Type. */
    public Type[] GetNestedTypes() {
	initNestedTypes();
	return (Type[]) nestedTypes.clone();
    }

    //##########################################################################
    // GetInterface/s/

    /** Searches for an Interface with the given name implemented by this type
     */
    public Type GetInterface(String name) {
	return GetInterface(name, false);
    }

    /** Searches for the specified interface,
     * specifying whether to do a case-sensitive search.
     * @param name - the name of the interface to get
     * @param ignoreCase <b>true</b> to perform a case-insensitive search for name
     *                   <b>false</b> to perform a case-sensitive search for name
     * @return A Type object representing the interface with the specified name,
     *         implemented or inherited by the current Type, if found;
     *         otherwise, a null reference
     */
    public Type GetInterface(String name, boolean ignoreCase) {
	initInterfaces();
	for (int i = 0; i < interfaces.length; i++) {
	    Type iface = interfaces[i];
	    if (ignoreCase) {
		if (name.equalsIgnoreCase(iface.Name)) return iface;
		if (name.equalsIgnoreCase(iface.FullName)) return iface;
	    } else {
		if (name.equals(iface.Name)) return iface;
		if (name.equals(iface.FullName)) return iface;
	    }
	}
	return BaseType() == null ? null
	    : BaseType().GetInterface(name, ignoreCase);
    }

    /** Returns the interfaces implemented or inherited by the current Type. */
    public Type[] GetInterfaces() {
	initInterfaces();
	if (BaseType() == null) return interfaces;

	Type[] ifaces = interfaces;
	int count = 0;
	for (int i = 0; i < interfaces.length; i++) {
	    if (BaseType().GetInterface(interfaces[i].FullName) == null)
		ifaces[count++] = ifaces[i];
	}
	Type[] baseTypeIfaces = BaseType().GetInterfaces();

	Type[] res = new Type[baseTypeIfaces.length + count];
	System.arraycopy(baseTypeIfaces, 0, res, 0, baseTypeIfaces.length);
	System.arraycopy(ifaces, 0, res, baseTypeIfaces.length, count);

	return res;
    }


    public boolean isSubtypeOf(Type that) {
	if (this == that || BaseType() == that || that == OBJECT()) return true;
	initInterfaces();
	for (int i = 0; i < interfaces.length; i++)
	    if (interfaces[i].isSubtypeOf(that))
		return true;
	boolean res = BaseType() == null ? false : BaseType().isSubtypeOf(that);
// 	if (!res) {
// 	    System.out.println(dumpType(this) + " not a subtype of " +
// 			       dumpType(that));
// 	}
	return res;
    }

    private static String formatType(Type t) {
	if (t == null) return "<null>";
	String cname = t.getClass().getName();
	int k = cname.lastIndexOf(".");
	if (k >= 0)
	    cname = cname.substring(k + 1);
	return  "[" + t.Assembly().GetName() + "]" + t +
	    "(" + cname + "#" + Integer.toHexString(t.hashCode()) + ")";
    }
    private static String dumpType(Type t) {
	StringBuffer str = new StringBuffer();
	str.append(formatType(t) + " : ");
	str.append(formatType(t.BaseType()));
	Type[] ifaces = t.GetInterfaces();
	for (int i = 0; i < ifaces.length; i++)
	    str.append(", " + formatType(ifaces[i]));
	return str.toString();
    }

    //##########################################################################
    // GetMember/s/

    protected MemberInfo[] members;

    public MemberInfo[] GetMember(String name) {
	aggregateMembers();
	List l = new ArrayList();
	for (int i = 0; i < members.length; i++) {
	    if (name.equals(members[i].Name))
		l.add(members[i]);
	}
	return (MemberInfo[])l.toArray(MemberInfo.EMPTY_ARRAY);
    }

    protected void aggregateMembers() {
	if (members != null)
	    return;
	initFields();
	initMethods();
	initProperties();
	initNestedTypes();
	// the List returned by Arrays.asList doesn't support the addAll method
	// so we have to wrap it in ArrayList
	List l = new ArrayList(Arrays.asList(fields));
	l.addAll(Arrays.asList(constructors));
	l.addAll(Arrays.asList(methods));
	l.addAll(Arrays.asList(properties));
	l.addAll(Arrays.asList(nestedTypes));
	members = (MemberInfo[]) l.toArray(MemberInfo.EMPTY_ARRAY);
    }

    //##########################################################################
    // non-standard methods that return only members declared in this type

    /**
     * Return only the fields declared in this type.
     */
    public FieldInfo[] getFields() {
	initFields();
	FieldInfo[] fields = new FieldInfo[this.fields.length];
	System.arraycopy(this.fields, 0, fields, 0, fields.length);
	return fields;
    }

    /**
     * Return only the conrtuctors declared in this type.
     */
    public ConstructorInfo[] getConstructors() {
	initMethods();
	ConstructorInfo[] ctors = new ConstructorInfo[constructors.length];
	System.arraycopy(constructors, 0, ctors, 0, ctors.length);
	return ctors;
    }

    /**
     * Return only the methods declared in this type.
     */
    public MethodInfo[] getMethods() {
	initMethods();
	MethodInfo[] methods = new MethodInfo[this.methods.length];
	System.arraycopy(this.methods, 0, methods, 0, methods.length);
	return methods;
    }

    /**
     * Return only the properties declared in this type.
     */
    public PropertyInfo[] getProperties() {
	initProperties();
	PropertyInfo[] props = new PropertyInfo[properties.length];
	System.arraycopy(properties, 0, props, 0, props.length);
	return props;
    }

    /**
     * Return only the interfaces directly implemented by this type.
     */
    public Type[] getInterfaces() {
	initInterfaces();
	Type[] ifaces = new Type[interfaces.length];
	System.arraycopy(interfaces, 0, ifaces, 0, ifaces.length);
	return ifaces;
    }

    /**
     * Return the types declared in this type.
     */
    public Type[] getNestedTypes() {
	initNestedTypes();
	Type[] nested = new Type[nestedTypes.length];
	System.arraycopy(nestedTypes, 0, nested, 0, nested.length);
	return nested;
    }

    //##########################################################################

    public String toString() {
	return FullName;
    }

    //##########################################################################
    // lazy type construction members

    private boolean initBaseType = true;
    protected final void initBaseType() {
        if (initBaseType) {
            loadBaseType();
            initBaseType = false;
        }
    }
    protected void loadBaseType() {}

    private boolean initInterfaces = true;
    protected void initInterfaces() {
	if (initInterfaces) {
	    loadInterfaces();
	    initInterfaces = false;
	}
	assert interfaces != null : "In type " + this;
    }
    protected void loadInterfaces() {}

    private boolean initNestedTypes = true;
    protected void initNestedTypes() {
	if (initNestedTypes) {
	    loadNestedTypes();
	    initNestedTypes = false;
	}
	assert nestedTypes != null : "In type " + this;
    }
    protected void loadNestedTypes() {}

    private boolean initFields = true;
    protected void initFields() {
	if (initFields) {
	    loadFields();
	    initFields = false;
	}
	assert fields != null : "In type " + this;
    }
    protected void loadFields() {}

    private boolean initMethods = true;
    protected void initMethods() {
	if (initMethods) {
	    loadMethods();
	    initMethods = false;
	}
	assert constructors != null : "In type " + this;
	assert methods != null : "In type " + this;
    }
    protected void loadMethods() {}

    private boolean initProperties = true;
    protected void initProperties() {
	if (initProperties) {
	    initMethods();
	    loadProperties();
	    initProperties = false;
	}
	assert properties != null : "In type " + this;
    }
    protected void loadProperties() {}

    private boolean initEvents = true;
    protected void initEvents() {
	if (initEvents) {
	    initMethods();
	    loadEvents();
	    initEvents = false;
	}
	assert events != null : "In type " + this;
    }
    protected void loadEvents() {}

    //##########################################################################

    //##########################################################################
    // static members

    private static Assembly MSCORLIB;
    private static Module   MSCORLIB_DLL;

    public static Type OBJECT() { return __OBJECT; }
    public static Type STRING() { return __STRING; }
    public static Type ARRAY() { return __ARRAY; }
    public static Type VOID() { return __VOID; }
    public static Type ENUM() { return __ENUM; }
    public static Type VALUE_TYPE() { return __VALUE_TYPE; }

    private static Type __OBJECT;
    private static Type __STRING;
    private static Type __ARRAY;
    private static Type __VOID;
    private static Type __ENUM;
    private static Type __VALUE_TYPE;

    public static void initMSCORLIB(Assembly mscorlib) {
        if (MSCORLIB != null)
            throw new RuntimeException("mscorlib already initialized");
	MSCORLIB = mscorlib;
	MSCORLIB_DLL = MSCORLIB.GetModules()[0];

	__OBJECT = mscorlib.GetType("System.Object");
	__STRING = mscorlib.GetType("System.String");
	__ARRAY  = mscorlib.GetType("System.Array");
	__VOID   = mscorlib.GetType("System.Void");
	__ENUM   = mscorlib.GetType("System.Enum");
	__VALUE_TYPE   = mscorlib.GetType("System.ValueType");
    }

    //##########################################################################

}  // class Type
