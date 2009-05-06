/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

// $Id$

package ch.epfl.lamp.compiler.msil;

/**
 * Discovers the attributes of a property
 * and provides access to property metadata.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public class PropertyInfo extends MemberInfo {

    //##########################################################################

    public final int MemberType() { return MemberTypes.Property; }

    public final short Attributes;

    public final boolean CanRead;

    public final boolean CanWrite;

    public final Type PropertyType;

    /** Returns an array of the public get and set accessors for this property.
     */
    public MethodInfo[] GetAccessors() {
	return GetAccessors(false);
    }

    /** Returns an array of the public or non-public <b>get</b>
     *  and <b>set</b> accessors for this property.
     */
    public MethodInfo[] GetAccessors(boolean nonPublic) {
	MethodInfo getter = GetGetMethod(nonPublic);
	MethodInfo setter = GetSetMethod(nonPublic);
	if (getter == null)
	    if (setter == null) return MethodInfo.EMPTY_ARRAY;
	    else return new MethodInfo[]{setter};
	else if (setter == null) return new MethodInfo[] {getter};
	else return new MethodInfo[] {getter, setter};
    }

    /** Returns the public <b>get</b> accessor for this property.
     */
    public MethodInfo GetGetMethod() {
	return GetGetMethod(false);
    }

    /** Returns the public or non-public <b>get</b> accessor for this property.
     */
    public MethodInfo GetGetMethod(boolean nonPublic) {
	return nonPublic ? getter
	    : getter == null || getter.IsPublic() ? getter : null;
    }

    /** Returns the public <b>set</b> accessor for this property.
     */
    public MethodInfo GetSetMethod() {
	return GetSetMethod(false);
    }

    /** Returns the public or non-public <b>set</b> accessor for this property.
     */
    public MethodInfo GetSetMethod(boolean nonPublic) {
	return nonPublic ? setter
	    : setter == null || setter.IsPublic() ? setter : null;
    }

    public String toString() {
	MethodInfo m = getter != null ? getter : setter;
	return MethodAttributes.accessFlagsToString
	    ((getter != null ? getter : setter).Attributes)
	    +  " " + PropertyAttributes.toString(Attributes)
	    + DeclaringType + "::" + Name;
    }

    //##########################################################################
    // protected members

    protected static final PropertyInfo[] EMPTY_ARRAY = new PropertyInfo[0];

    protected MethodInfo getter;
    protected MethodInfo setter;

    protected PropertyInfo(String name, Type declType, short attr,
			   Type propType, MethodInfo getter, MethodInfo setter)
    {
	super(name, declType);
	Attributes = attr;
	PropertyType = propType;
	this.getter = getter;
	this.setter = setter;
	CanRead = getter != null;
	CanWrite = setter != null;
    }

    //##########################################################################

} // class PropertyInfo
