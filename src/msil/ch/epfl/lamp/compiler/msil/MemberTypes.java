/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */


package ch.epfl.lamp.compiler.msil;

/**
 * Marks each type of member that is defined as a derived class of MemberInfo.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public final class MemberTypes {

    //##########################################################################

    /** Specifies that the member is a constructor,
     *  representing a ConstructorInfo member. */
    public static final int Constructor = 0x01;


    /** Specifies that the member is an event,
     *  representing an EventInfo member. */
    public static final int Event = 0x02;


    /** Specifies that the member is a field,
     *	representing a FieldInfo member. */
    public static final int Field = 0x04;


    /** Specifies that the member is a method,
     *  representing a MethodInfo member. */
    public static final int Method = 0x08;


    /** Specifies that the member is a property,
     *  representing a PropertyInfo member.
     */
    public static final int Property = 0x10;

    /** Specifies that the member is a type,
     *  representing a TypeInfo member. */
    public static final int TypeInfo = 0x20;


    /** Specifies that the member is a custom member type. */
    public static final int Custom = 0x40;


    /** Specifies that the member is a nested type,
     *  extending MemberInfo. */
    public static final int NestedType = 0x80;


    /** Specifies all member types. */
    public static final int All =
	Constructor | Event | Field | Method | Property | TypeInfo | NestedType;


    public static String toString(int memberType) {
	if ((memberType & Constructor) != 0) return "Constructor";
	if ((memberType & Event) != 0) return "Event";
	if ((memberType & Field) != 0) return "Field";
	if ((memberType & Method) != 0) return "Method";
	if ((memberType & Property) != 0) return "Property";
	if ((memberType & TypeInfo) != 0) return "TypeInfo";
	if ((memberType & Custom) != 0) return "Custom";
	if ((memberType & NestedType) != 0) return "NestedType";
	return "Unknown MemberType: " + memberType;
    }

    //##########################################################################

    // makes the class uninstantiable
    private MemberTypes() {}

    //##########################################################################

}  // class MemberTypes
