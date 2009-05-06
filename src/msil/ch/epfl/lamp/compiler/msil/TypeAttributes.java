/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

// $Id$

package ch.epfl.lamp.compiler.msil;

/**
 * Specifies type attributes.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public final class TypeAttributes {

    //##########################################################################
    // Visibilty attributes

    /** Bitmask used to retrieve visibility information. */
    public static final int VisibilityMask = 0x00000007;

    /** Class has no public scope. */
    public static final int NotPublic = 0x00000000;

    /** Class has public scope. */
    public static final int Public = 0x00000001;

    /** Class is nested with public visibility. */
    public static final int NestedPublic =  0x00000002;

    /** Class is nested with private visibility. */
    public static final int NestedPrivate = 0x00000003;

    /** Class is nested with family visibility, and is thus accessible
     *  only by methods within its own type and any subtypes. */
    public static final int NestedFamily = 0x00000004;

    /** Class is nested with assembly visibility, and is thus accessible
     *  only by methods within its assembly. */
    public static final int NestedAssembly = 0x00000005;

    /** Class is nested with assembly and family visibility, and is thus accessible
     *  only by methods lying in the intersection of its family and assembly. */
    public static final int NestedFamANDAssem = 0x00000006;

    /** Class is nested with family or assembly visibility, and is thus accessible
     *  only by methods lying in the union of its family and assembly. */
    public static final int NestedFamORAssem = 0x00000007;

    //##########################################################################
    // Class layout attributes

    /** Bitmask used to retrieve class layout information. */
    public static final int LayoutMask = 0x00000018;

    /** Class fields are automatically laid out by the CLR. */
    public static final int AutoLayout = 0x00000000;

    /** Class fields are laid out sequentially, in the order that the fields
     *  were emitted to the metadata. */
    public static final int SequentialLayout = 0x00000008;

    /** Class fields are laid out at the specified offsets. */
    public static final int ExplicitLayout = 0x00000010;

    //##########################################################################
    // Class semantics attributes

    /** Bitmask used to retrieve class semantics information. */
    public static final int ClassSemanticsMask = 0x00000020;

    /** Type is a class. */
    public static final int Class = 0x00000000;

    /** Type is an interface. */
    public static final int Interface = 0x00000020;

    //##########################################################################
    // Special semantics in addition to class semantics

    /** Class is abstract. */
    public static final int Abstract = 0x00000080;

    /** Class is cannot be extended. */
    public static final int Sealed = 0x00000100;

    /** Class is special in a way denoted by the name. */
    public static final int SpecialName = 0x00000400;

    //##########################################################################
    // Implementation attributes

    /** Class/interface is imported from another module. */
    public static final int Import = 0x00001000;

    /** Class can be serialized. */
    public static final int Serializable = 0x00002000;

    //##########################################################################
    // String formatting attributes

    /** Bitmask used to retrieve string information for native interop. */
    public static final int StringFormatMask = 0x00030000;

    /** LPTSTR is interpreted as ANSI. */
    public static final int AnsiClass = 0x00000000;

    /** LPTSTR is interpreted as UNICODE. */
    public static final int UnicodeClass = 0x00010000;

    /** LPTSTR is interpreted automatically. */
    public static final int AutoClass = 0x00020000;

    //##########################################################################
    // Class initialization attributes

    /** Initialize the class before first static field access. */
    public static final int BeforeFieldInit = 0x00100000;

    //##########################################################################
    // Additional flags

    /** CLI provides 'special' behavior, depending upon the name of the type. */
    public static final int RTSpecialName = 0x00000800;

    /** Type has security associate with it. */
    public static final int HasSecurity = 0x00040000;

    //##########################################################################

    public static String accessModsToString(int attrs) {
	switch (attrs & VisibilityMask) {
	case NotPublic: return "private";
	case Public: return "public";
	case NestedPublic: return "nested public";
	case NestedPrivate: return "nested private";
	case NestedFamily: return "nested family";
	case NestedAssembly: return "nested assembly";
	case NestedFamANDAssem: return "nested famandassem";
	case NestedFamORAssem: return "nested famorassem";
	default:
	    throw new RuntimeException();
	}
    }

    /** Returns a string representation of the given attributes. */
    public static String toString(int attrs) {
	StringBuffer str = new StringBuffer(accessModsToString(attrs));
	switch (attrs & LayoutMask) {
	case AutoLayout: str.append(" auto"); break;
	case SequentialLayout: str.append(" sequential"); break;
	case ExplicitLayout: str.append(" explicit"); break;
	}
	switch (attrs & StringFormatMask) {
	case AnsiClass: str.append(" ansi"); break;
	case UnicodeClass: str.append(" unicode"); break;
	case AutoClass: str.append(" autochar"); break;
	}
	if ((attrs & Interface) != 0) str.append(" interface");
	if ((attrs & Abstract) != 0) str.append(" abstract");
	if ((attrs & Sealed) != 0) str.append(" sealed");
	if ((attrs & BeforeFieldInit) != 0) str.append(" beforefieldinit");
	if ((attrs & Serializable) != 0) str.append(" serializable");
	if ((attrs & SpecialName) != 0) str.append(" specialname");
	if ((attrs & RTSpecialName) != 0) str.append(" rtspecialname");
	return str.toString();
    }

    /***/
    public static final boolean isNested(int attrs) {
	switch (attrs & VisibilityMask) {
	case NestedPublic:
	case NestedPrivate:
	case NestedFamily:
	case NestedAssembly:
	case NestedFamANDAssem:
	case NestedFamORAssem:
	    return true;
	default: return false;
	}
    }

    //##########################################################################

    // makes the class uninstantiable
    private TypeAttributes() {}

    //##########################################################################

}  // class TypeAttributes
