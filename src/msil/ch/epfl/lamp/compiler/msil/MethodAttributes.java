/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */


package ch.epfl.lamp.compiler.msil;

/** Specifies flags for method attributes.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public final class MethodAttributes {

    //##########################################################################
    // Method access attributes

    /** Bitmask used to retrieve accessibility information. */
    public static final short MemberAccessMask = 0x0007;

    ///** Member not referenceable*/
    //public static final short CompilerConstrolled = 0x0000;

    /** Indicates that the member cannot be referenced. */
    public static final short PrivateScope = 0x0000;

    /** Method is accessible only by the current class. */
    public static final short Private = 0x0001;

    /** Method is accessible to members of this type
     *  and its derived types that are in this assembly only. */
    public static final short FamANDAssem = 0x0002;

    /** Method is accessible to any class of this assembly. */
    public static final short Assembly = 0x0003;

    /** Method is accessible only to members of this class
     *  and its derived classes. */
    public static final short Family = 0x0004;

    /** Method is accessible to derived classes anywhere,
     *  as well as to any class in the assembly. */
    public static final short FamORAssem = 0x0005;

    /** Method is accessible to any object for which this object is in scope. */
    public static final short Public = 0x0006;


    //##########################################################################
    // Flags

    /** Method is defined on the type; otherwise, it is defined per instance. */
    public static final short Static = 0x0010;

    /** Method cannot be overridden. */
    public static final short Final = 0x0020;

    /** Method is virtual. */
    public static final short Virtual = 0x0040;

    /** Method hides by name and signature; otherwise, by name only. */
    public static final short HideBySig  = 0x0080;


    //##########################################################################
    // vtable attributes

    /** Bitmask used to retrieve vtable attributes. */
    public static final short VtableLayoutMask = 0x0100;

    /** Method reuses existing slot in the vtable. */
    public static final short ReuseSlot = 0x0000;


    /** Method always gets a new slot in the vtable. */
    public static final short NewSlot = 0x0100;


    //##########################################################################
    // Flags

    /** Method does not provide implementation. */
    public static final short Abstract = 0x0400;

    /** Method is special. */
    public static final short SpecialName = 0x0800;


    //##########################################################################
    // Interop attributes

    /** Method implementation is forwarded through PInvoke. */
    public static final short PInvokeImpl = 0x2000;

    /** Reserved: shall be zero for conforming implementations.
     *  Managed method is exported by thunk to unmanaged code. */
    public static final short UnmanagedExport = 0x0008;


    //##########################################################################
    // Additional flags

    /** CLI provides special behavior, depending on the name of the method. */
    public static final short RTSpecialName = 0x1000;

    /** Method has security associated with it.
     * Reserved flag for runtime use only.
     */
    public static final short HasSecurity = 0x00000040;

    /**
     * Indicates that the method calls another method containing security code.
     * Reserved flag for runtime use only.
     */
    public static final short RequireSecObject = 0x00004000;

    /** Indicates a reserved flag for runtime use only. */
    public static final short ReservedMask = 0x0000;


    //##########################################################################

    public static String toString(short attrs) {
	StringBuffer str = new StringBuffer(accessFlagsToString(attrs));
	if ((attrs & Static) != 0) str.append(" static");
	if ((attrs & Final) != 0) str.append(" final");
	if ((attrs & Virtual) != 0) str.append(" virtual");
	if ((attrs & Abstract) != 0) str.append(" abstract");
	if ((attrs & HideBySig) != 0) str.append(" hidebysig");
	if ((attrs & NewSlot) != 0) str.append(" newslot");
	if ((attrs & SpecialName) != 0) str.append(" specialname");
	if ((attrs & PInvokeImpl) != 0) str.append(" pinvokeimpl(?!?)");
	if ((attrs & RTSpecialName) != 0) str.append(" rtspecialname");
	return str.toString();

    }

    public static String accessFlagsToString(short attrs) {
	switch (attrs & MemberAccessMask) {
	case PrivateScope: return "compilercontrolled";
	case Private:      return "private";
	case FamANDAssem:  return "famandassem";
	case Assembly:     return "assembly";
	case Family:       return "family";
	case FamORAssem:   return "famorassem";
	case Public:       return "public";
	default: return "xxx";
	}
    }

    //##########################################################################

    // makes the class uninstantiable
    private MethodAttributes() {}

    //##########################################################################

}  // class Method Attributes
