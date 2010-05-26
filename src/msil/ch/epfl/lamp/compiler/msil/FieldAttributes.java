/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */


package ch.epfl.lamp.compiler.msil;

/**
 * Specifies flags that describe the attributes of a field.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public final class FieldAttributes {

    //##########################################################################

    /** Specifies the access level of a given field. */
    public static final short FieldAccessMask = 0x0007;

    /** Member not refereneceable. */
    public static final short CompilerControlled = 0x0000;

    /** Field is accessible only by the parent type. */
    public static final short Private = 0x0001;

    /** Field is accessible only by subtypes in this assembly. */
    public static final short FamANDAssem = 0x0002;

    /** Field is accessible throughout the assembly. */
    public static final short Assembly = 0x0003;

    /** Field is accessible only by type and subtypes. */
    public static final short Family = 0x0004;

    /** Field is accessible by subtypes anywhere,
     *  as well as throughout this assembly. */
    public static final short FamORAssem  = 0x0005;

    /** Specifies that the field is accessible by any member
     *  for whom this scope is visible. */
    public static final short Public = 0x0006;

    //##########################################################################
    //

    /** Field represents the defined type, or else it is per-instance. */
    public static final short Static = 0x0010;

    /** Field is initialized only and cannot be written after initialization. */
    public static final short InitOnly = 0x0020;

    /** Value is compile-time constant. */
    public static final short Literal = 0x0040;

    /** Field does not have to be serialized when the type is remoted. */
    public static final short NotSerialized = 0x0080;

    /** Field is special. */
    public static final short SpecialName = 0x0200;

    //##########################################################################
    // Interop attributes

    /** Implementation is forwarded through PInvoke */
    public static final short PinvokeImpl = 0x2000;


    //##########################################################################
    // Additional flags

    /** CLI provides 'special' behavior depending upon the name of the field */
    public static final short RTSpecialName = 0x0400;

    /** Field has marshalling information. */
    public static final short HasFieldMarshal = 0x1000;

    /** Field has a default value. */
    public static final short HasDefault = (short)0x8000;

    /** Field has a Relative Virtual Address (RVA). The RVA is the location
     *  of the method body in the current image, as an address relative
     *  to the start of the image file in which it is located. */
    public static final short HasFieldRVA = 0x0100;

    //##########################################################################
    //

    public static String toString(short attrs) {
	StringBuffer str = new StringBuffer();
	switch (attrs & FieldAccessMask) {
	case CompilerControlled: str.append("compilercontrolled"); break;
	case Private:            str.append("private"); break;
	case FamANDAssem:        str.append("famandassem"); break;
	case Assembly:           str.append("assembly"); break;
	case Family:             str.append("family"); break;
	case FamORAssem:         str.append("famorassem"); break;
	case Public:             str.append("public"); break;
	}
	if ((attrs & Static) != 0) str.append(" static");
	if ((attrs & InitOnly) != 0) str.append(" initonly");
	if ((attrs & Literal) != 0) str.append(" literal");
	if ((attrs & NotSerialized) != 0) str.append(" notserialized");
	if ((attrs & SpecialName) != 0) str.append(" specialname");
	if ((attrs & PinvokeImpl) != 0) str.append("");
	if ((attrs & RTSpecialName) != 0) str.append(" rtspecialname");
	if ((attrs & HasFieldMarshal) != 0) str.append(" marshal(<native type>)");
	//if ((attrs & HasDefault) != 0) str.append(" default(???)");
	return str.toString();
    }

    //##########################################################################

    // makes the class uninstantiable
    private FieldAttributes() {}

    //##########################################################################

}  // class FieldAttributes
