/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */


package ch.epfl.lamp.compiler.msil;

/**
 * Defines the attributes that may be associated with a parameter.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public final class ParameterAttributes {

    // just to make the class uninstantiable
    private ParameterAttributes() {}

    //##########################################################################

    /** Specifies that there is no parameter attribute. */
    public static final short None = 0x0000;

    /** Specifies that the parameter is an input parameter. */
    public static final short In = 0x0001;

    /** Specifies that the parameter is an output parameter. */
    public static final short Out = 0x0002;

    /** Specifies that the parameter is a locale identifier. */
    public static final short Lcid = 0x0004;

    /** Specifies that the parameter is a return value. */
    public static final short Retval = 0x0008;

    /** Specifies that the parameter is optional.
     *  Attention: In the specification the value is 0x0004 but
     *  in mscorlib.dll that it Lcid and Optional is 0x0010
     */
    public static final short Optional = 0x0010;

    /** Specifies that the parameter has a default value. */
    public static final short HasDefault = 0x1000;

    /** Specifies that the parameter has field marshaling information. */
    public static final short HasFieldMarshal = 0x2000;

    /** Reserved. */
    public static final short Reserved3 = 0x4000;

    /** Reserved. */
    public static final short Reserved4 = (short)0x8000;

    /** Specifies that the parameter is reserved. */
    public static final short ReservedMask = (short)0xf000;

    /** Reserved: shall be zero in all conforming implementations. */
    public static final short Unused = (short) 0xcfe0;

    public static final String toString(int attrs) {
	StringBuffer s = new StringBuffer();
	if ((attrs & In) != 0) s.append("in ");
	if ((attrs & Out) != 0) s.append("out ");
	if ((attrs & Optional) != 0) s.append("opt ");
	if ((attrs & HasDefault) != 0) s.append("default(???) ");
	if ((attrs & HasFieldMarshal) != 0) s.append("marshal(???) ");
	return s.toString();
    }

    //##########################################################################

}  // class ParameterAttributes
