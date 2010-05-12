/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */


package ch.epfl.lamp.compiler.msil;

/**
 * Discovers the attributes of a parameter and provides access to
 * parameter metadata.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public class ParameterInfo extends CustomAttributeProvider {

    //##########################################################################

    /** Attributes of the parameter. */
    public final short Attributes;

    /** Name of the parameter. */
    public final String Name;

    /** Type of the parameter. */
    public final Type ParameterType;

    /** Position of the parameter in the parameter list. */
    public final int Position;

    //##########################################################################

    /** Is this an input parameter? */
    public final boolean IsIn() {
        return (Attributes & ParameterAttributes.In) != 0;
    }

    /** Is this an output parameter? */
    public final boolean IsOut() {
        return (Attributes & ParameterAttributes.Out) != 0;
    }

    /** Is this an Lcid? */
    public final boolean IsLcid() {
        return (Attributes & ParameterAttributes.Lcid) != 0;
    }

    /** Is this a return value? */
    public final boolean IsRetval() {
        return (Attributes & ParameterAttributes.Retval) != 0;
    }

    /** Is this an optional parameter? */
    public final boolean IsOptional() {
        return (Attributes & ParameterAttributes.Optional) != 0;
    }

    //##########################################################################
    // members not part of the public Reflection.ParameterInfo interface

    /** Initializes a new instance of the ParameterInfo class. */
    protected ParameterInfo(String name, Type type, int attr, int pos) {
	Name = name;
	ParameterType = type;
	Attributes = (short)attr;
	Position = pos;
    }

    public String toString() {
        return ParameterAttributes.toString(Attributes) + ParameterType + " "
            + Name;
    }

    //##########################################################################

}  // class ParameterInfo
