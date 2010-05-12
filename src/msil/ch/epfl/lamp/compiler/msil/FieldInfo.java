/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */


package ch.epfl.lamp.compiler.msil;

/**
 * Discovers the attributes of a field and provides access to field metadata.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public class FieldInfo extends MemberInfo {

    //##########################################################################
    // public interface

    public final int MemberType() { return MemberTypes.Field; }

    /** Attributes associated with this field. */
    public final short Attributes;

    /** Type of the field represented by this FieldInfo object. */
    public final Type FieldType;

    protected final Object value;

    public final boolean IsStatic() {
	return (Attributes & FieldAttributes.Static)   != 0;
    }

    public final boolean IsInitOnly() {
	return (Attributes & FieldAttributes.InitOnly) != 0;
    }

    public final boolean IsLiteral() {
 	return (Attributes & FieldAttributes.Literal) != 0;

    }

    public final boolean IsPublic() {
	return (Attributes & FieldAttributes.FieldAccessMask)
	    == FieldAttributes.Public;
    }

    public final boolean IsPrivate() {
	return (Attributes & FieldAttributes.FieldAccessMask)
	    == FieldAttributes.Private;
    }

    public final boolean IsFamily() {
	return (Attributes & FieldAttributes.FieldAccessMask)
	    == FieldAttributes.Family;
    }

    public final boolean IsAssembly() {
	return (Attributes & FieldAttributes.FieldAccessMask)
	    == FieldAttributes.Assembly;
    }

    public final boolean IsFamilyOrAssembly() {
	return (Attributes & FieldAttributes.FieldAccessMask)
	    == FieldAttributes.FamORAssem;
    }

    public final boolean IsFamilyAndAssembly() {
	return (Attributes & FieldAttributes.FieldAccessMask)
	    == FieldAttributes.FamANDAssem;
    }
    public final boolean IsSpecialName() {
 	return (Attributes & FieldAttributes.SpecialName) != 0;
    }

    public final boolean IsPinvokeImpl() {
 	return (Attributes & FieldAttributes.PinvokeImpl) != 0;
    }

    public final boolean IsNotSerialized() {
 	return (Attributes & FieldAttributes.NotSerialized) != 0;
    }

    public String toString() {
	return FieldAttributes.toString(Attributes) + " " +
	    FieldType + " " + DeclaringType.FullName + "::" +  Name;
    }

    //##########################################################################

    protected static final FieldInfo[] EMPTY_ARRAY = new FieldInfo[0];

    /** Initializes a new instance of the FieldInfo class. */
    protected FieldInfo(String name, Type declType, int attrs, Type fieldType) {
	this(name, declType, attrs, fieldType, null);
    }

    protected FieldInfo(String name, Type declType,
			int attrs, Type fieldType, Object value)
    {
	super(name, declType);
	FieldType = fieldType;
	Attributes = (short) attrs;
	this.value = value;
    }

    /**
     */
    public Object getValue() { return value; }

    //##########################################################################

}  // class FieldInfo
