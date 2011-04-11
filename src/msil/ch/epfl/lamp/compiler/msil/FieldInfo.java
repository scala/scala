/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */


package ch.epfl.lamp.compiler.msil;

import ch.epfl.lamp.compiler.msil.util.PECustomMod;

/**
 * Discovers the attributes of a field and provides access to field metadata.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public class FieldInfo extends MemberInfo implements HasCustomModifiers {

    //##########################################################################
    // public interface

    public final int MemberType() { return MemberTypes.Field; }

    /** Attributes associated with this field. */
    public final short Attributes;

    /** Type of the field represented by this FieldInfo object. */
    public final Type FieldType;

    /** can be null */
    public final CustomModifier[] cmods;

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

    private boolean knownVolatile  = false;
    private boolean cachedVolatile = false;
    public final boolean IsVolatile() {
        if(knownVolatile) return cachedVolatile;
        knownVolatile  = true;
        if(cmods == null) {
            cachedVolatile = false;
            return cachedVolatile;
        }
        for (int idx = 0; idx < cmods.length; idx++) {
            if(cmods[idx].marker == CustomModifier.VolatileMarker()) {
                cachedVolatile = true;
                return cachedVolatile;
            }
        }
        cachedVolatile = false;
        return cachedVolatile;
    }

    public final Type[] GetOptionalCustomModifiers () {
        return CustomModifier.helperCustomMods(false, cmods);
    }

    public final Type[] GetRequiredCustomModifiers() {
        return CustomModifier.helperCustomMods(true, cmods);
    }

    public String toString() {
	return FieldAttributes.toString(Attributes) + " " +
	    FieldType + " " + DeclaringType.FullName + "::" +  Name;
    }

    //##########################################################################

    protected static final FieldInfo[] EMPTY_ARRAY = new FieldInfo[0];

    /** Initializes a new instance of the FieldInfo class. */
    protected FieldInfo(String name, Type declType,
			int attrs, PECustomMod fieldTypeWithMods, Object value)
    {
        super(name, declType);
        FieldType = fieldTypeWithMods.marked;
        cmods = fieldTypeWithMods.cmods;
        Attributes = (short) attrs;
        this.value = value;
    }

    /**
     */
    public Object getValue() { return value; }

    //##########################################################################

}  // class FieldInfo
