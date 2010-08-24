/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */


package ch.epfl.lamp.compiler.msil;

/**
 * Discovers the attributes of a class constructor and provides
 * access to constructor metadata.
 * ConstructorInfo is used to discover the attributes of a constructor
 * as well as to invoke a constructor. Objects are created by invoking
 * either the GetConstructors or GetConstructor method of a Type object.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public class ConstructorInfo extends MethodBase {
    //##########################################################################

    public final int MemberType() { return MemberTypes.Constructor; }

    public final boolean IsConstructor() { return true; }

    protected static final String CTOR = ".ctor";
    protected static final String CCTOR = ".cctor";
    protected static final ConstructorInfo[] EMPTY_ARRAY = new ConstructorInfo[0];

    protected static String getName(int attrs) {
	  return (attrs & MethodAttributes.Static) == 0 ? CTOR : CCTOR;
    }

    /** Public constructors */

    public ConstructorInfo(Type declType, int attrs, Type[] paramTypes) {
	  super(getName(attrs), declType, attrs, paramTypes);
	  assert declType != null : "Owner can't be 'null' for a constructor!";
    }

    public ConstructorInfo(Type declType, int attrs, ParameterInfo[] params)
    {
	  super(getName(attrs), declType, attrs, params);
	  assert declType != null : "Owner can't be 'null' for a constructor!";
    }


    public String toString() {
	return MethodAttributes.toString(Attributes) + " " + Type.VOID() +
	    " " + DeclaringType.FullName + "::" + Name + params2String();
    }

    //##########################################################################

} // class ConstructorInfo
