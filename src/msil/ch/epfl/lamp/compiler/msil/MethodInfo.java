/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */


package ch.epfl.lamp.compiler.msil;

import java.util.Iterator;

/**
 * Discovers the attributes of a method and provides access to method metadata.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public class MethodInfo extends MethodBase {

    public boolean HasPtrParamOrRetType() {
        if(ReturnType.IsByRef() && !(ReturnType.GetElementType().IsValueType())) {
            /* A method returning ByRef won't pass peverify, so I guess this is dead code. */
            return true;
        }
        if(ReturnType.IsPointer()) {
            return true;
        }
        return super.HasPtrParamOrRetType();
    }

    //##########################################################################
    // public members

    public final int MemberType() { return MemberTypes.Method; }

    public final boolean IsConstructor() { return false; }

    /** The return type of this method.
     */
    public final Type ReturnType;

    //##########################################################################
    // protected members

    protected static final MethodInfo[] EMPTY_ARRAY = new MethodInfo[0];

    /**
     * Constructor Initializes a new instance of the MethodInfo class.
     */
    protected MethodInfo(String name, Type declType,
			 int attrs, Type returnType, Type[] paramTypes )
    {
	super(name, declType, attrs, paramTypes);
	ReturnType = returnType;
    }

    protected MethodInfo(String name, Type declType,
			 int attrs, Type returnType, ParameterInfo[] params )
    {
	super(name, declType, attrs, params);
	ReturnType = returnType;
    }

    public String toString() {
 	return MethodAttributes.toString(Attributes) + " " + ReturnType +
	    " " + DeclaringType + "::" + Name + params2String();
    }

    //##########################################################################

}  // class MethodInfo
