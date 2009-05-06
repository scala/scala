/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

// $Id$

package ch.epfl.lamp.compiler.msil;

/**
 * The common superclass of MemberInfo and ConstructorInfo
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public abstract class MethodBase extends MemberInfo {

    //##########################################################################
    // public interface

    /** The attributes associated with this method/constructor. */
    public final short Attributes;

    /***/
    public final short CallingConvention;

    public abstract boolean IsConstructor();

    public final boolean IsAbstract() {
	return (Attributes & MethodAttributes.Abstract) != 0;
    }

    public final boolean IsFinal() {
	return (Attributes& MethodAttributes.Final)    != 0;
    }

    public final boolean IsVirtual() {
	return (Attributes& MethodAttributes.Virtual)  != 0;
    }

    public final boolean IsStatic() {
	return (Attributes & MethodAttributes.Static)   != 0;
    }

    public final boolean IsHideBySig() {
 	return (Attributes & MethodAttributes.HideBySig) != 0;
    }

    public final boolean IsSpecialName() {
 	return (Attributes & MethodAttributes.SpecialName) != 0;
    }


    public final boolean IsPublic() {
	return (Attributes & MethodAttributes.MemberAccessMask)
	    == MethodAttributes.Public;
    }

    public final boolean IsPrivate() {
	return (Attributes & MethodAttributes.MemberAccessMask)
	    == MethodAttributes.Private;
    }

    public final boolean IsFamily() {
	return (Attributes & MethodAttributes.MemberAccessMask)
	    == MethodAttributes.Family;
    }

    public final boolean IsAssembly() {
	return (Attributes & MethodAttributes.MemberAccessMask)
	    == MethodAttributes.Assembly;
    }

    public final boolean IsFamilyOrAssembly() {
	return (Attributes & MethodAttributes.MemberAccessMask)
	    == MethodAttributes.FamORAssem;
    }

    public final boolean IsFamilyAndAssembly() {
	return (Attributes & MethodAttributes.MemberAccessMask)
	    == MethodAttributes.FamANDAssem;
    }


    /** Returns the parameters of the method/constructor. */
    public ParameterInfo[] GetParameters() {
	return (ParameterInfo[]) params.clone();
    }

    public int GetMethodImplementationFlags() { return implAttributes; }

    //##########################################################################

    /** Method parameters. */
    protected ParameterInfo[] params;

    protected short implAttributes;

    protected MethodBase(String name, Type declType, int attrs, Type[] paramTypes)
    {
	this(name, declType, attrs);
	assert paramTypes != null;
	params = new ParameterInfo[paramTypes.length];
	for (int i = 0; i < params.length; i++)
	    params[i] = new ParameterInfo(null, paramTypes[i], 0, i);
    }

    protected MethodBase(String name, Type declType, int attrs,
			 ParameterInfo[] params)
    {
	this(name, declType, attrs);
	this.params = params;
    }

    /**
     */
    private MethodBase(String name, Type declType, int attrs) {
	super(name, declType);

	Attributes = (short) attrs;

	if (IsConstructor()) {
	    attrs |= MethodAttributes.SpecialName;
	    attrs |= MethodAttributes.RTSpecialName;
	}

	CallingConvention = (short) (CallingConventions.Standard
	    | (IsStatic() ? (short)0 : CallingConventions.HasThis));
   }

    //##########################################################################
    // internal methods

    protected String params2String() {
	StringBuffer s = new StringBuffer("(");
	for (int i = 0; i < params.length; i++) {
	    if (i > 0) s.append(", ");
	    s.append(params[i].ParameterType);
	}
	s.append(")");
	return s.toString();
    }

    //##########################################################################

}  // class MethodBase
