/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

// $Id$

package ch.epfl.lamp.compiler.msil;

/**
 * The root class of the Reflection hierarchy.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public abstract class MemberInfo extends CustomAttributeProvider {

    //##########################################################################

    /** The name of this member. */
    public final String Name;

    /**
     * The class that declares this member.
     * Note: if the MemberInfo object is a global member,
     * (that is, it was obtained from Module.GetMethods,
     * which returns global methods on a module), then DeclaringType
     * will be a null reference.
     */
    public final Type DeclaringType;

    /** An enumerated value from the MemberTypes class,
     *  specifying a constructor, event, field, method,
     *  property, type information, all, or custom. */
    public abstract int MemberType();

    //##########################################################################
    // protected members

    protected static final MemberInfo[] EMPTY_ARRAY = new MemberInfo[0];

    protected MemberInfo(String name, Type declType) {
	Name = name;
	DeclaringType = declType;
    }

    //########################################################################

}  // class MemberInfo
