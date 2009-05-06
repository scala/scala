/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

// $Id$

package ch.epfl.lamp.compiler.msil;


/**
 * Calling conventions
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public abstract class CallingConventions {

    //########################################################################

    /**
     * Specifies the default calling convention as determined by the
     * common language runtime.
     */
    public static final short Standard = (short) 0x0001;

    /**
     * Specifies the calling convention for methods with variable arguments.
     */
    public static final short VarArgs = (short) 0x0002;

    /**
     * Specifies that either the Standard or the VarArgs calling
     * convention may be used.
     */
    public static final short Any = Standard | VarArgs;

    /**
     * Specifies an instance or virtual method (not a static method).
     * At run-time, the called method is passed a pointer to the target
     * object as its first argument (the this pointer). The signature
     * stored in metadata does not include the type of this first argument,
     * because the method is known and its owner class can be discovered
     * from metadata.
     */
    public static final short HasThis = (short) 0x0020;

    /**
     * Specifies that the signature is a function-pointer signature,
     * representing a call to an instance or virtual method (not a static
     * method). If ExplicitThis is set, HasThis must also be set. The first
     * argument passed to the called method is still a this pointer, but the
     * type of the first argument is now unknown. Therefore, a token that
     * describes the type (or class) of the this pointer is explicitly stored
     * into its metadata signature.
     */
    public static final short ExplicitThis = (short) 0x0040;

    //########################################################################

    private CallingConventions() {}

    public static String toString(int callConv) {
	StringBuffer s = new StringBuffer();

	if ((callConv & HasThis) != 0) {
	    s.append("instance");
	    if ((callConv & ExplicitThis) != 0)
		s.append(" explicit");
	}

	return s.toString();
    }

    //##########################################################################

}  // class CallingConventions
