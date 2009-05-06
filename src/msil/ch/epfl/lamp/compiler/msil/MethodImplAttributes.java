/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

// $Id$

package ch.epfl.lamp.compiler.msil;

/**
 * Method implementation attributes
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public abstract class MethodImplAttributes {

    //##########################################################################

    /**
     * Specifies flags about code type. 3
     */
    public static final short CodeTypeMask = (short) 0x0003;

    /**
     * Specifies that the method implementation is in MSIL. 0
     */
    public static final short IL = (short) 0x0000;

    /**
     * Specifies that the method implementation is native. 1
     */
    public static final short Native = (short) 0x0001;

    /**
     * This member supports the .NET Framework infrastructure and
     * is not intended to be used directly from your code. 2
     */
    public static final short OPTIL = (short) 0x0002;

    /**
     * Specifies that the method implementation is provided by the runtime. 3
     */
    public static final short Runtime = (short) 0x0003;



    /**
     * Specifies whether the code is managed or unmanaged. 4
     */
    public static final short ManagedMask = (short) 0x0004;

    /**
     * Specifies that the method implementation is managed, otherwise unmanaged.
     */
    public static final short Managed = (short) 0x0000;

    /**
     * Specifies that the method implementation is unmanaged, otherwise managed.
     */
    public static final short Unmanaged = (short) 0x0004;



    /**
     * Specifies that the method cannot be inlined. 8
     */
    public static final short NoInlining = (short) 0x0008;

    /**
     * Specifies that the method is not defined. 16
     */
    public static final short ForwardRef = (short) 0x0010;

    /**
     * Specifies that the method is single-threaded through the body.
     * You can also use the C# lock statement or the Visual Basic
     * Lock function for this purpose. 32
     */
    public static final short Synchronized = (short) 0x0020;

    /**
     * Specifies that the method signature is exported exactly as declared. 128
     */
    public static final short PreserveSig = (short) 0x0080;

    /**
     * Specifies an internal call. 4096
     */
    public static final short InternalCall = (short) 0x1000;

    /**
     * Specifies a range check value. 65535
     */
    public static final short MaxMethodImplVal = (short) 0xffff;

    //##########################################################################

    public static String toString(int implAttr) {
	StringBuffer s = new StringBuffer();
	switch (implAttr & CodeTypeMask) {
	case IL: s.append("cil"); break;
	case Native: s.append("native"); break;
	case Runtime: s.append("runtime"); break;
	}
	switch (implAttr & ManagedMask) {
	case Managed: s.append(" managed"); break;
	case Unmanaged: s.append(" unmanaged"); break;
	}
	if ((implAttr & NoInlining) != 0) s.append(" noinlining");
	if ((implAttr & ForwardRef) != 0) s.append(" forwardref");
	if ((implAttr & Synchronized) != 0) s.append(" synchronized");
	if ((implAttr & InternalCall) != 0) s.append(" internalcall");
	return s.toString();
    }

    //##########################################################################

} // class MethodImplAttributes
