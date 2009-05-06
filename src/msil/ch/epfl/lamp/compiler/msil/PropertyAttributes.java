/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

// $Id$

package ch.epfl.lamp.compiler.msil;

/**
 * Attributes applcicable to properties
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public final class PropertyAttributes {

    // makes the class uninstantiable
    private PropertyAttributes() {}

    //##########################################################################

    /** Specifies that the property is special, with the name describing
     *  how the property is special.
     */
    public static final short SpecialName = 0x0200;

    /** Specifies that the metadata internal APIs check the name encoding.
     */
    public static final short RTSpecialName = 0x0400;

    /** Specifies that the property has a default value.
     */
    public static final short HasDefault = 0x1000;

    //##########################################################################

    public static String toString(short attrs) {
	StringBuffer str = new StringBuffer();
	if ((attrs & SpecialName) != 0) str.append("specialname ");
	if ((attrs & RTSpecialName) != 0) str.append("rtspecialname ");
	return str.toString();
    }

    //##########################################################################

} // class PropertyAttributes
