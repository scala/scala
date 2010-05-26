/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */


package ch.epfl.lamp.compiler.msil;

/**
 * Specifies flags that describe the attributes of a an event.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public final class EventAttributes {

    //##########################################################################

    /** Specifies that the event has no attributes. */
    public static final short None = 0x000;

    /** Specifies a reserved flag for CLR use only. */
    public static final short ReservedMask = 0x0400;

    /** Specifies that the event is special in a way described by the name. */
    public static final short SpecialName = 0x0200;

    /** Specifies the the CLR should check name encoding. */
    public static final short RTSpecialName = 0x0400;

    //##########################################################################

} // class EventAttributes
