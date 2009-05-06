/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

// $Id$

package ch.epfl.lamp.compiler.msil;


/**
 * Discovers the attributes of an event
 * and provides access to event metadata.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public class EventInfo extends MemberInfo {

    //##########################################################################

    public final int MemberType() { return MemberTypes.Event; }

    /** Attributes associated with the event. */
    public final short Attributes;

    /** The Type object for the underlying event-handler delegate
     *  associated with this event.
     */
    public final Type EventHandlerType;

    public MethodInfo GetAddMethod() { return addMethod; }

    public MethodInfo GetRemoveMethod() { return removeMethod; }

    public String toString() {
        return "" + EventHandlerType + " " + Name;
    }

    //##########################################################################

    protected static final EventInfo[] EMPTY_ARRAY = new EventInfo[0];

    protected MethodInfo addMethod;

    protected MethodInfo removeMethod;

    protected EventInfo(String name, Type declType, short attr,
                        Type handlerType, MethodInfo add, MethodInfo remove)
    {
        super(name, declType);
        Attributes = attr;
        EventHandlerType = handlerType;
        this.addMethod = add;
        this.removeMethod = remove;
    }

    //##########################################################################

} // class EventInfo
