/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util.debug;

/** This class implements a debugger that appends arrays. */
public class ArrayDebugger implements Debugger {

    //########################################################################
    // Public Constants

    /** The unique instance of this class. */
    public static final ArrayDebugger object = new ArrayDebugger();

    //########################################################################
    // Protected Constructors

    /** Initializes this instance. */
    protected ArrayDebugger() {}

    //########################################################################
    // Public Methods

    public boolean canAppend(Object object) {
        return object.getClass().isArray();
    }

    public void append(StringBuffer buffer, Object object) {
        buffer.append('[');
        if (object instanceof Object[]) {
            Object[] array = (Object[])object;
            for (int i = 0; i < array.length; i++) {
                if (i > 0) buffer.append(',');
                Debug.append(buffer, array[i]);
            }
        } else if (object instanceof boolean[]) {
            boolean[] array = (boolean[])object;
            for (int i = 0; i < array.length; i++) {
                if (i > 0) buffer.append(',');
                buffer.append(array[i]);
            }
        } else if (object instanceof byte[]) {
            byte[] array = (byte[])object;
            for (int i = 0; i < array.length; i++) {
                if (i > 0) buffer.append(',');
                buffer.append(array[i]);
            }
        } else if (object instanceof short[]) {
            short[] array = (short[])object;
            for (int i = 0; i < array.length; i++) {
                if (i > 0) buffer.append(',');
                buffer.append(array[i]);
            }
        } else if (object instanceof char[]) {
            char[] array = (char[])object;
            for (int i = 0; i < array.length; i++) {
                if (i > 0) buffer.append(',');
                buffer.append(array[i]);
            }
        } else if (object instanceof int[]) {
            int[] array = (int[])object;
            for (int i = 0; i < array.length; i++) {
                if (i > 0) buffer.append(',');
                buffer.append(array[i]);
            }
        } else if (object instanceof long[]) {
            long[] array = (long[])object;
            for (int i = 0; i < array.length; i++) {
                if (i > 0) buffer.append(',');
                buffer.append(array[i]);
            }
        } else if (object instanceof float[]) {
            float[] array = (float[])object;
            for (int i = 0; i < array.length; i++) {
                if (i > 0) buffer.append(',');
                buffer.append(array[i]);
            }
        } else if (object instanceof double[]) {
            double[] array = (double[])object;
            for (int i = 0; i < array.length; i++) {
                if (i > 0) buffer.append(',');
                buffer.append(array[i]);
            }
        }
        buffer.append(']');
    }

    //########################################################################
}
