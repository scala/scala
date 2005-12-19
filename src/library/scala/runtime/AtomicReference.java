/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.runtime;

/**
 * Blocking (i.e. non-atomic) placeholder for Java 1.5's
 * <code>java.util.concurrent.atomic.AtomicReference</code> class.
 */

public class AtomicReference implements java.io.Serializable {
    private Object value;

    public AtomicReference(Object value) {
        this.value = value;
    }

    public Object get() {
        return value;
    }

    public Object getAndSet(Object update) {
        Object previousValue = value;
        value = update;
        return previousValue;
    }

    public void set(Object update) {
        value = update;
    }

    public synchronized boolean compareAndSet(Object expected, Object update) {
        if (value == expected) {
            value = update;
            return true;
        } else
            return false;
    }

    public boolean weakCompareAndSet(Object expected, Object update) {
        return compareAndSet(expected, update);
    }

    public String toString() {
        return value.toString();
    }
}
