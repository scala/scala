package scala.runtime;

/**
 * Blocking (i.e. non-atomic) placeholder for Java 1.5's
 * java.util.concurrent.atomic.AtomicReference class.
 */

public class AtomicReference {
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
