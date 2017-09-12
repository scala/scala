package test;

public class Subject_1 {
    public volatile int _volatile = 0;
    public transient int _transient = 0;

    public synchronized int _synchonized() {
        return 0;
    }

    public native int _native();
}
