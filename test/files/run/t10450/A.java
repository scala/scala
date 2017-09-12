/*
 * filter: unchecked
 */
package a;

class B<T extends B<T>> {
    private int connectTimeout = 10000;
    private int failedAttempts = 3;

    public T setConnectTimeout(int connectTimeout) {
        this.connectTimeout = connectTimeout;
        return (T) this;
    }

    public T setFailedAttempts(int slaveFailedAttempts) {
        this.failedAttempts = slaveFailedAttempts;
        return (T) this;
    }
}
public class A extends B<A> { }
