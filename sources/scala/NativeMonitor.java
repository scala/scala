package scala;

public class NativeMonitor  {

    /** @meta method [?A] (def ?A) ?A;
     */
    public java.lang.Object synchronised(scala.Function0 p) {
	java.lang.Object result;
	synchronized(this) {
	    result = p.apply();
	}
	return result;
    }
}
