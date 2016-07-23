/**
 * A package header
 */
package test.scaladoc;

/**
 * Testing java comments. The presence of a :marker:
 * tag is verified by tests.
 */
public class JavaComments {

    /** A field */
    public final int x;
    /** A field */
    protected int y;
    /** A field */
    private int z;

    /**
     * Inner class
     */
    public class Inner {
	/** Inner method */
	public void foo() {
	}
    }

    /**
     * A typed inner class
     * @param <T> some type
     */
    public class InnerTyped<T> {
    }

    /**
     * Compute the answer to the ultimate question of life, the
     * universe, and everything. :marker:
     * @param factor scaling factor to the answer
     * @return the answer to everything (42) scaled by factor
     */
    public int answer(int factor) {
	return 42 * factor;
    }

    /** Private */
    private double foo(double value) {
	return value;
    }

    /** Protected */
    protected double bar(double value) {
	return value;
    }

    /** No qualifier*/
    String noqualifier() {
	return "something";
    }

    /** Void */
    public void voidmethod(boolean t) {
    }

    /**
     * Typed parameter
     * @param <A> the parameter type
     * @param a parameter
     * @return something
     */
    public <A> void tparams(A a) {
    }

    /**
     * Typed parameter
     * @param <A> the return type
     * @param <B> the parameter typeA
     * @param b parameter
     * @return casts B to A
     */
    public <A, B extends A> A cast(B b) {
	return (B) b;
    }

}
