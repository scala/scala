/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $OldId: Array.java,v 1.9 2002/03/18 16:55:10 zenger Exp $
// $Id$

package scala;

/** @meta class [?T] extends scala.Function1[scala.Int, ?T];
 */
public abstract class Array
    extends java.lang.Object
    implements Function1, Cloneable, java.io.Serializable {

    /** @meta constr (scala.Int);
     */
    public Array() {
    }

    public boolean[] asBooleanArray() {
        throw new ClassCastException();
    }

    public byte[] asByteArray() {
        throw new ClassCastException();
    }

    public short[] asShortArray() {
        throw new ClassCastException();
    }

    public char[] asCharArray() {
        throw new ClassCastException();
    }

    public int[] asIntArray() {
        throw new ClassCastException();
    }

    public long[] asLongArray() {
        throw new ClassCastException();
    }

    public float[] asFloatArray() {
        throw new ClassCastException();
    }

    public double[] asDoubleArray() {
        throw new ClassCastException();
    }

    /** @meta method () scala.Array[scala.AnyRef];
     */
    public java.lang.Object[] asObjectArray() {
        throw new ClassCastException();
    }

    /** @meta method () scala.Array[?T];
     */
    public java.lang.Object asArray() {
        throw new ClassCastException();
    }

    public java.lang.Object apply(java.lang.Object i) {
	return apply(((Number) i).intValue());
    }

    /** @meta method (scala.Int) ?T;
     */
    public abstract java.lang.Object apply(int i);

    /** @meta method (scala.Int, ?T) scala.Unit;
     */
    public abstract void update(int i, java.lang.Object x);

    /** @meta method []scala.Int;
     */
    public abstract int length();
}
