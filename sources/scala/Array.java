/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


/** @meta class [?T] extends scala.Function1[scala.Int, ?T] with scala.ScalaObject with java.lang.Cloneable with java.io.Serializable;
 */
public abstract class Array
    extends java.lang.Object
    implements ScalaObject, Function1, Cloneable, java.io.Serializable {

    /** @meta constr (scala.Int);
     */
    public Array() {
    }

    /** @meta method []scala.Int;
     */
    public abstract int length();

    public boolean isDefinedAt(int x) {
        return (x >= 0) && (x < length());
    }

    /** @meta method (scala.Int, ?T) scala.Unit;
     */
    public abstract void update(int i, java.lang.Object x);

    /** @meta method (scala.Int) ?T;
     */
    public abstract java.lang.Object apply(int i);

    public java.lang.Object apply(java.lang.Object i) {
		return apply(((scala.Int)i).value);
    }

    /** @meta method () scala.Array[scala.AnyRef];
     */
    public java.lang.Object[] asObjectArray() {
        throw new ClassCastException();
    }

    /** @meta method (scala.Function1[?T, scala.Unit]) scala.Unit;
     */
    public void foreach(Function1 f) {
    	for (int i = 0; i < length(); i++)
    		f.apply(apply(scala.runtime.RunTime.box_ivalue(i)));
    }

    /** @meta method (scala.Function1[?T, scala.Boolean]) scala.Boolean;
     */
    public boolean forall(Function1 f) {
    	for (int i = 0; i < length(); i++)
    		if (!((scala.Boolean)f.apply(apply(scala.runtime.RunTime.box_ivalue(i)))).value)
    			return false;
    	return true;
    }

    /** @meta method (scala.Function1[?T, scala.Boolean]) scala.Boolean;
     */
    public boolean exists(Function1 f) {
    	for (int i = 0; i < length(); i++)
    		if (((scala.Boolean)f.apply(apply(scala.runtime.RunTime.box_ivalue(i)))).value)
    			return true;
    	return false;
    }

    /** @meta method () scala.Array[?T];
     */
    public java.lang.Object asArray() {
        throw new ClassCastException();
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

    public int $tag() {
    	return 0;
    }
}
