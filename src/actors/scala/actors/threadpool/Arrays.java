/*
 * Written by Dawid Kurzyniec, based on code written by Doug Lea with assistance
 * from members of JCP JSR-166 Expert Group. Released to the public domain,
 * as explained at http://creativecommons.org/licenses/publicdomain.
 */

package scala.actors.threadpool;

import java.lang.reflect.Array;
import java.util.List;
import java.util.ArrayList;
import java.util.Comparator;

public class Arrays {

    private Arrays() {}

    public static void sort(long[] a) {
        java.util.Arrays.sort(a);
    }

    public static void sort(long[] a, int fromIndex, int toIndex) {
        java.util.Arrays.sort(a, fromIndex, toIndex);
    }

    public static void sort(int[] a) {
        java.util.Arrays.sort(a);
    }

    public static void sort(int[] a, int fromIndex, int toIndex) {
        java.util.Arrays.sort(a, fromIndex, toIndex);
    }

    public static void sort(short[] a) {
        java.util.Arrays.sort(a);
    }

    public static void sort(short[] a, int fromIndex, int toIndex) {
        java.util.Arrays.sort(a, fromIndex, toIndex);
    }

    public static void sort(char[] a) {
        java.util.Arrays.sort(a);
    }

    public static void sort(char[] a, int fromIndex, int toIndex) {
        java.util.Arrays.sort(a, fromIndex, toIndex);
    }

    public static void sort(byte[] a) {
        java.util.Arrays.sort(a);
    }

    public static void sort(byte[] a, int fromIndex, int toIndex) {
        java.util.Arrays.sort(a, fromIndex, toIndex);
    }

    public static void sort(double[] a) {
        java.util.Arrays.sort(a);
    }

    public static void sort(double[] a, int fromIndex, int toIndex) {
        java.util.Arrays.sort(a, fromIndex, toIndex);
    }

    public static void sort(float[] a) {
        java.util.Arrays.sort(a);
    }

    public static void sort(float[] a, int fromIndex, int toIndex) {
        java.util.Arrays.sort(a, fromIndex, toIndex);
    }


    public static void sort(Object[] a) {
        java.util.Arrays.sort(a);
    }

    public static void sort(Object[] a, int fromIndex, int toIndex) {
        java.util.Arrays.sort(a, fromIndex, toIndex);
    }

    public static void sort(Object[] a, Comparator c) {
        java.util.Arrays.sort(a, c);
    }

    public static void sort(Object[] a, int fromIndex, int toIndex, Comparator c) {
        java.util.Arrays.sort(a, fromIndex, toIndex, c);
    }


    // Searching

    public static int binarySearch(long[] a, long key) {
        return java.util.Arrays.binarySearch(a, key);
    }

    public static int binarySearch(int[] a, int key) {
        return java.util.Arrays.binarySearch(a, key);
    }

    public static int binarySearch(short[] a, short key) {
        return java.util.Arrays.binarySearch(a, key);
    }

    public static int binarySearch(char[] a, char key) {
        return java.util.Arrays.binarySearch(a, key);
    }

    public static int binarySearch(byte[] a, byte key) {
        return java.util.Arrays.binarySearch(a, key);
    }

    public static int binarySearch(double[] a, double key) {
        return java.util.Arrays.binarySearch(a, key);
    }

    public static int binarySearch(float[] a, float key) {
        return java.util.Arrays.binarySearch(a, key);
    }

    public static int binarySearch(Object[] a, Object key) {
        return java.util.Arrays.binarySearch(a, key);
    }

    public static int binarySearch(Object[] a, Object key, Comparator c) {
        return java.util.Arrays.binarySearch(a, key, c);
    }


    // Equality Testing

    public static boolean equals(long[] a, long[] a2) {
        return java.util.Arrays.equals(a, a2);
    }

    public static boolean equals(int[] a, int[] a2) {
        return java.util.Arrays.equals(a, a2);
    }

    public static boolean equals(short[] a, short a2[]) {
        return java.util.Arrays.equals(a, a2);
    }

    public static boolean equals(char[] a, char[] a2) {
        return java.util.Arrays.equals(a, a2);
    }

    public static boolean equals(byte[] a, byte[] a2) {
        return java.util.Arrays.equals(a, a2);
    }

    public static boolean equals(boolean[] a, boolean[] a2) {
        return java.util.Arrays.equals(a, a2);
    }

    public static boolean equals(double[] a, double[] a2) {
        return java.util.Arrays.equals(a, a2);
    }

    public static boolean equals(float[] a, float[] a2) {
        return java.util.Arrays.equals(a, a2);
    }

    public static boolean equals(Object[] a, Object[] a2) {
        return java.util.Arrays.equals(a, a2);
    }


    // Filling

    public static void fill(long[] a, long val) {
        java.util.Arrays.fill(a, val);
    }

    public static void fill(long[] a, int fromIndex, int toIndex, long val) {
        java.util.Arrays.fill(a, fromIndex, toIndex, val);
    }

    public static void fill(int[] a, int val) {
        java.util.Arrays.fill(a, val);
    }

    public static void fill(int[] a, int fromIndex, int toIndex, int val) {
        java.util.Arrays.fill(a, fromIndex, toIndex, val);
    }

    public static void fill(short[] a, short val) {
        java.util.Arrays.fill(a, val);
    }

    public static void fill(short[] a, int fromIndex, int toIndex, short val) {
        java.util.Arrays.fill(a, fromIndex, toIndex, val);
    }

    public static void fill(char[] a, char val) {
        java.util.Arrays.fill(a, val);
    }

    public static void fill(char[] a, int fromIndex, int toIndex, char val) {
        java.util.Arrays.fill(a, fromIndex, toIndex, val);
    }

    public static void fill(byte[] a, byte val) {
        java.util.Arrays.fill(a, val);
    }

    public static void fill(byte[] a, int fromIndex, int toIndex, byte val) {
        java.util.Arrays.fill(a, fromIndex, toIndex, val);
    }

    public static void fill(boolean[] a, boolean val) {
        java.util.Arrays.fill(a, val);
    }

    public static void fill(boolean[] a, int fromIndex, int toIndex,
                            boolean val) {
        java.util.Arrays.fill(a, fromIndex, toIndex, val);
    }

    public static void fill(double[] a, double val) {
        java.util.Arrays.fill(a, val);
    }

    public static void fill(double[] a, int fromIndex, int toIndex,double val) {
        java.util.Arrays.fill(a, fromIndex, toIndex, val);
    }

    public static void fill(float[] a, float val) {
        java.util.Arrays.fill(a, val);
    }

    public static void fill(float[] a, int fromIndex, int toIndex, float val) {
        java.util.Arrays.fill(a, fromIndex, toIndex, val);
    }

    public static void fill(Object[] a, Object val) {
        java.util.Arrays.fill(a, val);
    }

    public static void fill(Object[] a, int fromIndex, int toIndex, Object val) {
        java.util.Arrays.fill(a, fromIndex, toIndex, val);
    }


    // Cloning

    /**
     * @since 1.6
     */
    public static Object[] copyOf(Object[] original, int newLength) {
        return copyOf(original, newLength, original.getClass());
    }

    /**
     * @since 1.6
     */
    public static Object[] copyOf(Object[] original, int newLength, Class newType) {
        Object[] arr = (newType == Object[].class) ? new Object[newLength] :
            (Object[])Array.newInstance(newType.getComponentType(), newLength);
        int len  = (original.length < newLength ? original.length : newLength);
        System.arraycopy(original, 0, arr, 0, len);
        return arr;
    }

    /**
     * @since 1.6
     */
    public static byte[] copyOf(byte[] original, int newLength) {
        byte[] arr = new byte[newLength];
        int len  = (original.length < newLength ? original.length : newLength);
        System.arraycopy(original, 0, arr, 0, len);
        return arr;
    }

    /**
     * @since 1.6
     */
    public static short[] copyOf(short[] original, int newLength) {
        short[] arr = new short[newLength];
        int len  = (original.length < newLength ? original.length : newLength);
        System.arraycopy(original, 0, arr, 0, len);
        return arr;
    }

    /**
     * @since 1.6
     */
    public static int[] copyOf(int[] original, int newLength) {
        int[] arr = new int[newLength];
        int len  = (original.length < newLength ? original.length : newLength);
        System.arraycopy(original, 0, arr, 0, len);
        return arr;
    }

    /**
     * @since 1.6
     */
    public static long[] copyOf(long[] original, int newLength) {
        long[] arr = new long[newLength];
        int len  = (original.length < newLength ? original.length : newLength);
        System.arraycopy(original, 0, arr, 0, len);
        return arr;
    }

    /**
     * @since 1.6
     */
    public static char[] copyOf(char[] original, int newLength) {
        char[] arr = new char[newLength];
        int len  = (original.length < newLength ? original.length : newLength);
        System.arraycopy(original, 0, arr, 0, len);
        return arr;
    }

    /**
     * @since 1.6
     */
    public static float[] copyOf(float[] original, int newLength) {
        float[] arr = new float[newLength];
        int len  = (original.length < newLength ? original.length : newLength);
        System.arraycopy(original, 0, arr, 0, len);
        return arr;
    }

    /**
     * @since 1.6
     */
    public static double[] copyOf(double[] original, int newLength) {
        double[] arr = new double[newLength];
        int len  = (original.length < newLength ? original.length : newLength);
        System.arraycopy(original, 0, arr, 0, len);
        return arr;
    }

    /**
     * @since 1.6
     */
    public static boolean[] copyOf(boolean[] original, int newLength) {
        boolean[] arr = new boolean[newLength];
        int len  = (original.length < newLength ? original.length : newLength);
        System.arraycopy(original, 0, arr, 0, len);
        return arr;
    }

    /**
     * @since 1.6
     */
    public static Object[] copyOfRange(Object[] original, int from, int to) {
        return copyOfRange(original, from, to, original.getClass());
    }

    /**
     * @since 1.6
     */
    public static Object[] copyOfRange(Object[] original, int from, int to, Class newType) {
        int newLength = to - from;
        if (newLength < 0) throw new IllegalArgumentException(from + " > " + to);
        Object[] arr = (newType == Object[].class) ? new Object[newLength] :
            (Object[])Array.newInstance(newType.getComponentType(), newLength);
        int ceil = original.length-from;
        int len = (ceil < newLength) ? ceil : newLength;
        System.arraycopy(original, from, arr, 0, len);
        return arr;
    }

    /**
     * @since 1.6
     */
    public static byte[] copyOfRange(byte[] original, int from, int to) {
        int newLength = to - from;
        if (newLength < 0) throw new IllegalArgumentException(from + " > " + to);
        byte[] arr = new byte[newLength];
        int ceil = original.length-from;
        int len = (ceil < newLength) ? ceil : newLength;
        System.arraycopy(original, from, arr, 0, len);
        return arr;
    }

    /**
     * @since 1.6
     */
    public static short[] copyOfRange(short[] original, int from, int to) {
        int newLength = to - from;
        if (newLength < 0) throw new IllegalArgumentException(from + " > " + to);
        short[] arr = new short[newLength];
        int ceil = original.length-from;
        int len = (ceil < newLength) ? ceil : newLength;
        System.arraycopy(original, from, arr, 0, len);
        return arr;
    }

    /**
     * @since 1.6
     */
    public static int[] copyOfRange(int[] original, int from, int to) {
        int newLength = to - from;
        if (newLength < 0) throw new IllegalArgumentException(from + " > " + to);
        int[] arr = new int[newLength];
        int ceil = original.length-from;
        int len = (ceil < newLength) ? ceil : newLength;
        System.arraycopy(original, from, arr, 0, len);
        return arr;
    }

    /**
     * @since 1.6
     */
    public static long[] copyOfRange(long[] original, int from, int to) {
        int newLength = to - from;
        if (newLength < 0) throw new IllegalArgumentException(from + " > " + to);
        long[] arr = new long[newLength];
        int ceil = original.length-from;
        int len = (ceil < newLength) ? ceil : newLength;
        System.arraycopy(original, from, arr, 0, len);
        return arr;
    }

    /**
     * @since 1.6
     */
    public static char[] copyOfRange(char[] original, int from, int to) {
        int newLength = to - from;
        if (newLength < 0) throw new IllegalArgumentException(from + " > " + to);
        char[] arr = new char[newLength];
        int ceil = original.length-from;
        int len = (ceil < newLength) ? ceil : newLength;
        System.arraycopy(original, from, arr, 0, len);
        return arr;
    }

    /**
     * @since 1.6
     */
    public static float[] copyOfRange(float[] original, int from, int to) {
        int newLength = to - from;
        if (newLength < 0) throw new IllegalArgumentException(from + " > " + to);
        float[] arr = new float[newLength];
        int ceil = original.length-from;
        int len = (ceil < newLength) ? ceil : newLength;
        System.arraycopy(original, from, arr, 0, len);
        return arr;
    }

    /**
     * @since 1.6
     */
    public static double[] copyOfRange(double[] original, int from, int to) {
        int newLength = to - from;
        if (newLength < 0) throw new IllegalArgumentException(from + " > " + to);
        double[] arr = new double[newLength];
        int ceil = original.length-from;
        int len = (ceil < newLength) ? ceil : newLength;
        System.arraycopy(original, from, arr, 0, len);
        return arr;
    }

    /**
     * @since 1.6
     */
    public static boolean[] copyOfRange(boolean[] original, int from, int to) {
        int newLength = to - from;
        if (newLength < 0) throw new IllegalArgumentException(from + " > " + to);
        boolean[] arr = new boolean[newLength];
        int ceil = original.length-from;
        int len = (ceil < newLength) ? ceil : newLength;
        System.arraycopy(original, from, arr, 0, len);
        return arr;
    }


    public static List asList(Object[] a) {
        return java.util.Arrays.asList(a);
    }

    /**
     * @since 1.5
     */
    public static int hashCode(long a[]) {
        if (a == null) return 0;
        int hash = 1;
        for (int i=0; i<a.length; i++) {
            long e = a[i];
            hash = 31*hash + (int)(e ^ (e >>> 32));
        }
        return hash;
    }

    /**
     * @since 1.5
     */
    public static int hashCode(int a[]) {
        if (a == null) return 0;
        int hash = 1;
        for (int i=0; i<a.length; i++) {
            hash = 31*hash + a[i];
        }
        return hash;
    }

    /**
     * @since 1.5
     */
    public static int hashCode(short a[]) {
        if (a == null) return 0;
        int hash = 1;
        for (int i=0; i<a.length; i++) {
            hash = 31*hash + a[i];
        }
        return hash;
    }

    /**
     * @since 1.5
     */
    public static int hashCode(char a[]) {
        if (a == null) return 0;
        int hash = 1;
        for (int i=0; i<a.length; i++) {
            hash = 31*hash + a[i];
        }
        return hash;
    }

    /**
     * @since 1.5
     */
    public static int hashCode(byte a[]) {
        if (a == null) return 0;
        int hash = 1;
        for (int i=0; i<a.length; i++) {
            hash = 31*hash + a[i];
        }
        return hash;
    }

    /**
     * @since 1.5
     */
    public static int hashCode(boolean a[]) {
        if (a == null) return 0;
        int hash = 1;
        for (int i=0; i<a.length; i++) {
            hash = 31*hash + (a[i] ? 1231 : 1237);
        }
        return hash;
    }

    /**
     * @since 1.5
     */
    public static int hashCode(float a[]) {
        if (a == null) return 0;
        int hash = 1;
        for (int i=0; i<a.length; i++) {
            hash = 31*hash + Float.floatToIntBits(a[i]);
        }
        return hash;
    }

    /**
     * @since 1.5
     */
    public static int hashCode(double a[]) {
        if (a == null) return 0;
        int hash = 1;
        for (int i=0; i<a.length; i++) {
            long e = Double.doubleToLongBits(a[i]);
            hash = 31*hash + (int)(e ^ (e >>> 32));
        }
        return hash;
    }

    /**
     * @since 1.5
     */
    public static int hashCode(Object a[]) {
        if (a == null) return 0;
        int hash = 1;
        for (int i=0; i<a.length; i++) {
            Object e = a[i];
            hash = 31*hash + (e == null ? 0 : e.hashCode());
        }
        return hash;
    }

    /**
     * @since 1.5
     */
    public static int deepHashCode(Object a[]) {
        if (a == null) return 0;
        int hash = 1;
        for (int i=0; i<a.length; i++) {
            Object e = a[i];
            hash = 31*hash +
                   (e instanceof Object[]  ? deepHashCode((Object[])e) :
                   (e instanceof byte[]    ? hashCode((byte[])e) :
                   (e instanceof short[]   ? hashCode((short[])e) :
                   (e instanceof int[]     ? hashCode((int[])e) :
                   (e instanceof long[]    ? hashCode((long[])e) :
                   (e instanceof char[]    ? hashCode((char[])e) :
                   (e instanceof boolean[] ? hashCode((boolean[])e) :
                   (e instanceof float[]   ? hashCode((float[])e) :
                   (e instanceof double[]  ? hashCode((double[])e) :
                   (e != null              ? e.hashCode() : 0))))))))));
        }
        return hash;

    }

    /**
     * @since 1.5
     */
    public static boolean deepEquals(Object[] a1, Object[] a2) {
        if (a1 == a2) return true;
        if (a1 == null || a2==null) return false;
        int len = a1.length;
        if (len != a2.length) return false;
        for (int i = 0; i < len; i++) {
            Object e1 = a1[i];
            Object e2 = a2[i];
            if (e1 == e2) continue;
            if (e1 == null) return false;
            boolean eq =
                (e1.getClass() != e2.getClass() || e1.getClass().isArray()) ?
                        e1.equals(e2) :
                (e1 instanceof Object[] && e2 instanceof Object[]) ?
                        deepEquals((Object[])e1, (Object[])e2) :
                (e1 instanceof byte[] && e2 instanceof byte[]) ?
                        equals((byte[])e1, (byte[])e2) :
                (e1 instanceof short[] && e2 instanceof short[]) ?
                        equals((short[])e1, (short[])e2) :
                (e1 instanceof int[] && e2 instanceof int[]) ?
                        equals((int[])e1, (int[])e2) :
                (e1 instanceof long[] && e2 instanceof long[]) ?
                        equals((long[])e1, (long[])e2) :
                (e1 instanceof char[] && e2 instanceof char[]) ?
                        equals((char[])e1, (char[])e2) :
                (e1 instanceof boolean[] && e2 instanceof boolean[]) ?
                        equals((boolean[])e1, (boolean[])e2) :
                (e1 instanceof float[] && e2 instanceof float[]) ?
                        equals((float[])e1, (float[])e2) :
                (e1 instanceof double[] && e2 instanceof double[]) ?
                        equals((double[])e1, (double[])e2) :
                e1.equals(e2);

            if (!eq) return false;
        }
        return true;
    }

    /**
     * @since 1.5
     */
    public static String toString(long[] a) {
        if (a == null) return "null";
        if (a.length == 0) return "[]";
        StringBuffer buf = new StringBuffer();
        buf.append('[').append(a[0]);
        for (int i=1; i<a.length; i++) buf.append(", ").append(a[i]);
        buf.append(']');
        return buf.toString();
    }

    /**
     * @since 1.5
     */
    public static String toString(int[] a) {
        if (a == null) return "null";
        if (a.length == 0) return "[]";
        StringBuffer buf = new StringBuffer();
        buf.append('[').append(a[0]);
        for (int i=1; i<a.length; i++) buf.append(", ").append(a[i]);
        buf.append(']');
        return buf.toString();
    }

    /**
     * @since 1.5
     */
    public static String toString(short[] a) {
        if (a == null) return "null";
        if (a.length == 0) return "[]";
        StringBuffer buf = new StringBuffer();
        buf.append('[').append(a[0]);
        for (int i=1; i<a.length; i++) buf.append(", ").append(a[i]);
        buf.append(']');
        return buf.toString();
    }

    /**
     * @since 1.5
     */
    public static String toString(char[] a) {
        if (a == null) return "null";
        if (a.length == 0) return "[]";
        StringBuffer buf = new StringBuffer();
        buf.append('[').append(a[0]);
        for (int i=1; i<a.length; i++) buf.append(", ").append(a[i]);
        buf.append(']');
        return buf.toString();
    }

    /**
     * @since 1.5
     */
    public static String toString(byte[] a) {
        if (a == null) return "null";
        if (a.length == 0) return "[]";
        StringBuffer buf = new StringBuffer();
        buf.append('[').append(a[0]);
        for (int i=1; i<a.length; i++) buf.append(", ").append(a[i]);
        buf.append(']');
        return buf.toString();
    }

    /**
     * @since 1.5
     */
    public static String toString(boolean[] a) {
        if (a == null) return "null";
        if (a.length == 0) return "[]";
        StringBuffer buf = new StringBuffer();
        buf.append('[').append(a[0]);
        for (int i=1; i<a.length; i++) buf.append(", ").append(a[i]);
        buf.append(']');
        return buf.toString();
    }

    /**
     * @since 1.5
     */
    public static String toString(float[] a) {
        if (a == null) return "null";
        if (a.length == 0) return "[]";
        StringBuffer buf = new StringBuffer();
        buf.append('[').append(a[0]);
        for (int i=1; i<a.length; i++) buf.append(", ").append(a[i]);
        buf.append(']');
        return buf.toString();
    }

    /**
     * @since 1.5
     */
    public static String toString(double[] a) {
        if (a == null) return "null";
        if (a.length == 0) return "[]";
        StringBuffer buf = new StringBuffer();
        buf.append('[').append(a[0]);
        for (int i=1; i<a.length; i++) buf.append(", ").append(a[i]);
        buf.append(']');
        return buf.toString();
    }

    /**
     * @since 1.5
     */
    public static String toString(Object[] a) {
        if (a == null) return "null";
        if (a.length == 0) return "[]";
        StringBuffer buf = new StringBuffer();
        buf.append('[').append(a[0]);
        for (int i=1; i<a.length; i++) buf.append(", ").append(a[i]);
        buf.append(']');
        return buf.toString();
    }

    /**
     * @since 1.5
     */
    public static String deepToString(Object[] a) {
        if (a == null) return "null";
        StringBuffer buf = new StringBuffer();
        deepToString(a, buf, new ArrayList());
        return buf.toString();
    }

    private static void deepToString(Object[] a, StringBuffer buf, List seen) {
        seen.add(a);
        buf.append('[');
        for (int i = 0; i < a.length; i++) {
            if (i>0) buf.append(", ");
            Object e = a[i];
            if (e == null) {
                buf.append("null");
            }
            else if (!e.getClass().isArray()) {
                buf.append(e.toString());
            }
            else if (e instanceof Object[]) {
                if (seen.contains(e)) buf.append("[...]");
                else deepToString((Object[])e, buf, seen);
            }
            else {
                // primitive arr
                buf.append(
                    (e instanceof byte[]) ? toString( (byte[]) e) :
                    (e instanceof short[]) ? toString( (short[]) e) :
                    (e instanceof int[]) ? toString( (int[]) e) :
                    (e instanceof long[]) ? toString( (long[]) e) :
                    (e instanceof char[]) ? toString( (char[]) e) :
                    (e instanceof boolean[]) ? toString( (boolean[]) e) :
                    (e instanceof float[]) ? toString( (float[]) e) :
                    (e instanceof double[]) ? toString( (double[]) e) : "");
            }
        }
        buf.append(']');
        seen.remove(seen.size()-1);
    }
}
