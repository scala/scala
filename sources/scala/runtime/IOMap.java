/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.runtime;

/**
 * Purely functional maps from integers to objects. Implemented as
 * red-black trees.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class IOMap implements java.io.Serializable {

    /** The map class itself */
    public static class T implements java.io.Serializable {
        public case N(int c, T l, T r, int k, Object v);
        public case E;
    }

    public static final T EMPTY = T.E;

    // Node colors (Black and Red)
    private static final int B = 0;
    private static final int R = 1;

    public static class ConflictException extends Exception {
        public final int key;
        public final Object oldValue, newValue;

        public ConflictException(int key, Object oldValue, Object newValue) {
            this.key = key;
            this.oldValue = oldValue;
            this.newValue = newValue;
        }
        public Throwable fillInStackTrace() {
            // do nothing, to speed up things
            return this;
        }
    }

    public Object resolveConflict(int k, Object oldV, Object newV)
        throws ConflictException {
        throw new ConflictException(k, oldV, newV);
    }

    public T put(T map, int key, Object value) throws ConflictException {
        switch (putAux(map, key, value)) {
        case N(_, T l, T r, int k, Object v):
            return T.N(B, l, r, k, v);
        default:
            throw new Error();
        }
    }

    private T putAux(T map, int key, Object value) throws ConflictException {
        switch (map) {
        case N(int c, T l, T r, int k, Object v):
            if (key < k)
                return balance(T.N(c, putAux(l, key, value), r, k, v));
            else if (key > k)
                return balance(T.N(c, l, putAux(r, key, value), k, v));
            else
                return T.N(c, l, r, k, resolveConflict(k, v, value));
        case E:
            return T.N(R, T.E, T.E, key, value);
        default:
            throw new Error();
        }
    }

    private T balance(T t) {
        switch (t) {
        case N(B,
               N(R, N(R, T a, T b, int xK, Object xV), T c, int yK, Object yV),
               T d,
               int zK, Object zV):
            return T.N(R, T.N(B, a, b, xK, xV), T.N(B, c, d, zK, zV), yK, yV);
        case N(B,
               N(R, T a, N(R, T b, T c, int yK, Object yV), int xK, Object xV),
               T d,
               int zK, Object zV):
            return T.N(R, T.N(B, a, b, xK, xV), T.N(B, c, d, zK, zV), yK, yV);
        case N(B,
               T a,
               N(R, N(R, T b, T c, int yK, Object yV), T d, int zK, Object zV),
               int xK, Object xV):
            return T.N(R, T.N(B, a, b, xK, xV), T.N(B, c, d, zK, zV), yK, yV);
        case N(B,
               T a,
               N(R, T b, N(R, T c, T d, int zK, Object zV), int yK, Object yV),
               int xK, Object xV):
            return T.N(R, T.N(B, a, b, xK, xV), T.N(B, c, d, zK, zV), yK, yV);
        default:
            return t;
        }
    }

    public Object get(T map, int key) {
        switch (map) {
        case N(_, T l, T r, int k, Object v):
            if (key < k)
                return get(l, key);
            else if (key > k)
                return get(r, key);
            else
                return v;
        case E:
            return null;
        default:
            throw new Error("unexpected node " + this);
        }
    }

    public int size(T map) {
        switch (map) {
        case N(_, T l, T r, _, _):
            return size(l) + size(r) + 1;
        case E:
            return 0;
        default:
            throw new Error("unexpected node " + this);
        }
    }

    public int depth(T map) {
        switch (map) {
        case N(_, T l, T r, _, _):
            return Math.max(depth(l), depth(r)) + 1;
        case E:
            return 0;
        default:
            throw new Error("unexpected node " + this);
        }
    }
}

// class RBTest {
//     static class MyIOMap extends IOMap {
//         public Object resolveConflict(int k, Object oldV, Object newV) {
//             throw new Error("conflict!!!");
//         }
//     }

//     public static void main(String[] args) {
//         MyIOMap map = new MyIOMap();
//         MyIOMap.T t = map.EMPTY;

//         long start = System.currentTimeMillis();
//         for (int i = 0; i < args.length; ++i) {
//             t = map.put(t, FNV_Hash.hash32(args[i]), new Integer(i));
//         }

//         for (int i = 0; i < args.length; ++i) {
//             map.get(t, FNV_Hash.hash32(args[i]));
//         }
//         long end = System.currentTimeMillis();
//         System.out.println("time: " + (end - start) + "ms");

//         System.out.println("size  = " + map.size(t));
//         System.out.println("depth = " + map.depth(t));
//     }
// }
