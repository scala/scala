/*                     __                                               * \
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.runtime.types;

import scala.Type;
import scala.Array;
import scala.runtime.AtomicReference;
import scala.runtime.IOMap;
import scala.runtime.FNV_Hash;

/**
 * Class modelling a type constructor (this includes non-polymorphic
 * types, which are handled as polymorphic types with zero arguments).
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class TypeConstructor {
    public final static TypeConstructor[] EMPTY_ARRAY =
        new TypeConstructor[0];

    /** Full (qualified) name for this constructor. */
    public final String fullName;

    /** Java class corresponding to this constructor. */
    public Class clazz;

    /** Enclosing class for this type constructor */
    public final Object outer;

    /**
     * Number of invariant (z), contravariant (m) and covariant (p)
     * type arguments.
     */
    public final int zCount, mCount, pCount;

    /**
     * Level of this type in the hierarchy (scala.AnyRef is at 0, its
     * direct children at 1, and so on).
     */
    public final int level;

    /**
     * Indication of triviality: a constructor is trivial iff it has
     * no enclosing class, and no type arguments.
     */
    public final boolean isTrivial;

    /**
     * "Code" to compute the display for an instance of this
     * constructor, based on the display of its parents. This code is
     * structured as follows:
     *
     * n1 p1,1 o1,1 p1,2 o1,2 ... p1,n o1,n  n2 p2,1 ...  nl pl,1 ol,2 ...
     *
     * where all n, p and o are integers, and l is the level of this
     * constructor. ni gives the number of additional entries to add
     * to the display of the super-class at level i. pi gives the
     * index of the parent in which to pick this additional entry, and
     * oi gives the offset of this entry in the parent's display.
     */
    public final int[] displayCode;

    private final InstantiationMap instMapModule = new InstantiationMap();
    private final AtomicReference/*<InstantiationMap.T>*/ instances =
        new AtomicReference(IOMap.EMPTY);

    private static final ClassLoader loader =
        ClassLoader.getSystemClassLoader();

    public TypeConstructor(int level,
                           String fullName,
                           Object outer,
                           int zCount,
                           int mCount,
                           int pCount,
                           int[] displayCode) {
        this.level = level;
        this.fullName = fullName;
        this.outer = outer;
        this.zCount = zCount;
        this.mCount = mCount;
        this.pCount = pCount;

        this.isTrivial = (outer == null) && (zCount + pCount + mCount == 0);

        this.displayCode = displayCode;

        try {
            this.clazz = Class.forName(fullName, false, loader);
        } catch (ClassNotFoundException e) {
            throw new Error(e);
        }

        assert (zCount >= 0) && (mCount >= 0) && (pCount >= 0);
    }

    public String toString() {
        return fullName;
    }

    public ScalaClassType getInstantiation(Type[] args) {
        return instMapModule.get((InstantiationMap.T)instances.get(), args);
    }

    public ScalaClassType instantiate(Type[] args, ScalaClassType[] parents) {
        ScalaClassType tp = new ScalaClassType(this, args, parents);

        try {
            InstantiationMap.T oldMap, newMap;
            do {
                oldMap = (InstantiationMap.T)instances.get();
                newMap = instMapModule.put(oldMap, args, tp);
            } while (!instances.compareAndSet(oldMap, newMap));
        } catch (IOMap.ConflictException e) {
            return (ScalaClassType)e.oldValue;
        }
        return tp;
    }


    //////////////////////////////////////////////////////////////////////

    private static class InstantiationMap extends IOMap {
        public T put(T map, Type[] inst, ScalaClassType value)
            throws ConflictException {
            return super.put(map, hashInst(inst), value);
        }

        public ScalaClassType get(T map, Type[] inst) {
            return (ScalaClassType)super.get(map, hashInst(inst));
        }

        // Random permutation of [0;255], used for Pearson's hashing.
        private int[] table = new int[] {
            251, 117, 191, 48, 37, 199, 178, 157, 9, 50, 183, 197, 42, 40, 104,
            83, 156, 250, 215, 14, 233, 33, 74, 253, 128, 10, 36, 73, 217, 224,
            116, 86, 132, 204, 20, 2, 80, 55, 222, 5, 207, 201, 129, 216, 165,
            155, 159, 236, 19, 146, 108, 124, 112, 0, 58, 92, 70, 152, 135, 88,
            97, 122, 61, 255, 184, 211, 214, 141, 67, 79, 18, 62, 101, 173,
            238, 154, 170, 164, 130, 229, 252, 205, 43, 81, 94, 149, 59, 151,
            93, 45, 25, 166, 139, 44, 143, 16, 188, 30, 91, 218, 77, 60, 142,
            168, 47, 176, 13, 49, 34, 102, 31, 65, 203, 76, 240, 78, 115, 84,
            244, 32, 11, 175, 247, 209, 242, 71, 163, 167, 35, 136, 22, 237,
            134, 56, 181, 17, 4, 24, 206, 192, 105, 63, 89, 239, 6, 72, 53,
            219, 69, 227, 133, 15, 161, 68, 120, 12, 111, 179, 245, 100, 103,
            8, 148, 107, 144, 127, 160, 26, 241, 162, 213, 1, 220, 150, 82,
            190, 96, 98, 137, 174, 145, 46, 243, 125, 198, 231, 66, 234, 177,
            212, 210, 226, 95, 228, 21, 254, 27, 28, 121, 196, 187, 54, 249,
            109, 208, 153, 232, 194, 113, 23, 140, 235, 158, 248, 182, 202,
            186, 147, 119, 225, 87, 126, 64, 221, 193, 246, 169, 189, 90, 180,
            138, 57, 38, 75, 230, 41, 123, 110, 223, 118, 106, 7, 172, 114,
            131, 99, 51, 185, 39, 171, 195, 52, 29, 200, 3, 85
        };

        // Pearson's algorithm, used to hash an integer into a single
        // byte.
        private int hashInt(int i) {
            final int h1 = table[i & 0xFF];
            final int h2 = table[h1 ^ ((i >>>  8) & 0xFF)];
            final int h3 = table[h2 ^ ((i >>> 16) & 0xFF)];
            return table[h3 ^ (i >>> 24)];
        }

        private int hashInst(Type[] inst) {
            final int len = inst.length;

            int h = FNV_Hash.INIT;
            for (int i = 0; i < len; ++i)
                h = FNV_Hash.hashStep(h, hashInt(inst[i].hashCode()));

            return h;
        }
    }
}
