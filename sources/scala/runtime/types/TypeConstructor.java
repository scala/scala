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

/**
 * Class modelling a type constructor (this includes non-polymorphic
 * types, which are handled as polymorphic types with zero arguments).
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class TypeConstructor implements java.io.Serializable {
    public final static TypeConstructor[] EMPTY_ARRAY =
        new TypeConstructor[0];

    public final static Object FUNCTION_OUTER = new Object();

    /** Java class corresponding to this constructor. */
    public Class clazz;

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
     * no enclosing class, and no type arguments. It is strongly
     * trivial if all its ancestors, itself included, are trivial.
     */
    public final boolean isTrivial;
    public final boolean isStronglyTrivial;

    public final int ancestorCacheDepth;
    /**
     * "Code" to compute the ancestors for an instance of this
     * constructor, based on the ancestors of its not-strongly-trivial
     * parents. This code is structured as follows:
     *
     * l1 n1 p1,0 o1,0 p1,1 o1,1 ... l2 n2 p2,0 o2,0 ...
     *
     * where all l, n, p and o are integers. ni gives the number of
     * additional entries to add to the ancestors of the first parent
     * at level li. pi gives the index of the parent in which to pick
     * this additional entry, and oi gives the offset of this entry in
     * the parent's ancestors.
     */
    public final int[] ancestorCode;

    /** Enclosing class for this type constructor */
    private final Object outer;

    private final InstantiationMap instMapModule = new InstantiationMap();
    private final AtomicReference/*<InstantiationMap.T>*/ instances =
        new AtomicReference(IOMap.EMPTY);

    private static final ClassLoader loader =
        ClassLoader.getSystemClassLoader();

    private static final int[] EMPTY_ANCESTOR_CODE = new int[0];

    public TypeConstructor(int level,
                           String fullName,
                           Object outer,
                           int zCount,
                           int mCount,
                           int pCount,
                           int ancestorCacheDepth,
                           int[] ancestorCode) {
        this.level = level;
        this.outer = outer;
        this.zCount = zCount;
        this.mCount = mCount;
        this.pCount = pCount;

        this.ancestorCacheDepth = ancestorCacheDepth;
        this.ancestorCode =
            (ancestorCode == null ? EMPTY_ANCESTOR_CODE : ancestorCode);

        this.isTrivial = (outer == null) && (zCount + pCount + mCount == 0);
        this.isStronglyTrivial = (ancestorCacheDepth == 0);

        try {
            this.clazz = Class.forName(fullName, false, loader);
        } catch (ClassNotFoundException e) {
            throw new Error(e);
        }

        assert (zCount >= 0) && (mCount >= 0) && (pCount >= 0);
    }

    public String toString() {
        if (outer == null)
            return clazz.getName();
        else if (outer == FUNCTION_OUTER)
            return "<function>." + clazz.getName();
        else
            return outer.toString() + "." + clazz.getName();
    }

    public ScalaClassType getInstantiation(Type[] args) {
        ScalaClassType inst =
            instMapModule.get((InstantiationMap.T)instances.get(), args);
        assert Statistics.incInstantiations(inst == null);
        return inst;
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

    private static class InstantiationMap
        extends IOMap
        implements java.io.Serializable {
        public T put(T map, Type[] inst, ScalaClassType value)
            throws ConflictException {
            return super.put(map, Type.hashCode(inst), value);
        }

        public ScalaClassType get(T map, Type[] inst) {
            return (ScalaClassType)super.get(map, Type.hashCode(inst));
        }
    }
}
