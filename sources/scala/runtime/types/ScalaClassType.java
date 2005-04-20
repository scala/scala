/*                     __                                               *\
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
import scala.ScalaObject;
import scala.runtime.RunTime;
import scala.runtime.FNV_Hash;
import scala.runtime.PearsonHash;

public class ScalaClassType extends ClassType {
    public static final ScalaClassType[] EMPTY_ARRAY =
        new ScalaClassType[0];

    private static final ScalaClassType[][] EMPTY_ANCESTORS =
        new ScalaClassType[0][];

    private final TypeConstructor constr;
    private final Type[] inst;

    private ScalaClassType[] parents;
    private ScalaClassType[][] ancestors = null;

    private final int hashCode;

    public ScalaClassType(TypeConstructor constr,
                          Type[] inst,
                          ScalaClassType[] parents) {
        super(constr.clazz, constr.isTrivial);

        this.constr = constr;
        this.inst = inst;

        int hash = FNV_Hash.hashStep(FNV_Hash.INIT,
                                     PearsonHash.hash8(constr.hashCode()));
        for (int i = 0; i < inst.length; ++i) {
            hash = FNV_Hash.hashStep(hash,
                                     PearsonHash.hash8(inst[i].hashCode()));
        }
        this.hashCode = hash;
        this.parents = parents;
        this.ancestors = constr.isStronglyTrivial ? EMPTY_ANCESTORS : null;
    }

    public boolean isInstance(Object o) {
        return super.isInstance(o)
            && (isTrivial
                || ((ScalaObject)o).getScalaType().isNonTrivialSubClassType(this));
    }

    public boolean isNonTrivialInstance(Object o) {
        assert Statistics.incWeakInstanceOf();
        return ((ScalaObject)o).getScalaType().isNonTrivialSubClassType(this);
    }

    protected boolean isSubClassType(ClassType that) {
        return (this == that)
            || (super.isSubClassType(that)
                && (that.isTrivial
                    || isNonTrivialSubClassType((ScalaClassType)that)));
    }

    public boolean isNonTrivialSubClassType(ClassType that) {
        ScalaClassType thatCT = (ScalaClassType)that;
        ScalaClassType parentCT = myInstantiationFor(thatCT);

        // At this stage, if parentCT is null, it means that the
        // constructors had different prefixes, hence we return false.
        return (parentCT != null)
            && (parentCT == thatCT || parentCT.hasSubInstantiation(thatCT));
    }

    // Return true iff the instantiation of THIS is "smaller" than the
    // one of THAT.
    private boolean hasSubInstantiation(ScalaClassType that) {
        assert this.constr == that.constr;

        final Type[] thisInst = this.inst;
        final Type[] thatInst = that.inst;

        int i = 0;

        // invariant parameters
        final int firstM = this.constr.zCount;
        while (i < firstM) {
            Type thisTp = thisInst[i], thatTp = thatInst[i];
            if (!(thisTp == thatTp || thisTp.isSameType(thatTp)))
                return false;
            ++i;
        }
        // contravariant parameters
        final int firstP = firstM + this.constr.mCount;
        while (i < firstP) {
            Type thisTp = thisInst[i], thatTp = thatInst[i];
            if (!(thisTp == thatTp || thatTp.isSubType(thisTp)))
                return false;
            ++i;
        }
        // covariant parameters
        final int firstOutside = firstP + this.constr.pCount;
        while (i < firstOutside) {
            Type thisTp = thisInst[i], thatTp = thatInst[i];
            if (!(thisTp == thatTp || thisTp.isSubType(thatTp)))
                return false;
            ++i;
        }
        return true;
    }

    public boolean isSameType(Type that) {
        return this == that;
    }

    private ScalaClassType myInstantiationFor(ScalaClassType that) {
        // Find our instantiation for the other type, if any.
        ScalaClassType[] thisSlice = getAncestors()[that.constr.level];

        for (int i = 0; i < thisSlice.length; ++i) {
            if (thisSlice[i].constr == that.constr) {
                assert Statistics.addAncestorSearchIterations(i + 1);
                return thisSlice[i];
            }
        }
        assert Statistics.addAncestorSearchIterations(thisSlice.length);

        return null;
    }

    public String toString() {
        StringBuffer buf = new StringBuffer();

        int firstM = constr.zCount;
        int firstP = firstM + constr.mCount;
        buf.append(constr);
        if (inst.length > 0) {
            buf.append("[");
            for (int i = 0; i < inst.length; ++i) {
                if (i > 0) buf.append(", ");
                if (i >= firstP)
                    buf.append('+');
                else if (i >= firstM)
                    buf.append('-');
                buf.append(inst[i]);
            }
            buf.append("]");
        }
        return buf.toString();
    }

    public int hashCode() {
        return hashCode;
    }

    public ScalaClassType setParents(ScalaClassType[] parents) {
        assert this.parents == null || Type.isSameType(this.parents, parents);
        this.parents = parents;
        // TODO notifyAll?
        return this;
    }

    public ScalaClassType[] getParents() {
        int timeout = 1;
        while (parents == null) {
            try {
                wait(timeout);
            } catch (InterruptedException e) {
                throw new Error(e);
            }
            timeout *= 2;
            if (timeout >= 1000)
                throw new Error("computation of parents apparently stuck for "
                                + this);
        }
        return parents;
    }

    private ScalaClassType[][] getAncestors() {
        if (ancestors == null)
            computeAncestors();
        return ancestors;
    }

    private void computeAncestors() {
        final int level = constr.level;
        final int ancestorDepth = constr.ancestorCacheDepth;
        final int[] ancestorCode = constr.ancestorCode;
        ScalaClassType[] parents = getParents();

        ancestors = new ScalaClassType[ancestorDepth][];
        ScalaClassType[][] initialAncestors = parents.length > 0
            ? parents[0].getAncestors()
            : EMPTY_ANCESTORS;

        for (int l = 0, dci = 0; l < ancestorDepth; ++l) {
            int toAddParents = 0;
            if (dci < ancestorCode.length && ancestorCode[dci] == l) {
                dci++;
                toAddParents = ancestorCode[dci++];
            }
            int toAddSelf = (l == level) && (!constr.isTrivial) ? 1 : 0;
            int toAdd = toAddParents + toAddSelf;
            ScalaClassType[] initialRow;

            if (l < initialAncestors.length)
                initialRow = initialAncestors[l];
            else
                initialRow = ScalaClassType.EMPTY_ARRAY;

            if (toAdd == 0) {
                ancestors[l] = initialRow;
            } else {
                int initialLen = initialRow.length;
                ScalaClassType[] newRow =
                    new ScalaClassType[initialLen + toAdd];

                if (toAddSelf == 1)
                    newRow[0] = this;

                System.arraycopy(initialRow, 0, newRow, toAddSelf, initialLen);
                for (int i = 0; i < toAddParents; ++i) {
                    int p = ancestorCode[dci++];
                    int o = ancestorCode[dci++];
                    newRow[toAddSelf + initialLen + i] =
                        parents[p].getAncestors()[l][o];
                }
                ancestors[l] = newRow;
            }
        }
    }
}
