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
    private static ScalaClassType[] EMPTY_DISPLAY_ROW =
        new ScalaClassType[0];
    private static ScalaClassType[][] EMPTY_DISPLAY =
        new ScalaClassType[0][];

    private final TypeConstructor constr;
    private final Type[] inst;

    private ScalaClassType[] parents = null;
    private ScalaClassType[][] display = null;

    private final int hashCode;

    public ScalaClassType(TypeConstructor constr, Type[] inst) {
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
    }

    public boolean isInstance0(Object o) {
        return super.isInstance0(o)
            && ((ScalaObject)o).getType().weakIsSubScalaClassType(this);
    }

    protected boolean isSubClassType(ClassType that) {
        return super.isSubClassType(that)
            && (that.isTrivial
                || weakIsSubScalaClassType((ScalaClassType)that));
    }

    private boolean weakIsSubScalaClassType(ScalaClassType that) {
        ScalaClassType parentCT = myInstantiationFor(that);

        // At this stage, if parentCT is null, it means that the
        // constructors had different prefixes, hence we return false.
        return (parentCT != null)
            && (parentCT == that || parentCT.hasSubInstantiation(that));
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
            if (!thisInst[i].isSameAs(thatInst[i]))
                return false;
            ++i;
        }
        // contravariant parameters
        final int firstP = firstM + this.constr.mCount;
        while (i < firstP) {
            if (!thatInst[i].isSubType(thisInst[i]))
                return false;
            ++i;
        }
        // covariant parameters
        final int firstOutside = firstP + this.constr.pCount;
        while (i < firstOutside) {
            if (!thisInst[i].isSubType(thatInst[i]))
                return false;
            ++i;
        }
        return true;
    }

    public boolean isSameAs(Type that) {
        if (super.isSameAs(that)) {
            ScalaClassType thatCT = (ScalaClassType)that;
            ScalaClassType parentCT = myInstantiationFor(thatCT);
            return (parentCT != null)
                && (parentCT.hasSameInstantiation(thatCT));
        } else
            return false;
    }

    private boolean hasSameInstantiation(ScalaClassType that) {
        final Type[] thisInst = this.inst;
        final Type[] thatInst = that.inst;

        for (int i = 0; i < thisInst.length; ++i) {
            if (!thisInst[i].isSameAs(thatInst[i]))
                return false;
        }
        return true;
    }

    private ScalaClassType myInstantiationFor(ScalaClassType that) {
        // Find our instantiation for the other type, if any.
        ScalaClassType[] thisSlice = getDisplay()[that.constr.level];

        for (int i = 0; i < thisSlice.length; ++i) {
            if (thisSlice[i].constr == that.constr)
                return thisSlice[i];
        }

        return null;
    }

    public String toString() {
        StringBuffer buf = new StringBuffer();
        if (constr.outer != null)
            buf.append(constr.outer).append(".");

        int firstM = constr.zCount;
        int firstP = firstM + constr.mCount;
        buf.append(constr).append("[");
        for (int i = 0; i < inst.length; ++i) {
            if (i > 0) buf.append(", ");
            if (i >= firstP)
                buf.append('+');
            else if (i >= firstM)
                buf.append('-');
            buf.append(inst[i]);
        }
        return buf.append("]").toString();
    }

    public int hashCode() {
        return hashCode;
    }

    public ScalaClassType setParents(ScalaClassType[] parents) {
        assert this.parents == null || Type.isSameAs(this.parents, parents);
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
            if (timeout >= 1000)
                throw new Error("computation of parents apparently stuck for "
                                + this);
        }
        return parents;
    }

    private ScalaClassType[][] getDisplay() {
        if (display == null)
            computeDisplay();
        return display;
    }

    private void computeDisplay() {
        final int level = constr.level;
        final int[] displayCode = constr.displayCode;
        ScalaClassType[] parents = getParents();

        display = new ScalaClassType[level + 1][];
        ScalaClassType[][] superClassDisplay = constr.inheritsFromJavaClass
            ? EMPTY_DISPLAY
            : parents[0].getDisplay();

        for (int l = 0, dci = 0; l <= level; ++l) {
            int additionalEntries = displayCode[dci++];
            ScalaClassType[] initialRow;

            if (l < superClassDisplay.length)
                initialRow = superClassDisplay[l];
            else if (l == level)
                initialRow = new ScalaClassType[] { this };
            else
                initialRow = EMPTY_DISPLAY_ROW;

            if (additionalEntries == 0) {
                display[l] = initialRow;
            } else {
                int superLen = initialRow.length;
                ScalaClassType[] newRow =
                    new ScalaClassType[superLen + additionalEntries];

                System.arraycopy(initialRow, 0, newRow, 0, superLen);
                for (int i = 0; i < additionalEntries; ++i) {
                    int p = displayCode[dci++];
                    int o = displayCode[dci++];
                    newRow[superLen + i] = parents[p].getDisplay()[l][o];
                }

                display[l] = newRow;
            }
        }
    }
}
