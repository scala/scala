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

public class ScalaClassType extends ClassType {
    private static ScalaClassType[] EMPTY_DISPLAY_ROW =
        new ScalaClassType[0];
    private static ScalaClassType[][] EMPTY_DISPLAY =
        new ScalaClassType[0][];

    private final TypeConstructor constr;
    private final Type[] inst;
    private final ScalaClassType[] parents;

    private ScalaClassType[][] display = EMPTY_DISPLAY;

    public ScalaClassType(TypeConstructor constr,
                          Type[] inst,
                          ScalaClassType[] parents) {
        super(constr.clazz, constr.isTrivial);

        this.constr = constr;
        this.inst = inst;
        this.parents = parents;

        computeDisplay();       // TODO do this lazily
    }

    public String toString() {
        StringBuffer buf = new StringBuffer();
        if (constr.outer != null)
            buf.append(constr.outer).append(".");
        buf.append(constr).append("[");
        for (int i = 0; i < inst.length; ++i) {
            if (i > 0) buf.append(", ");
            buf.append(inst[i]);
        }
        return buf.append("]").toString();
    }

    public boolean weakIsSubScalaClassType(ScalaClassType that) {
        // Find our instantiation for the other type, if any.
        ScalaClassType[] thisSlice = display[that.constr.level];
        ScalaClassType parentCT = null;

        for (int i = 0; i < thisSlice.length; ++i) {
            if (thisSlice[i].constr == that.constr) {
                parentCT = thisSlice[i];
                break;
            }
        }

        // At this stage, if parentCT is null, it means that the
        // constructors had different prefixes, hence we return false.

        return (parentCT != null)
            && (parentCT == that || parentCT.hasSubInstantiation(that));
    }

    public boolean isInstance(Object o) {
        return super.isInstance(o)
            && ((ScalaObject)o).getType().weakIsSubScalaClassType(this);
    }

    public boolean isSubType(ClassType that) {
        return that.isTrivial || weakIsSubScalaClassType((ScalaClassType)that);
    }

    public boolean isSubType(Type that) {
        return super.isSubType(that) && isSubType((ClassType)that);
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
            if (thisInst[i] != thatInst[i])
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

    private void computeDisplay() {
        final int level = constr.level;
        final int[] displayCode = constr.displayCode;

        display = new ScalaClassType[level + 1][];
        ScalaClassType[][] superClassDisplay =
            parents.length > 0 ? parents[0].display : EMPTY_DISPLAY;

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
                    newRow[superLen + i] = parents[p].display[l][o];
                }

                display[l] = newRow;
            }
        }
    }
}
