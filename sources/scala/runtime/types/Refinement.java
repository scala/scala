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

/**
 * Refinement for a class member.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class Refinement {
    public final static Refinement[] EMPTY_ARRAY = new Refinement[0];

    public final int hash;
    public final Type type;

    public Refinement(int hash, Type type) {
        this.hash = hash;
        this.type = type;
    }

    public boolean isSameAs(Refinement that) {
        return (this.hash == that.hash) && this.type.isSameAs(that.type);
    }

    public boolean isFinerThan(Refinement that) {
        return (this.hash == that.hash) && this.type.isSubType(that.type);
    }

    public static boolean isFiner(Refinement[] r1, Refinement[] r2) {
        for (int i2 = 0, i1 = 0; i2 < r2.length; ++i2) {
            Refinement r = r2[i2];
            while (i1 < r1.length && r1[i1].hash != r.hash)
                ++i1;

            if (i1 == r1.length || !r1[i1].isFinerThan(r))
                return false;
        }
        return true;
    }

    public static Refinement[] make(ScalaClassType[] parents,
                                    Refinement[] base,
                                    int[] code) {
        int pc = 0;
        int len = code[pc++];
        Refinement[] result = new Refinement[len];
        for (int i = 0; i < len; ++i) {
            int par = code[pc++], idx = code[pc++];
            result[i] = (par == -1)
                ? base[idx]
                : parents[par].refinements[idx];
        }
        assert pc == code.length;
        return result;
    }
}
