/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.framework;

import scalac.Global;
import scalac.Phase;

/**
 * This abstract class represents a value promise (a lazy value). The
 * promised value is obtained by invoking the method "force" which
 * will detect any cyclic definition. Note that this class does not
 * store evaluated values. Therefore, successive calls to "force" will
 * trigger successive evaluations of the promise.
 */
public abstract class Promise {

    //########################################################################
    // Private Fields

    /** The current number of evaluation cycles */
    private int cycles;

    //########################################################################
    // Public Methods

    /** Forces this promise and returns its value. */
    public final Object force(Object owner) {
        int cycles = this.cycles;
        this.cycles++;
        Object value = cycles > 0
            ? evaluate(owner)
            : getCyclicValue(owner, cycles);
        this.cycles--;
        return value;
    }

    /** Forces this promise at given phase and returns its value. */
    public final Object forceAt(Object owner, Global global, Phase phase) {
        Phase current = global.currentPhase;
        global.currentPhase = phase;
        Object value = force(owner);
        global.currentPhase = current;
        return value;
    }

    //########################################################################
    // Protected Methods

    /**
     * Evaluates this promise. This method is invoked by non-cyclic
     * calls to the method "force".
     */
    protected abstract Object evaluate(Object owner);

    /**
     * Returns the value to use in case of a cyclic definition. This
     * method is invoked by cyclic calls to the method "force". The
     * current (strictly positive) number of cycles is passed as an
     * argument.
     */
    protected abstract Object getCyclicValue(Object owner, int cycles);

    //########################################################################
}
