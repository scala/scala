/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.framework;

import scalac.Global;
import scalac.Phase;
import scalac.util.Debug;

/**
 * This class implements a value history. It stores phase dependent
 * values.
 *
 * Implementation: the value "values[n]" is valid from start of phase
 * "starts[n]" to end of phase "(n == 0 ? limit : starts[n-1].prev)"
 * and the promised value "next" is valid from start of phase
 * "limit.next".
 */
public class History {

    //########################################################################
    // Private Constants

    /** An empty array of objects */
    private static final Object[] NO_VALUES = new Object[0];

    /** An empty array of phases */
    private static final Phase[] NO_STARTS = new Phase[0];

    //########################################################################
    // Private Fields

    /** The global environment */
    private final Global global;

    /** The successive values of this history */
    private Object[] values;

    /** The start phases of the successive values */
    private Phase[] starts;

    /** The last known valid phase of the last value */
    private Phase limit;

    /** A promise of the next value (may be null) */
    private Promise next;

    //########################################################################
    // Public Constructors

    /** Initializes this instance with current phase as first one. */
    public History() {
        this(Global.instance);
    }

    /** Initializes this instance with current phase as first one. */
    public History(Global global) {
        this(global, global.currentPhase);
    }

    /** Initializes this instance with given first phase. */
    public History(Phase first) {
        this(first.global, first);
    }

    /** Initializes this instance with given first phase. */
    public History(Global global, Phase first) {
        this.global = global;
        reset(first);
    }

    //########################################################################
    // Public Methods

    /** Returns the first phase of this history. */
    public final Phase getFirstPhase() {
        return starts.length > 0 ? starts[starts.length - 1] : limit.next;
    }

    /** Returns the value at current phase. */
    public final Object getValue(Object owner) {
        return getValueAt(owner, getPhase());
    }

    /** Returns the value at next phase. */
    public final Object getNextValue(Object owner) {
        return getValueAt(owner, getNextPhase());
    }

    /** Returns the value at given phase. */
    public final Object getValueAt(Object owner, Phase phase) {
        assert phase != null: show(owner, phase);
        while (limit.id < phase.id) incrementLimit(owner);
        for (int i = 0; i < starts.length; i++)
            if (starts[i].id <= phase.id) return values[i];
        throw Debug.abort("prehistoric phase", show(owner, phase));
    }

    /** Sets the value at current phase. */
    public final History setValue(Object owner, Object value) {
        return setValueAt(owner, getPhase(), value);
    }

    /** Sets the value at next phase. */
    public final History setNextValue(Object owner, Object value) {
        return setValueAt(owner, getNextPhase(), value);
    }

    /** Sets the value at given phase. */
    public final History setValueAt(Object owner, Phase phase, Object value) {
        assert phase != null: show(owner, phase);
        assert phase == limit.next && next == null: show(owner, phase);
        if (values.length == 0 || values[0] != value) {
            this.values = append(value, values);
            this.starts = append(phase, starts);
        }
        this.limit = phase;
        return this;
    }

    /** Sets a value promise at current phase. */
    public final History setPromise(Object owner, Promise promise) {
        return setPromiseAt(owner, getPhase(), promise);
    }

    /** Sets a value promise at next phase. */
    public final History setNextPromise(Object owner, Promise promise) {
        return setPromiseAt(owner, getNextPhase(), promise);
    }

    /** Sets a value promise at given phase. */
    public final History setPromiseAt(Object owner, Phase phase,
        Promise promise)
    {
        assert phase != null: show(owner, phase);
        assert phase == limit.next && next == null: show(owner, phase);
        this.next = promise;
        return this;
    }

    /** Erases all values. */
    public final void reset() {
        reset(getFirstPhase());
    }

    /** Erases all values and sets first phase to given one. */
    public final void reset(Phase first) {
        assert first != null && first.prev != null: this + " - " + first;
        this.values = NO_VALUES;
        this.starts = NO_STARTS;
        this.limit = first.prev;
        this.next = null;
    }

    /** Returns a string representation of this history. */
    public String toString() {
        StringBuffer buffer = new StringBuffer("[");
        for (int i = 0; i < values.length; i++) {
            buffer.append(starts[i]).append(" -> ");
            buffer.append(Debug.show(values[i])).append(", ");
        }
        buffer.append(limit);
        if (next != null) buffer.append(" - ").append(next);
        return buffer.append("]").toString();
    }

    //########################################################################
    // Protected Methods

    /**
     * Computes the value at phase following given one by transforming
     * the given value (which is the value at given phase). The
     * default implementation changes the current phase to given one
     * and then forwards the call to method "transformValue".
     */
    protected Object transformValueAt(Object owner, Phase phase, Object value){
        Phase current = global.currentPhase;
        global.currentPhase = phase;
        Object result = transformValue(owner, value);
        global.currentPhase = current;
        return result;
    }

    /**
     * Computes the value at next phase by transforming the given
     * value (which is the value at current phase). The default
     * implementation forwards the call to method "computeValueAt".
     */
    protected Object transformValue(Object owner, Object value) {
        return computeValueAt(owner, global.currentPhase.next);
    }

    /**
     * Computes the value at given phase. The default implementation
     * changes the current phase to given one and then forwards the
     * call to method "computeValue".
     */
    protected Object computeValueAt(Object owner, Phase phase) {
        Phase current = global.currentPhase;
        global.currentPhase = phase;
        Object result = computeValue(owner);
        global.currentPhase = current;
        return result;
    }

    /**
     * Computes the value at current phase. The default implementation
     * throws an exception.
     */
    protected Object computeValue(Object owner) {
        throw Debug.abort("undefined value", show(owner, getPhase()));
    }

    //########################################################################
    // Private Methods

    /** Returns the current phase. */
    private Phase getPhase() {
        return global.currentPhase;
    }

    /** Returns the next phase. */
    private Phase getNextPhase() {
        return getPhase().next;
    }

    /** Increments the limit of this history. */
    private void incrementLimit(Object owner) {
        Phase phase = limit;
        Object value;
        if (next != null) {
            value = next.forceAt(owner, global, phase.next);
            if (limit == phase) next = null;
        } else if (values.length > 0) {
            value = transformValueAt(owner, phase, values[0]);
        } else {
            value = computeValueAt(owner, phase.next);
        }
        if (limit == phase) setValueAt(owner, phase.next, value);
    }

    /** Returns a string of this history and given owner. */
    private String show(Object owner) {
        return this + " @ " + Debug.show(owner);
    }

    /** Returns a string of this history and given owner and phase. */
    private String show(Object owner, Phase phase) {
        return show(owner) + " - " + phase;
    }

    //########################################################################
    // Private Functions

    /** Returns the concatenation of given values. */
    private static Object[] append(Object value, Object[] values) {
        Object[] array = new Object[1 + values.length];
        array[0] = value;
        for (int i = 1; i < array.length; i++) array[i] = values[i - 1];
        return array;
    }

    /** Returns the concatenation of given phases. */
    private static Phase[] append(Phase phase, Phase[] phases) {
        Phase[] array = new Phase[1 + phases.length];
        array[0] = phase;
        for (int i = 1; i < array.length; i++) array[i] = phases[i - 1];
        return array;
    }

    //########################################################################
}
