/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import scalac.util.Debug;

/** Information about a compiler phase. */
public final class PhaseDescriptor {

    //########################################################################
    // Public Constants

    public static final int STOP  = 0x0001;
    public static final int SKIP  = 0x0002;
    public static final int PRINT = 0x0004;
    public static final int GRAPH = 0x0008;
    public static final int CHECK = 0x0010;
    public static final int LOG   = 0x0020;

    //########################################################################
    // Public Functions

    /** Freezes the given phases (patches flags and assigns ids). */
    public static void freeze(PhaseDescriptor[] phases) {
        // there are at least two phases: INITIAL and TERMINAL
        assert phases.length >= 2;
        if (phases.length == 0) return;
        // propagate STOP and SKIP
        for (int i = 0; i < phases.length - 1; i++) {
            phases[i].flags |= (phases[i + 1].flags >>> 16) & (STOP | SKIP);
            phases[i+1].flags &= ~((STOP | SKIP) << 16);
        }
        // transform STOP flags into SKIP flags
        boolean stop = false;
        for (int i = 0; i < phases.length; i++) {
            if (stop) phases[i].flags |= SKIP;
            stop |= phases[i].hasStopFlag();
            if (stop) phases[i].flags &= ~STOP;
        }
        // remove SKIP flag on INITIAL and TERMINAL phases
        phases[0].flags &= ~SKIP;
        phases[phases.length - 1].flags &= ~SKIP;
        // propagate other flags and freeze remaining phases
        PhaseDescriptor last = null;
        for (int i = 0; i < phases.length; i++) {
            phases[i].id = i;
            if (phases[i].hasSkipFlag()) continue;
            if (last != null) last.flags |= phases[i].flags >>> 16;
            phases[i].flags &= 0x0000FFFF;
            last = phases[i];
        }
    }

    //########################################################################
    // Private Fields

    /** The phase name */
    private final String name;
    /** The phase description */
    private final String description;
    /** The phase task description */
    private final String task;
    /** The phase implementation class */
    private final Class clasz;

    /** The flags of this phase */
    private int flags;
    /** The phase instance */
    private Phase phase;
    /** The phase identifier */
    private int id = -1;

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public PhaseDescriptor(String name, String description, String task,
        Class clasz)
    {
        this.name = name;
        this.description = description;
        this.task = task;
        this.clasz = clasz;
    }

    //########################################################################
    // Public Methods

    /** Returns the one-word name of this phase. */
    public String name() {
        return name;
    }

    /** Returns a one-line description of this phase. */
    public String description() {
        return description;
    }

    /** Returns a one-line task description of this phase. */
    public String taskDescription() {
        return task;
    }

    /** Creates the object implementing this phase. */
    public Phase create(Global global) {
        assert id >= 0 : "phase " + name + " not yet frozen";
        assert phase == null : "phase " + name + " already created";
        try {
            Class[] params = { Global.class, PhaseDescriptor.class };
            Object[] args = { global, this };
            Constructor constructor = clasz.getConstructor(params);
            return phase = (Phase)constructor.newInstance(args);
        } catch (NoSuchMethodException exception) {
            throw Debug.abort(exception);
        } catch (IllegalAccessException exception) {
            throw Debug.abort(exception);
        } catch (InstantiationException exception) {
            throw Debug.abort(exception);
        } catch (InvocationTargetException exception) {
            throw Debug.abort(exception);
        }
    }

    /** Returns the object implementing this phase. */
    public Phase phase() {
        assert phase != null : "phase " + name + " not yet created";
        return phase;
    }

    /** Returns the identifier of this phase. */
    public int id() {
        assert id >= 0 : "phase " + name + " not yet frozen";
        return id;
    }

    /** Adds the given flag (to previous phase if prev is true). */
    public void addFlag(int flag, boolean prev) {
        assert id < 0 : "phase " + name + " already frozen";
        flags |= flag << (prev ? 16 : 0);
    }

    /** Adds the SKIP flag. */
    public void addSkipFlag() {
        flags |= SKIP;
    }

    /** Adds the CHECK flag. */
    public void addCheckFlag() {
        flags |= CHECK;
    }

    /** Adds the PRINT flag. */
    public void addPrintFlag() {
        flags |= PRINT;
    }

    /** Adds the GRAPH flag. */
    public void addGraphFlag() {
        flags |= GRAPH;
    }

    /** Adds the STOP flag. */
    public void addStopFlag() {
        flags |= STOP;
    }

    /** Adds the LOG flag. */
    public void addLogFlag() {
        flags |= LOG;
    }

    /** Has this phase the SKIP flag? */
    public boolean hasSkipFlag() {
        return (flags & SKIP) != 0;
    }

    /** Has this phase the CHECK flag? */
    public boolean hasCheckFlag() {
        return (flags & CHECK) != 0;
    }

    /** Has this phase the PRINT flag? */
    public boolean hasPrintFlag() {
        return (flags & PRINT) != 0;
    }

    /** Has this phase the GRAPH flag? */
    public boolean hasGraphFlag() {
        return (flags & GRAPH) != 0;
    }

    /** Has this phase the STOP flag? */
    public boolean hasStopFlag() {
        return (flags & STOP) != 0;
    }

    /** Has this phase the LOG flag? */
    public boolean hasLogFlag() {
        return (flags & LOG) != 0;
    }

    /** Returns the name of this phase. */
    public String toString() {
        return name();
    }

    //########################################################################
}
