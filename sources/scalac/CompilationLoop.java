/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac;

import java.util.ArrayList;

import scalac.util.Debug;

/** This class implements the scalac compilation loop. */
public class CompilationLoop {

    //########################################################################
    // Private Fields

    /** The global environment */
    private final Global global;
    /** The list of active phases */
    private final Phase[] phases;
    /** The units currently associated to each active phase. */
    private final ArrayList/*<CompilationUnit>*/[] unitss;
    /** The indexes of the first units that are not yet processed. */
    private final int[] indexes;

    //########################################################################
    // Private Constructors

    /** Initializes this instance. */
    public CompilationLoop(Global global) {
        this.global = global;
        int count = 0;
        for (Phase p = global.PHASE.INITIAL.phase(); p != null; p = p.next)
            count++;
        this.phases = new Phase[count];
        this.unitss = new ArrayList[count];
        this.indexes = new int[count];
        Phase phase = global.PHASE.INITIAL.phase();
        for (int i = 0; i < count; i++) {
            phases[i] = phase;
            unitss[i] = new ArrayList();
            phase = phase.next;
        }
    }

    //########################################################################
    // Public Methods

    /**
     * Compiles the given units and returns list of all compiled units
     * including those loaded automatically.
     */
    public CompilationUnit[] compile(CompilationUnit[] units) {
        for (int i = 0; i < units.length; i++) unitss[0].add(units[i]);
        int limit = phases.length - 1;
        loop(limit, true);
        return global.reporter.errors() != 0
            ? new CompilationUnit[0]
            : (CompilationUnit[])unitss[limit].toArray(
                new CompilationUnit[unitss[limit].size()]);
    }

    /**
     * Inserts the given unit and compiles it up to the current phase
     * excluded.
     */
    public void insert(CompilationUnit unit) {
        Phase backup = global.currentPhase;
        unitss[0].add(unit);
        // !!! This "false" may be dangerous; it may lead to crashes
        // because phases might work on trees/symbol-tables that
        // contain errors (however this occurs only if the newly
        // loaded code may contain errors). On the other hand "true",
        // might starve some phases; if a phase requests a compilation
        // unit, it may never receive it.
        loop(getPhaseIndex(backup), false);
        global.currentPhase = backup;
    }

    //########################################################################
    // Private Methods

    /** Compiles all units up to phase "phases[limit]" excluded. */
    private void loop(int limit, boolean main) {
        assert limit < phases.length: Debug.show(""+limit, phases);
        for (int current = getFirstNonEmptyIndex(); current < limit; ) {
            // !!! remove NAMER test ?
            if (main && global.reporter.errors() != 0
                && global.currentPhase != global.PHASE.NAMER.phase()) break;
            // Update the current phase pointer.
            Phase phase = global.currentPhase = phases[current];
            // If the phase is not yet started, start it.
            if (indexes[current] == 0) global.timer.start();
            // Apply the phase to all available units. It's important
            // to not cache the result of "size()" as new units may be
            // added during the loop.
            for (int i = indexes[current]; i < unitss[current].size(); i++)
                phase.apply((CompilationUnit)unitss[current].get(i));
            int next = getFirstNonEmptyIndex();
            // If no new units were introduced, stop the phase.
            if (next == current) {
                PhaseDescriptor descriptor = phase.descriptor;
                global.timer.stop(descriptor.taskDescription());
                CompilationUnit[] units =
                    (CompilationUnit[])unitss[current].toArray(
                        new CompilationUnit[unitss[current].size()]);
                if (descriptor.hasPrintFlag()) global.print(units);
                // if (descriptor.hasGraphFlag()); // !!!
                // if (descriptor.hasCheckFlag()); // !!!
                if (phase == global.PHASE.PARSER.phase()) global.fix1(units);
                if (phase == global.PHASE.ANALYZER.phase()) global.fix2(units);
                unitss[current + 1].addAll(unitss[current]);
                unitss[current].clear();
                next++;
            }
            indexes[current] = unitss[current].size();
            current = next;
        }
    }

    /**
     * Returns the first index with a non-empty unit list. If there is
     * no such index, returns "phases.length".
     */
    private int getFirstNonEmptyIndex() {
        int index = 0;
        while (index < phases.length && unitss[index].isEmpty()) index++;
        return index;
    }

    /** Returns the index of the given phase. */
    private int getPhaseIndex(Phase phase) {
        for (int index = 0; index < phases.length; index++)
            if (phases[index] == phase) return index;
        throw Debug.abort("phase not found", Debug.show(phase, phases));
    }

    //########################################################################
}
