/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac;

import scalac.ast.printer.TreePrinter;
import scalac.symtab.Symbol;
import scalac.symtab.Type;
import scalac.checkers.Checker;

public abstract class Phase {

    // !!! remove these obsolete methods !
    public final String name()  {
        throw new Error("!!! obsolete");
    }
    public final String taskDescription() {
        throw new Error("!!! obsolete");
    }
    public final String description()  {
        throw new Error("!!! obsolete");
    }
    public final void initialize(Global global) {
        throw new Error("!!! obsolete");
    }
    public final void initialize(Global global, int id) {
        throw new Error("!!! obsolete");
    }
    public final void apply(Global global) {
        throw new Error("!!! obsolete");
    }
    public final void apply(Unit unit) {
        throw new Error("!!! obsolete");
    }

    //########################################################################
    // Public Fields

    /** The global environment */
    public final Global global;

    /** The phase descriptor */
    public final PhaseDescriptor descriptor;

    /** The phase identifier */
    public final int id;

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public Phase(Global global, PhaseDescriptor descriptor) {
        this.global = global;
        this.descriptor = descriptor;
        this.id = descriptor.id();
        global.currentPhase = global.phases[id] = this;
    }

    //########################################################################
    // Public Methods

    /**
     * Returns the info of `sym' after the phase. Assumes that `tp' is
     * the info of symbol `sym' before this phase.
     */
    public Type transformInfo(Symbol sym, Type tp) {
        return tp;
    }

    /** Applies this phase to the given compilation units. */
    public abstract void apply(Unit[] units);

    /** Prints all compilation units. */
    public void print(Global global) {
        TreePrinter printer = global.printer;
        printer.beginSection(1, "Trees after phase " + this);
        for (int i = 0; i < global.units.length; i++)
            printer.print(global.units[i]);
    }

    /** Graphs all compilation units. */
    public void graph(Global global) {
        for (int i = 0; i < global.units.length; i++) graph(global.units[i]);
    }

    /** Graphs the result of this phase for the given compilation unit. */
    public void graph(Unit unit) {
        // !!! new scala.compiler.gdl.TreePrinter().printInFile(
        // !!!     unit, unit.source + "-" + name() + ".gdl");
    }

    /** Checks all compilation units. */
    public void check(Global global) {
        for (int i = 0; i < global.units.length; i++) check(global.units[i]);
    }

    /** Check the result of this phase for the given compilation unit. */
    public void check(Unit unit) {
        Checker[] checkers = postCheckers(unit.global);
        for (int i = 0; i < checkers.length; i++) checkers[i].traverse(unit);
    }

    /** Returns an array of checkers which can be applied after the phase. */
    public Checker[] postCheckers(Global global) {
        return new Checker[0];
    }

    /** Returns the name of this phase. */
    public final String toString() {
        return descriptor.name();
    }

    //########################################################################
}
