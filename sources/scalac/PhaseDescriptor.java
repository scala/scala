/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac;

import java.util.*;
import scalac.ast.printer.*;
import scalac.ast.*;
import scalac.symtab.*;
import scalac.checkers.*;
import java.io.PrintWriter;

/**
 * Information about a compiler phase.
 *
 * @author Michel Schinz
 */

public abstract class PhaseDescriptor {

    private static class InitialPhaseDescriptor extends PhaseDescriptor {

        public String name() {
            return "initial";
        }

        public String description() {
            return "initializing compiler";
        }

	/** apply phase to all compilation units
	 */
	public void apply(Global global) {}
    }

    private static class TerminalPhaseDescriptor extends PhaseDescriptor {

        public String name() {
            return "terminal";
        }

        public String description() {
            return "compilation terminated ";
        }

	/** apply phase to all compilation units
	 */
	public void apply(Global global) {}
    }

    public static PhaseDescriptor INITIAL = new InitialPhaseDescriptor();
    public static PhaseDescriptor TERMINAL = new TerminalPhaseDescriptor();

    public static final int SKIP  = 0x0001;
    public static final int CHECK = 0x0002;
    public static final int PRINT = 0x0004;
    public static final int GRAPH = 0x0008;
    public static final int STOP  = 0x0010;
    public static final int LOG   = 0x0020;

    public int flags;
    public int id;

    /** return a short, one-word name for the phase.
     */
    public abstract String name();

    /** return a one-line description for the phase.
     */
    public abstract String description();

    /** a one-line task description of this phase
     */
    public String taskDescription() {
        return description();
    }

    /** initialize the phase
     */
    public final void initialize(Global global) {
        throw new Error();
    }
    public void initialize(Global global, int id) {
        this.id = id;
    }

    /** Assume that `tp' is the info of symbol `sym' before this phase.
     *  Return the info of `sym' after the phase.
     */
    public Type transformInfo(Symbol sym, Type tp) {
	return tp;
    }

    /** apply phase to all compilation units
     */
    public abstract void apply(Global global);

    /** check all compilation units
     */
    public void check(Global global) {
        for (int i = 0; i < global.units.length; i++)
            check(global.units[i]);
    }

    /** print all compilation units
     */
    public void print(Global global) {
        TreePrinter printer = global.printer;

        printer.beginSection(1, "Trees after phase " + name());
        for (int i = 0; i < global.units.length; i++)
            printer.print(global.units[i]);
    }

    /** graph all compilation units
     */
    public void graph(Global global) {
        for (int i = 0; i < global.units.length; i++)
            graph(global.units[i]);
    }

    /** return an array of checkers which can be applied after the phase
     */
    public Checker[] postCheckers(Global global) {
        return new Checker[0];
    }

    /** check the result of this phase for the given compilation unit
     */
    public void check(Unit unit) {
        Checker[] checkers = postCheckers(unit.global);
        for (int i = 0; i < checkers.length; i++)
            checkers[i].traverse(unit);
    }

    /** graph the result of this phase for the given compilation unit
     */
    public void graph(Unit unit) {
	/* todo: uncomment
        new scala.compiler.gdl.TreePrinter().printInFile(
            unit, unit.source + "-" + name() + ".gdl");
	*/
    }

    public String toString() {
        return name();
    }
}
