/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac;

import java.util.ArrayList;
import java.util.List;

/**
 * This class defines all compiler phases and maintains a list of
 * active phases.
 */
public class CompilerPhases {

    //########################################################################
    // Public Fields

    /** The compiler phases. */
    public final PhaseDescriptor INITIAL;
    public final PhaseDescriptor PARSER;
    public final PhaseDescriptor ANALYZER;
    public final PhaseDescriptor REFCHECK;
    public final PhaseDescriptor UNCURRY;
    // public final PhaseDescriptor OPTIMIZE;
    public final PhaseDescriptor TRANSMATCH;
    public final PhaseDescriptor LAMBDALIFT;
    public final PhaseDescriptor EXPLICITOUTER;
    public final PhaseDescriptor ADDACCESSORS;
    public final PhaseDescriptor TAILCALL;
    public final PhaseDescriptor ADDINTERFACES;
    public final PhaseDescriptor EXPANDMIXIN;
    public final PhaseDescriptor ADDCONSTRUCTORS;
    public final PhaseDescriptor ERASURE;
    public final PhaseDescriptor GENMSIL;
    public final PhaseDescriptor GENJVM;
    public final PhaseDescriptor GENJVM_BCEL;
    public final PhaseDescriptor TERMINAL;

    //########################################################################
    // Private Fields

    /** The list containing the active phases */
    private final List phases;

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public CompilerPhases() {
        this.phases = new ArrayList();
        PhaseDescriptor[] array = {
            this.INITIAL = new PhaseDescriptor(
                "initial",
                "initializing compiler",
                "initializing compiler",
                scalac.util.EmptyPhase.class),
            this.PARSER = new PhaseDescriptor(
                "parse",
                "parse source files",
                "parsed",
                scalac.ast.parser.ParserPhase.class),
            this.ANALYZER = new PhaseDescriptor(
                "analyze",
                "name and type analysis",
                "type checking",
                scalac.typechecker.AnalyzerPhase.class),
            this.REFCHECK = new PhaseDescriptor(
                "refcheck",
                "reference checking",
                "reference checking",
                scalac.typechecker.RefCheckPhase.class),
            this.UNCURRY = new PhaseDescriptor(
                "uncurry",
                "uncurry function types and applications",
                "uncurried",
                scalac.transformer.UnCurryPhase.class),
//             this.OPTIMIZE = new PhaseDescriptor(
//                 "optimize",
//                 "tree-optimizer",
//                 "tree optimization",
//                 scalac.optimizer.OptimizePhase.class),
            this.TRANSMATCH = new PhaseDescriptor(
                "transmatch",
                "translate match expressions",
                "translated pattern matching",
                scalac.transformer.TransMatchPhase.class),
            this.LAMBDALIFT = new PhaseDescriptor(
                "lambdalift",
                "lambda lifter",
                "lambda lifting",
                scalac.transformer.LambdaLiftPhase.class),
            this.EXPLICITOUTER = new PhaseDescriptor(
                "explicitouterclasses",
                "make links from inner classes to enclosing one explicit",
                "made outer links explicit",
                scalac.transformer.ExplicitOuterClassesPhase.class),
            this.ADDACCESSORS = new PhaseDescriptor(
                "addaccessors",
                "add accessors for constructor arguments",
                "added accessors",
                scalac.transformer.AddAccessorsPhase.class),
            this.TAILCALL = new PhaseDescriptor(
                "tailcall",
                "add tail-calls",
                "added tail-calls",
                scalac.transformer.TailCallPhase.class),
            this.ADDINTERFACES = new PhaseDescriptor(
                "addinterfaces",
                "add one interface per class",
                "added interfaces",
                scalac.transformer.AddInterfacesPhase.class),
            this.EXPANDMIXIN = new PhaseDescriptor(
                "expandmixins",
                "expand mixins by code copying",
                "expanded mixins",
                scalac.transformer.ExpandMixinsPhase.class),
            this.ADDCONSTRUCTORS = new PhaseDescriptor(
                "addconstructors",
                "add explicit constructor for each class",
                "added constructors",
                scalac.transformer.AddConstructorsPhase.class),
            this.ERASURE = new PhaseDescriptor(
                "erasure",
                "type eraser",
                "erased types",
                scalac.transformer.ErasurePhase.class),
            this.GENMSIL = new PhaseDescriptor(
                "genmsil",
                "generate MSIL code",
                "generated MSIL code",
                scalac.backend.msil.GenMSILPhase.class),
            this.GENJVM = new PhaseDescriptor(
                "genjvm",
                "generate JVM bytecodes",
                "generated JVM code",
                scalac.backend.jvm.GenJVMPhase.class),
            this.GENJVM_BCEL = new PhaseDescriptor(
                "genjvm-bcel",
                "generate JVM bytecodes",
                "generated JVM code",
                scalac.backend.jvm.GenJVMBCELPhase.class),
            this.TERMINAL = new PhaseDescriptor(
                "terminal",
                "compilation terminated",
                "compilation terminated",
                scalac.util.EmptyPhase.class),
        };
        for (int i = 0; i < array.length; i++) phases.add(array[i]);
    }

    //########################################################################
    // Public Methods

    /** Returns an array containing all active phases. */
    public PhaseDescriptor[] phases() {
        PhaseDescriptor[] array = new PhaseDescriptor[phases.size()];
        phases.toArray(array);
        return array;
    }

    /** Freezes all active phases. */
    public void freeze() {
        PhaseDescriptor[] phases = phases();
        PhaseDescriptor.freeze(phases);
    }

    /** Activates phase "phase" by placing it before phase "where". */
    public int insertBefore(PhaseDescriptor phase, PhaseDescriptor where) {
        int index = phases.indexOf(where);
        assert index >= 0 : "could not find phase " + where;
        phases.add(index, phase);
        return index;
    }

    /** Activates phase "phase" by placing it after phase "where". */
    public int insertAfter(PhaseDescriptor phase, PhaseDescriptor where) {
        int index = phases.indexOf(where);
        assert index >= 0 : "could not find phase " + where;
        phases.add(index + 1, phase);
        return index + 1;
    }

    //########################################################################
}
