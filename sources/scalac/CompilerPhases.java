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
public abstract class CompilerPhases {

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
    public final PhaseDescriptor ADDACCESSORS;
    public final PhaseDescriptor EXPLICITOUTER;
    public final PhaseDescriptor ADDCONSTRUCTORS;
    public final PhaseDescriptor TYPESASVALUES;
    public final PhaseDescriptor TAILCALL;
    public final PhaseDescriptor WHOLEPROG;
    public final PhaseDescriptor ADDINTERFACES;
    public final PhaseDescriptor EXPANDMIXIN;
    public final PhaseDescriptor MAKEBOXINGEXPLICIT;
    public final PhaseDescriptor ERASURE;
    public final PhaseDescriptor ICODE;
    public final PhaseDescriptor TERMINAL;

    //########################################################################
    // Private Fields

    /** The list containing the active phases */
    private final List phases;

    /** Phase names, can be overridden to install .
     */
    protected abstract Class PARSER_PHASE();
    protected abstract Class ANALYZER_PHASE();
    protected abstract Class REFCHECK_PHASE();
    protected abstract Class UNCURRY_PHASE();
    protected abstract Class TRANSMATCH_PHASE();
    protected Class LAMBDALIFT_PHASE() { return scalac.transformer.LambdaLiftPhase.class; }
    protected Class EXPLICITOUTER_PHASE() { return scalac.transformer.ExplicitOuterClassesPhase.class; }
    protected Class ADDACCESSORS_PHASE() { return scalac.transformer.AddAccessorsPhase.class; }
    protected Class ADDCONSTRUCTORS_PHASE() { return scalac.transformer.AddConstructorsPhase.class; }
    protected Class TYPESASVALUES_PHASE() { return scalac.transformer.TypesAsValuesPhase.class; }
    protected Class TAILCALL_PHASE() { return scalac.transformer.TailCallPhase.class; }
    protected abstract Class WHOLEPROG_PHASE();
    protected Class ADDINTERFACES_PHASE() { return scalac.transformer.AddInterfacesPhase.class; }
    protected Class EXPANDMIXIN_PHASE() { return scalac.transformer.ExpandMixinsPhase.class; }
    protected Class ERASURE_PHASE() { return scalac.transformer.ErasurePhase.class; }
    protected abstract Class ICODE_PHASE();

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
                PARSER_PHASE()),
            this.ANALYZER = new PhaseDescriptor(
                "analyze",
                "name and type analysis",
                "type checking",
                ANALYZER_PHASE()),
            this.REFCHECK = new PhaseDescriptor(
                "refcheck",
                "reference checking",
                "reference checking",
                REFCHECK_PHASE()),
            this.UNCURRY = new PhaseDescriptor(
                "uncurry",
                "uncurry function types and applications",
                "uncurried",
                UNCURRY_PHASE()),
//             this.OPTIMIZE = new PhaseDescriptor(
//                 "optimize",
//                 "tree-optimizer",
//                 "tree optimization",
//                 scalac.optimizer.OptimizePhase.class),
            this.TRANSMATCH = new PhaseDescriptor(
                "transmatch",
                "translate match expressions",
                "translated pattern matching",
                TRANSMATCH_PHASE()),
            this.LAMBDALIFT = new PhaseDescriptor(
                "lambdalift",
                "lambda lifter",
                "lambda lifting",
                LAMBDALIFT_PHASE()),
            this.TYPESASVALUES = new PhaseDescriptor(
                "typesasvalues",
                "represent types as values",
                "represented types as values",
                TYPESASVALUES_PHASE()),
            this.ADDACCESSORS = new PhaseDescriptor(
                "addaccessors",
                "add accessors for constructor arguments",
                "added accessors",
                ADDACCESSORS_PHASE()),
            this.EXPLICITOUTER = new PhaseDescriptor(
                "explicitouterclasses",
                "make links from inner classes to enclosing one explicit",
                "made outer links explicit",
                EXPLICITOUTER_PHASE()),
            this.ADDCONSTRUCTORS = new PhaseDescriptor(
                "addconstructors",
                "add explicit constructor for each class",
                "added constructors",
                ADDCONSTRUCTORS_PHASE()),
            this.TAILCALL = new PhaseDescriptor(
                "tailcall",
                "add tail-calls",
                "added tail-calls",
                TAILCALL_PHASE()),
            this.WHOLEPROG = new PhaseDescriptor(
                "wholeprog",
                "perform whole program analysis",
                "find monomorphic callsites and performs inlining",
                WHOLEPROG_PHASE()),
            this.ADDINTERFACES = new PhaseDescriptor(
                "addinterfaces",
                "add one interface per class",
                "added interfaces",
                ADDINTERFACES_PHASE()),
            this.EXPANDMIXIN = new PhaseDescriptor(
                "expandmixins",
                "expand mixins by code copying",
                "expanded mixins",
                EXPANDMIXIN_PHASE()),
            this.MAKEBOXINGEXPLICIT = new PhaseDescriptor(
                "boxing",
                "makes boxing explicit",
                "made boxing explicit",
                scalac.transformer.MakeBoxingExplicitPhase.class),
            this.ERASURE = new PhaseDescriptor(
                "erasure",
                "type eraser",
                "erased types",
                ERASURE_PHASE()),
            this.ICODE = new PhaseDescriptor(
                "icode",
                "generate icode",
                "generated icode",
                ICODE_PHASE()),
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
