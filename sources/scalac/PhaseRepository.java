/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac;

import scalac.ast.parser.ParserPhase;
import scalac.typechecker.AnalyzerPhase;
import scalac.typechecker.RefCheckPhase;
import scalac.transformer.UnCurryPhase;
import scalac.transformer.TransMatchPhase;
import scalac.transformer.LambdaLiftPhase;
import scalac.transformer.ExplicitOuterClassesPhase;
import scalac.transformer.AddAccessorsPhase;
import scalac.transformer.AddInterfacesPhase;
import scalac.transformer.ExpandMixinsPhase;
import scalac.transformer.ErasurePhase;
import scalac.transformer.AddConstructorsPhase;
/*
import scalac.optimizer.OptimizePhase;
*/
import scalac.backend.jvm.GenJVMPhase;
import scalac.backend.msil.GenMSILPhase;

public class PhaseRepository {

    //########################################################################
    // Private state

    public final PhaseDescriptor[] phases;

    //########################################################################
    // Reporter constructors

    public PhaseRepository() {
        this.phases = new PhaseDescriptor[] {
            INITIAL = PhaseDescriptor.INITIAL,
            PARSER = new ParserPhase(),
            ANALYZER = new AnalyzerPhase(),
            REFCHECK = new RefCheckPhase(),
            UNCURRY = new UnCurryPhase(),
	    /*
            OPTIMIZE = new OptimizePhase(),
            */
            TRANSMATCH = new TransMatchPhase(),
            LAMBDALIFT = new LambdaLiftPhase(),
            EXPLICITOUTER = new ExplicitOuterClassesPhase(),
            ADDACCESSORS = new AddAccessorsPhase(),
            ADDINTERFACES = new AddInterfacesPhase(),
            EXPANDMIXIN = new ExpandMixinsPhase(),
            ERASURE = new ErasurePhase(),
            ADDCONSTRUCTORS = new AddConstructorsPhase(),
            GENJVM = new GenJVMPhase(),
            GENMSIL = new GenMSILPhase(),
            TERMINAL = PhaseDescriptor.TERMINAL,
        };
    }

    //########################################################################
    // Reporter interface

    public final PhaseDescriptor INITIAL;
    public final ParserPhase PARSER;
    public final AnalyzerPhase ANALYZER;
    public final RefCheckPhase REFCHECK;
    public final UnCurryPhase UNCURRY;
    /*
    public final OptimizePhase OPTIMIZE;
    */
    public final TransMatchPhase TRANSMATCH;
    public final LambdaLiftPhase LAMBDALIFT;
    public final ExplicitOuterClassesPhase EXPLICITOUTER;
    public final AddAccessorsPhase ADDACCESSORS;
    public final AddInterfacesPhase ADDINTERFACES;
    public final ExpandMixinsPhase EXPANDMIXIN;
    public final ErasurePhase ERASURE;
    public final AddConstructorsPhase ADDCONSTRUCTORS;
    public final GenJVMPhase GENJVM;
    public final GenMSILPhase GENMSIL;
    public final PhaseDescriptor TERMINAL;

    //########################################################################
}
