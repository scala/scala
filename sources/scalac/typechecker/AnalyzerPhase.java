/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.typechecker;

import ch.epfl.lamp.util.Position;
import scalac.*;
import scalac.util.*;
import scalac.ast.*;
import scalac.symtab.*;
import scalac.checkers.*;
import java.util.HashMap;
import java.util.ArrayList;

public class AnalyzerPhase extends Phase {

    final Context startContext;
    final Context consoleContext;
    final HashMap/*<Unit,Context>*/ contexts = new HashMap();
    final ArrayList/*<Unit>*/ newSources = new ArrayList();

    /** Initializes this instance. */
    public AnalyzerPhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);
        Definitions definitions = global.definitions;
        this.startContext = new Context(
            Tree.Empty,
            definitions.ROOT_CLASS,
            definitions.ROOT_CLASS.members(),
            Context.NONE);
        this.startContext.enclClass = this.startContext;

        if (!global.noimports) {
            addImport(startContext, definitions.getModule(Names.java_lang));
            addImport(startContext, definitions.getModule(Names.scala));
        }

        if (!global.noimports && !global.nopredefs) {
            addImport(startContext, definitions.getModule(Names.scala_Predef));
        }

        this.consoleContext = new Context(
            Tree.Empty,
            definitions.ROOT_CLASS,
            definitions.ROOT_CLASS.members(),
            startContext);
    }

    public void addConsoleImport(Symbol module) {
        addImport(consoleContext, module);
    }

    private void addImport(Context context, Symbol module) {
        global.prevPhase();
        Tree tree = global.treeGen.mkImportAll(Position.NOPOS, module);
        global.nextPhase();
        context.imports = new ImportList(tree, new Scope(), context.imports);
    }

    public void apply(Unit[] units) {
        new Analyzer(global, this).apply(units);
    }

    public void lateEnter(Global global, Unit unit, Symbol symbol) {
        new Analyzer(global, this).lateEnter(unit, symbol);
    }

    public Checker[] postCheckers(Global global) {
        return new Checker[] {
            new CheckSymbols(global),
            new CheckTypes(global),
            new CheckOwners(global),
            new CheckNames(global)
        };
    }
}
