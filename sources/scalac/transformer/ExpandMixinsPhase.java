/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: ExpandMixinsPhase.java,v 1.8 2002/05/02 10:59:35 schinz Exp $
// $Id$

package scalac.transformer;

import java.util.Map;
import java.util.HashMap;

import scalac.PhaseDescriptor;
import scalac.Global;
import scalac.Unit;
import scalac.ast.Tree;
import scalac.ast.Tree.Template;
import scalac.ast.Traverser;
import scalac.ast.Transformer;
import scalac.symtab.Symbol;
import scalac.symtab.Type;
import scalac.util.Debug;

// TODO do not copy hidden members which are not accessible via
// "super".

/**
 * A phase to expand mixins using code copying. We assume that links
 * to outer classes have been made explicit by a previous phase.
 */
public class ExpandMixinsPhase extends PhaseDescriptor {

    //########################################################################
    // Private Fields

    /** The global environment */
    private Global global;

    /** A map from classes to their interface */
    private Map interfaces;

    /** A map from classes to their expanded template */
    private final Map/*<Symbol,Template>*/ expansions = new HashMap();

    /** A map from classes to their original (unexpanded) template */
    private final Map/*<Symbol,Template>*/ templates = new HashMap();

    /** A traverser that collects class definitions */
    private Traverser collector;

    /** A transformer that expands classes that have mixins */
    private Transformer expander;

    //########################################################################
    // Public Methods

    public String name () {
        return "expandmixins";
    }

    public String description () {
        return "expand mixins by code copying";
    }

    public String taskDescription() {
        return "expanded mixins";
    }

    public void apply(Global global) {
        apply(global, global.units);
    }

    public void apply(Unit unit) {
        apply(unit.global, new Unit[] { unit });
    }

    public void apply(Global global, Unit[] units) {
        if (this.global == null) {
            this.global = global;
            this.interfaces = global.PHASE.ADDINTERFACES.classToIFace;
            this.collector = new Collector();
            this.expander = new Expander(global);
        }
        collector.traverse(units);
        expander.apply(units);
        assert templates.isEmpty() : templates.keySet();
    }

    public Type transformInfo(Symbol symbol, Type type) {
        // !!! make this work and remove *1 in ClassExpander
        // if (!symbol.isJava() && symbol.isClass() && !symbol.isInterface())
        //     type = getExpandedTemplate(symbol).type();
        return type;
    }

    // !!!
    // public Checker[] postCheckers(Global global) {
    //     return new Checker[] {
    //         new CheckSymbols(global),
    //         new CheckTypes(global),
    //         new CheckOwners(global),
    //      new CheckNames(global)
    //     };
    // }

    //########################################################################
    // Private Methods

    private Template getExpandedTemplate(Symbol clasz) {
	if (global.debug) global.log("get expanded " + clasz + " in " + clasz.owner());
        Template template = (Template)expansions.get(clasz);
        if (template == null) {
            template = (Template)templates.remove(clasz);
            assert template != null : Debug.show(clasz);
            template = expandTemplate(clasz, expander.transform(template));
            expansions.put(clasz, template);
        }
        return template;
    }

    private Template expandTemplate(Symbol clasz, Template template) {
        assert Debug.log("expanding ", clasz);
        ClassExpander expander = null;
        Type[] parents = clasz.parents();
        for (int i = parents.length - 1; i > 0; --i) {
            Symbol parent = parents[i].symbol();
            if (parent.isInterface()) continue;
            if (expander == null)
                expander = new ClassExpander(global, clasz, template);
            assert Debug.log("expanding ", clasz, ": inlining ", parent);
            expander.inlineMixin(i, parents[i], (Symbol)interfaces.get(parent),
                getExpandedTemplate(parent));
        }
        return expander == null ? template : expander.getTemplate();
    }

    //########################################################################
    // Private Class - Collector

    private class Collector extends Traverser {
        public void traverse(Tree tree) {
            switch(tree) {
            case ClassDef(_, _, _, _, _, Template template):
                Symbol clasz = tree.symbol();
                if (!clasz.isInterface()) templates.put(clasz, template);
                traverse(template.body);
                return;
            case PackageDef(_, Template(_, Tree[] body)):
                traverse(body);
                return;
            }
        }
    }

    //########################################################################
    // Private Class - Expander

    private class Expander extends Transformer {
        public Expander(Global global) {
            super(global);
        }
        public Tree transform(Tree tree) {
            switch (tree) {
            case ClassDef(_, _, _, _, _, _):
                Symbol clasz = tree.symbol();
                if (clasz.isInterface()) return super.transform(tree);
                return gen.ClassDef(tree.pos,clasz,getExpandedTemplate(clasz));
            case PackageDef(_, _):
                return super.transform(tree);
            case Template(_, _):
                return super.transform(tree);
            default:
                return tree;
            }
        }
    }

    //########################################################################
}
