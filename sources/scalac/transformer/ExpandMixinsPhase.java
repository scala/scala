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

import scalac.Global;
import scalac.Phase;
import scalac.PhaseDescriptor;
import scalac.Unit;
import scalac.ast.Tree;
import scalac.ast.Tree.Template;
import scalac.ast.Traverser;
import scalac.ast.Transformer;
import scalac.symtab.Scope;
import scalac.symtab.Symbol;
import scalac.symtab.Type;
import scalac.util.Debug;

// TODO do not copy hidden members which are not accessible via
// "super".

/**
 * A phase to expand mixins using code copying. We assume that links
 * to outer classes have been made explicit by a previous phase.
 */
public class ExpandMixinsPhase extends Phase {

    //########################################################################
    // Private Fields

    /** A map from classes to their interface */
    private final Map/*<Symbol,Symbol>*/ interfaces;

    /** A map from classes to their expanded template */
    private final Map/*<Symbol,Template>*/ expansions;

    /** A map from classes to their original (unexpanded) template */
    private final Map/*<Symbol,Template>*/ templates;
    /** A traverser that collects class definitions */
    private final Traverser collector;

    /** A transformer that expands classes that have mixins */
    private final Transformer expander;

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public ExpandMixinsPhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);
        Phase addinterfaces = global.PHASE.ADDINTERFACES.phase();
        this.interfaces = ((AddInterfacesPhase)addinterfaces).classToIFace;
        this.expansions = new HashMap();
        this.templates = new HashMap();
        this.collector = new Collector();
        this.expander = new Expander(global);
    }

    //########################################################################
    // Public Methods

    public void apply(Unit[] units) {
        collector.traverse(units);
        expander.apply(units);
        // !!! assert templates.isEmpty(): templates.keySet();
    }

    public Type transformInfo(Symbol symbol, Type type) {
        // !!! make this work and remove *1 in ClassExpander
        // if (!symbol.isJava() && symbol.isClass() && !symbol.isInterface())
        //     type = getExpandedTemplate(symbol).type();
        if (symbol.isClass() && !symbol.isInterface()) {
            switch (type) {
            case CompoundType(Type[] parents, Scope members):
                Type[] types = parents;
                for (int i = 1; i < parents.length; i++) {
                    switch (parents[i]) {
                    case TypeRef(Type prefix, Symbol parent, Type[] args):
                        if (parent.isInterface()) continue;
                        if (types == parents) types = Type.cloneArray(parents);
                        parent = (Symbol)interfaces.get(parent);
                        assert parent != null: parents[i];
                        types[i] = Type.TypeRef(prefix, parent, args);
                        continue;
                    default:
                        throw Debug.abort("illegal case", parents[i]);
                    }
                }
                if (types != parents)
                    type = Type.compoundType(types, members, symbol);
                break;
            default:
                throw Debug.abort("illegal case", type);
            }
        }
        return type;
    }

    //########################################################################
    // Private Methods

    private Template getExpandedTemplate(Symbol clasz) {
	assert Debug.log("get expanded " + clasz + " in " + clasz.owner());
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
        ClassExpander expander = new ClassExpander(global, clasz, template);
        Type[] parents = clasz.parents();
        // force expansion of superclass
        if (!parents[0].symbol().isExternal())
            getExpandedTemplate(parents[0].symbol());
        // inline mixins
        for (int i = parents.length - 1; i > 0; --i) {
            Symbol parent = parents[i].symbol();
            if (parent.isInterface()) continue;
            assert Debug.log("expanding ", clasz, ": inlining ", parent);
            expander.inlineMixin(i, parents[i], (Symbol)interfaces.get(parent),
                getExpandedTemplate(parent));
        }
        return expander.getTemplate();
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
        public void apply(Unit unit) {
            if (unit.mixinOnly) {
                assert Debug.log("removing " +unit+ " after mixin expansion");
                unit.body = Tree.EMPTY_ARRAY;
            } else
                super.apply(unit);
        }
        public Tree transform(Tree tree) {
            switch (tree) {
            case ClassDef(_, _, _, _, _, _):
                Symbol clasz = tree.symbol();
                if (clasz.isInterface()) return super.transform(tree);
                return gen.ClassDef(clasz, getExpandedTemplate(clasz));
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
