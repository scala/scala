/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.transformer;

import scalac.Global;
import scalac.Phase;
import scalac.PhaseDescriptor;
import scalac.Unit;
import scalac.ast.Tree;
import scalac.ast.GenTransformer;
import scalac.symtab.LabelSymbol;
import scalac.symtab.Symbol;
import scalac.symtab.Type;
import scalac.util.Debug;

/**
 * A Tail Call Transformer
 *
 * @author     Erik Stenman
 * @version    1.0
 *
 * What it does:
 *
 * Finds method calls in tail-position and replaces them with jumps.
 * A call is in a tail-position if it is the last instruction to be
 * executed in the body of a method.  This is done by recursing over
 * the trees that may contain calls in tail-position (trees that can't
 * contain such calls are not transformed).
 *
 * Self-recursive calls in tail-position are replaced by jumps to a
 * label at the beginning of the method. As the JVM provides no way to
 * jump from a method to another one, non-recursive calls in
 * tail-position are not optimized.
 *
 * A method call is self-recursive if it calls the current method on
 * the current instance and the method is final (otherwise, it could
 * be a call to an overridden method in a subclass). Furthermore, If
 * the method has type parameters, the call must contain these
 * parameters as type arguments.
 *
 * If a method contains self-recursive calls, a label is added to at
 * the beginning of its body and the calls are replaced by jumps to
 * that label.
 */
public class TailCallPhase extends Phase {

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public TailCallPhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);
    }

   //########################################################################
    // Public Methods

    /** Applies this phase to the given compilation units. */
    public void apply(Unit[] units) {
        treeTransformer.apply(units);
    }

   //########################################################################
    // Private Classes

    /** The tree transformer */
    private final GenTransformer treeTransformer = new GenTransformer(global) {

        /** The current method */
        private Symbol method;

        /** The current tail-call label */
        private Symbol label;

        /** The expected type arguments of self-recursive calls */
        private Type[] types;

        /** Transforms the given tree. */
        public Tree transform(Tree tree) {
            switch (tree) {

            case DefDef(_, _, _, _, _, Tree rhs):
                assert method == null: Debug.show(method) + " -- " + tree;
                method = tree.symbol();
                if (method.isMethodFinal()) {
                    label = new LabelSymbol(method);
                    types = Type.EMPTY_ARRAY;
                    Type type = method.type();
                    switch (type) {
                    case PolyType(Symbol[] tparams, Type result):
                        types = Symbol.type(tparams);
                        type = result;
                    }
                    label.setInfo(type.cloneType(method, label));
                    rhs = transform(rhs);
                    if (label.isAccessed()) {
                        rhs = gen.LabelDef(label, method.valueParams(), rhs);
                        tree = gen.DefDef(method, rhs);
                    }
                    types = null;
                    label = null;
                }
                method = null;
                return tree;

            case Block(Tree[] stats, Tree value):
                return gen.Block(tree.pos, stats, transform(value));

            case If(Tree cond, Tree thenp, Tree elsep):
                Type type = tree.type();
                thenp = transform(thenp);
                elsep = transform(elsep);
                return gen.If(tree.pos, cond, thenp, elsep, type);

            case Switch(Tree test, int[] tags, Tree[] bodies, Tree otherwise):
                Type type = tree.type();
                bodies = transform(bodies);
                otherwise = transform(otherwise);
                return gen.Switch(tree.pos, test, tags, bodies,otherwise,type);

            case Apply(TypeApply(Tree fun, Tree[] targs), Tree[] vargs):
                if (!Type.isSameAs(Tree.typeOf(targs), types)) return tree;
                return transform(tree, fun, vargs);
            case Apply(Tree fun, Tree[] vargs):
                return transform(tree, fun, vargs);

            case ClassDef(_, _, _, _, _, _):
            case PackageDef(_, _):
            case LabelDef(_, _, _):
            case Return(_):
                return super.transform(tree);

            case Empty:
            case ValDef(_, _, _, _):
            case Assign(_, _):
            case New(_):
            case Super(_, _):
            case This(_):
            case Select(_, _):
            case Ident(_):
            case Literal(_):
            case TypeTerm():
                return tree;

            default:
                throw Debug.abort("illegal case", tree);
            }
        }

        /** Transforms the given function call. */
        private Tree transform(Tree tree, Tree fun, Tree[] vargs) {
            Symbol symbol = fun.symbol();
            if (symbol != method) return tree;
            switch (fun) {
            case Select(Tree qual, _):
                if (!isReferenceToThis(qual, method.owner())) return tree;
                return gen.Apply(tree.pos, gen.Ident(qual.pos, label), vargs);
            case Ident(_):
                return tree; // !!! or is this a tail call?
            default:
                throw Debug.abort("illegal case", fun);
            }
        }

        /**
         * Returns true if the tree represents the current instance of
         * given class.
         */
        private boolean isReferenceToThis(Tree tree, Symbol clasz) {
            switch (tree) {
            case This(_):
                assert tree.symbol() == clasz: tree +" -- "+ Debug.show(clasz);
                return true;
            case Select(Tree qual, _):
                if (!clasz.isModuleClass()) return false;
                if (tree.symbol() != clasz.module()) return false;
                return isReferenceToThis(qual, clasz.owner());
            case Ident(_):
                if (!clasz.isModuleClass()) return false;
                if (tree.symbol() != clasz.module()) return false;
                return true;
            default:
                return false;
            }
        }

    };

    //########################################################################
}
