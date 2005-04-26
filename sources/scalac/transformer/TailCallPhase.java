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
import scalac.CompilationUnit;
import scalac.ast.Tree;
import scalac.ast.GenTransformer;
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
 * contain such calls are not transformed). However, they are not that
 * many.
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
 * Nested functions can be tail recursive (if this phase runs before
 * lambda lift) and they are transformed as well.
 *
 * If a method contains self-recursive calls, a label is added to at
 * the beginning of its body and the calls are replaced by jumps to
 * that label.
 */
public class TailCallPhase extends Phase {

    private static class Context {
        /** The current method */
        public Symbol method = Symbol.NONE;

        /** The current tail-call label */
        public Symbol label = Symbol.NONE;

        /** The expected type arguments of self-recursive calls */
        public Type[] types;

        /** Tells whether we are in a tail position. */
        public boolean tailPosition;

        public Context() {
            this.tailPosition = false;
        }

        /**
         * The Context of this transformation. It contains the current enclosing method,
         * the label and the type parameters of the enclosing method, together with a
         * flag which says whether our position in the tree allows tail calls. This is
         * modified only in <code>Block</code>, where stats cannot possibly contain tail
         * calls, but the value can.
         */
        public Context(Symbol method, Symbol label, Type[] types, boolean tailPosition) {
            this.method = method;
            this.label  = label;
            this.types  = types;
            this.tailPosition = tailPosition;
        }
    }


    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public TailCallPhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);
    }

   //########################################################################
    // Public Methods

    /** Applies this phase to the given compilation unit. */
    public void apply(CompilationUnit unit) {
        treeTransformer.apply(unit);
    }

   //########################################################################
    // Private Classes

    /** The tree transformer */
    private final GenTransformer treeTransformer = new GenTransformer(global) {


        /** The context of this call */
        private Context ctx = new Context();

        /** Transform the given tree, which is (or not) in tail position */
        public Tree transform(Tree tree, boolean tailPos) {
            boolean oldTP = ctx.tailPosition;
            ctx.tailPosition = tailPos;
            tree = transform(tree);
            ctx.tailPosition = oldTP;
            return tree;
        }

        public Tree[] transform(Tree[] trees, boolean tailPos) {
            boolean oldTP = ctx.tailPosition;
            ctx.tailPosition = tailPos;
            trees = transform(trees);
            ctx.tailPosition = oldTP;
            return trees;
        }

        /** Transforms the given tree. */
        public Tree transform(Tree tree) {
            switch (tree) {

            case DefDef(_, _, _, _, _, Tree rhs):
                Context oldCtx = ctx;

                ctx = new Context();

                //assert method == null: Debug.show(method) + " -- " + tree;
                ctx.method = tree.symbol();
                ctx.tailPosition = true;

                if (ctx.method.isMethodFinal() || ctx.method.owner().isMethod()) {
                    ctx.label = ctx.method.newLabel(ctx.method.pos, ctx.method.name);
                    ctx.types = Type.EMPTY_ARRAY;
                    Type type = ctx.method.type();
                    switch (type) {
                    case PolyType(Symbol[] tparams, Type result):
                        ctx.types = Symbol.type(tparams);
                        type = result;
                    }
                    ctx.label.setInfo(type.cloneType(ctx.method, ctx.label));
                    rhs = transform(rhs);
                    if (ctx.label.isAccessed()) {
                        global.log("Rewriting def " + ctx.method.simpleName());
                        rhs = gen.LabelDef(ctx.label, ctx.method.valueParams(), rhs);
                    }
                    tree = gen.DefDef(ctx.method, rhs);
                } else {
                    assert !ctx.method.isMethodFinal()
                        : "Final method: " + ctx.method.simpleName();
                    // non-final method
                    ctx.tailPosition = false;
                    tree = gen.DefDef(tree.symbol(), transform(rhs));
                }
                ctx = oldCtx;
                return tree;

            case Block(Tree[] stats, Tree value):
                boolean oldPosition = ctx.tailPosition;
                ctx.tailPosition = false;  stats = transform(stats);
                ctx.tailPosition = oldPosition;
                return gen.Block(tree.pos, stats, transform(value));


            case If(Tree cond, Tree thenp, Tree elsep):
                Type type = tree.type();
                thenp = transform(thenp);
                elsep = transform(elsep);
                return gen.If(tree.pos, cond, thenp, elsep);

            case Switch(Tree test, int[] tags, Tree[] bodies, Tree otherwise):
                Type type = tree.type();
                bodies = transform(bodies);
                otherwise = transform(otherwise);
                return gen.Switch(tree.pos, test, tags, bodies,otherwise,type);

            // handle pattern matches explicitly
	    case Apply(Select(_, scalac.util.Names._match), Tree[] args):
		Tree newTree = global.make.Apply(tree.pos, ((Tree.Apply)tree).fun, transform(args));
		newTree.setType(tree.getType());
		return newTree;

            case Apply(TypeApply(Tree fun, Tree[] targs), Tree[] vargs):
		if (ctx.method != Symbol.NONE && ctx.tailPosition) {
		    assert targs != null : "Null type arguments " + tree;
		    assert ctx.types != null : "Null types " + tree;

		    if (!Type.isSameAs(Tree.typeOf(targs), ctx.types) |
                        !ctx.tailPosition)
                        return tree;
		    return transform(tree, fun, transform(vargs, false));
		} else
		    return tree;

            case Apply(Tree fun, Tree[] vargs):
                if (ctx.tailPosition)
                    return transform(tree, fun, transform(vargs, false));
                else
                    return gen.mkApply_V(fun, transform(vargs, false));

	    case Visitor(Tree.CaseDef[] cases):
		Tree newTree = global.make.Visitor(tree.pos, super.transform(cases));
		newTree.setType(tree.getType());
		return newTree;

	    case CaseDef(Tree pattern, Tree guard, Tree body):
		return gen.CaseDef(pattern, guard, transform(body));

	    case Typed(Tree expr, Tree type):
		return gen.Typed(transform(expr), type);

            case ClassDef(_, _, _, _, _, Tree.Template impl):
                Symbol impl_symbol = getSymbolFor(impl);
                Tree[] body = transform(impl.body);
                return gen.ClassDef(getSymbolFor(tree), impl.parents, impl_symbol, body);

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
	    case AbsTypeDef(_, _, _, _):
	    case AliasTypeDef(_, _, _, _):
	    case Import(_, _):
	    case Function(_, _):
                return tree;

            default:
                throw Debug.abort("illegal case", tree);
            }
        }

        /** Transforms the given function call. */
        private Tree transform(Tree tree, Tree fun, Tree[] vargs) {
            if (fun.symbol() != ctx.method)
		return tree;
            switch (fun) {
            case Select(Tree qual, _):
                if (!isReferenceToThis(qual, ctx.method.owner()))
		    return tree;
                global.log("Applying tail call recursion elimination for " +
                           ctx.method.enclClass().simpleName() + "." + ctx.method.simpleName());
                return gen.Apply(tree.pos, gen.Ident(qual.pos, ctx.label), vargs);

            case Ident(_):
                global.log("Applying tail call recursion elimination for function " +
                           ctx.method.enclClass().simpleName() + "." + ctx.method.simpleName());
                return gen.Apply(tree.pos, gen.Ident(fun.pos, ctx.label), vargs);

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
            default:
                return false;
            }
        }

    };

    //########################################################################
}
