/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.transformer;

import java.io.*;
import java.util.HashMap;
import scalac.*;
import scalac.util.*;
import scalac.ast.*;
import scalac.symtab.*;
import Tree.*;

/** A Tail Call transformer
 *
 *  @author     Erik Stenman
 *  @version    1.0
 *
 *  What it does:
 *  Finds calls in tail-positions and
 *  replaces them with direct calls to LabelDefs.
 *  A call is in a tail-position if it is the last
 *  instruciton to be executed in the body of a function.
 *  This is done by recursing over the tree keeping track
 *  of whether we are in a tail position or not.
 *
 *  At the moment this transformation is tageted towards the
 *  JVM. Since the JVM only supports jumps to known locations
 *  within a function some special care has to be taken.
 *  First we have to check that the call is self-recursive.
 *  Then we check that the function (or the class) is final.
 *  (Otherwise it could be a call to an overridden function in
 *  a subclass.)
 *  If all conditions are met we introduce a labelDef at the
 *  beginning of the function, and redirects the call (apply)
 *  to this label.
 *
 *  While traversing the tree we use a stack of States to keep track of
 *  information about the current node in the tree.
 *	inTailPosition  -- True iff the curent node is in a tail position
 *	currentClass    -- The symbol of the enclosing class of the node
 *                         if there is one.
 *	currentFunction -- The symbol of the enclosing function of the node
 *                         if there is one.
 *	newLabel        -- When in a function this is the symbol of the
 *                         LabelDef to recurse back to if a tail-call occures.
 *      needLabelDef    -- Set to true when a tail call is encounterd to
 *                         indicate that the LabelDef needs to be inserted.
 */

public class TailCall extends Transformer {
    final Global global;
    final Definitions definitions;
    private State state = new State();

    public TailCall(Global global, PhaseDescriptor descr) {
        super(global);
	this.global = global;
	this.definitions = global.definitions;
    }

    /* Keep track of information about the current node. */
    private class State {
	State next = null;
	boolean inTailPosition = false;
	Symbol currentClass = null;
	Symbol currentFunction = null;
	Symbol newLabel = null;
	boolean needLabelDef = false;
    }

    /* Push a new empty state on the state stack */
    private void push_new() {
	State old = state;
	state = new State();
	state.next=old;
    }

    /* Push a copy of the current state on the stack,
       with aditional information about a new LabelDef. */
    private void push_label(Symbol label) {
	State old = state;
	state = new State();
	state.next=old;
	state.inTailPosition = old.inTailPosition;
	state.currentClass = old.currentClass;
	state.currentFunction = old.currentFunction;
	state.newLabel = label;
	state.needLabelDef = false;
    }

    /* Pop a state from the state stack. */
    private void pop() {
	state = state.next;
    }


    public Tree transform(Tree tree) {
        switch (tree) {

	case DefDef(int mods, Name name, AbsTypeDef[] tparams, ValDef[][] vparams, Tree tpe, Tree rhs): {
	    AbsTypeDef[] newTparams  = tail_transform(tparams,false);
	    ValDef[][] newVparams = tail_transform(vparams,false);
	    Tree newTpe   = tail_transform(tpe,false);

	    /* Create a new symbol for the LabelDef */
	    Symbol newLabel = new TermSymbol(tree.pos, name, tree.symbol(), Modifiers.LABEL);
	    newLabel.setInfo(tree.symbol().type());

	    /* Push information about the label on the state stack. */
	    push_label(newLabel);
	    state.inTailPosition = true;
	    state.currentFunction = tree.symbol();

	    /* Traverse the body of the function */
	    Tree newRhs   = transform(rhs);

	    /* Was a tail-call inserted? */
	    if (state.needLabelDef) {
		pop(); // Pop the label-state from the stack.
		// The LabelDef needs identifiers as parameters.
		Ident [] args = to_ident(newVparams[0]);
		// Create a new LabelDef with the new body of the function as the rhs
		Tree labelDef = new ExtLabelDef(newLabel,args,newRhs).setType(newRhs.type());
		// Create a new function node with the LabelDef as the rhs.
		return copy.DefDef(tree,tree.symbol(), newTparams,
				   newVparams, newTpe, labelDef);
	    } else { // No recursive tail-vall inserted.
		pop(); // Pop the label-state from the stack.
		// Don't insert the LabelDef.
		return copy.DefDef(tree,tree.symbol(), newTparams,
				   newVparams, newTpe, newRhs);
	    }
	}

	case Apply(Tree fun, Tree[] args): {
	    if (state.inTailPosition) {  // This is a tail-call
		switch (fun) {
		case Select(Tree qual, Name name):
		    if (state.currentFunction == fun.symbol()) { // Is is self-recursive?
			// Make sure that function is from the same instance of the class as we are in.
			// If it is an Object (Module) we don't necessarily have a THIS, so we compare
			// the types.
			// If it's a class we have to make sure that the qulifier is a THIS node.
			if ((state.currentClass.isModuleClass() &&
			     qual.type.isSameAs(state.currentClass.thisType().widen())) ||
			    (qual instanceof This && qual.symbol() == state.currentClass)){

			    // We can only rewrite final functions in a safe way.
			    if(state.currentFunction.isMethodFinal()) {

				Tree[] newArgs = tail_transform(args,false);
				// Redirect the call to the LabelDef.
				Tree newTarget = new ExtIdent(state.newLabel).setType(fun.type());
				// Indicate that we have inserted a tail call.
				state.needLabelDef = true;
				return copy.Apply(tree,newTarget,newArgs);
			    }
			}
		    }
		    break;
		    // TODO: Handle the case of Apply(TypeApply(T))
		    // Have to check that the type T is the same as currentFunction.type()
		default:
		    break;
		}
	    }
	    // Call not in tail-pos: recurse over the arguments.
	    Tree[] newArgs = tail_transform(args,false);
	    Tree newFun = tail_transform(fun,false);
	    //  global.debugPrinter.print("OldApply fun "+newFun.symbol().name).println().end();
	    return copy.Apply(tree, newFun, newArgs);

	}


	case ClassDef(int mods, Name name, AbsTypeDef[] tparams,
		      ValDef[][] vparams, Tree tpe, Template impl): {
	    return copy.ClassDef(tree, tree.symbol(),tparams,vparams,tpe,
				 transform_class(impl,tree.symbol()));
	}


        case AbsTypeDef(int mods, Name name, Tree rhs, Tree lobound):
	    return copy.AbsTypeDef(tree, tree.symbol(),
				   tail_transform(rhs,false),
				   tail_transform(lobound,false));

       case CaseDef(Tree pat, Tree guard, Tree body):
            return copy.CaseDef(tree,
				tail_transform(pat,false),
				tail_transform(guard,false),
				transform(body));


       case Template(Tree[] parents, Tree[] body):
	   return copy.Template(tree, tree.symbol(),
				tail_transform(parents,false),
				transform(body));

	case Block(Tree[] stats): {
	    int last = stats.length-1;
	    // All statements except the last will not be tail-calls
	    for (int i = 0; i < last; i++) {
		Tree t = tail_transform(stats[i],false);
		// We are a bit paranoid about creating garbage...
		if (t != stats[i]) { // ... so we only copy the tree if it changes.
		    Tree[] res = new Tree[stats.length];
		    // copy all the preceding statements to the new array.
		    System.arraycopy(stats, 0, res, 0, i);
		    res[i++] = t;
		    // Put all the following statements in the new array.
		    for (; i < last; i++)
			res[i] = tail_transform(stats[i],false);
		    // The last statement might be in a tail position.
		    res[last] = transform(stats[last]);
		    return copy.Block(tree,res);
		}
	    } // No change in the tree after handling all but the last statement.
	    if (last > 0) {
		Tree t = transform(stats[last]);
		if (t != stats[last]) {
		    Tree[] res = new Tree[stats.length];
		    System.arraycopy(stats, 0, res, 0, last);
		    res[last] = t;
		    return copy.Block(tree,res);
		}
	    }
	    // No change at all
            return copy.Block(tree,stats);
	}


        case Sequence(Tree[] trees):
            return copy.Sequence(tree, tail_transform(trees,false));



        case Assign(Tree lhs, Tree rhs):
            return copy.Assign(tree,
			       tail_transform(lhs,false),
			       tail_transform(rhs,false));


	case If(Tree cond, Tree thenp, Tree elsep): {
            return copy.If(tree, tail_transform(cond,false),
			   transform(thenp),
			   transform(elsep));
	}

	case New(Template templ):
	    // At the moment we assume that the call to the constructor can not be
	    // a tailcall.
            return copy.New(tree, tail_transform(templ,false));

        case Typed(Tree expr, Tree tpe):
            return copy.Typed(tree,
			      transform(expr),
			      tail_transform(tpe,false));

        case TypeApply(Tree fun, Tree[] args):
            return copy.TypeApply(tree,
				  tail_transform(fun,false),
				  tail_transform(args,false));

        default:
            return super.transform(tree);
        }
    }

    public Tree transform_new(Tree tree) {
	push_new();
	Tree res = transform(tree);
	pop();
	return res;
    }


    public Template transform_new(Template t) {
	push_new();
	Template res = (Template)transform((Tree)t);
	pop();
	return res;
    }

    public Tree tail_transform(Tree tree,boolean inTailPosition) {
	boolean save = state.inTailPosition;
	state.inTailPosition = inTailPosition;
	Tree res = transform(tree);
	state.inTailPosition = save;
	return res;
    }

    public Tree[] tail_transform(Tree[] tree,boolean inTailPosition) {
	boolean save = state.inTailPosition;
	state.inTailPosition = inTailPosition;
	Tree[] res = transform(tree);
	state.inTailPosition = save;
	return res;
    }

    public Template tail_transform(Template tree,boolean inTailPosition) {
	boolean save = state.inTailPosition;
	state.inTailPosition = inTailPosition;
	Template res = (Template)transform((Tree)tree);
	state.inTailPosition = save;
	return res;
    }

    public AbsTypeDef[] tail_transform(AbsTypeDef[] tree,boolean inTailPosition) {
	boolean save = state.inTailPosition;
	state.inTailPosition = inTailPosition;
	AbsTypeDef[] res = transform(tree);
	state.inTailPosition = save;
	return res;
    }

    public ValDef[][] tail_transform(ValDef[][] tree,boolean inTailPosition) {
	boolean save = state.inTailPosition;
	state.inTailPosition = inTailPosition;
	ValDef[][] res = transform(tree);
	state.inTailPosition = save;
	return res;
    }


    public Template transform_class(Template t, Symbol currentClass) {
	Symbol saveClass = state.currentClass;
	Symbol saveFunction = state.currentFunction;
	state.currentFunction = null;
	state.currentClass = currentClass;
	Template res = (Template)transform((Tree)t);
	state.currentFunction = saveFunction;
	state.currentClass = saveClass;
	return res;
    }



    private Ident[] to_ident(Tree[] tree) {
	Ident[] ids = new Ident[tree.length];
	for (int i = 0; i < tree.length; i++) {
	    switch (tree[i]) {
	    case AbsTypeDef(int mods, Name name, Tree bound, Tree lobound):
		Ident arg = new ExtIdent(tree[i].symbol());
		arg.setType(tree[i].type());
		ids[i]= arg;
		break;

	    case ValDef(int mods, Name name, Tree tpe, Tree.Empty):
		Ident arg = new ExtIdent(tree[i].symbol());
		arg.setType(tree[i].type());
		ids[i]= arg;
		break;

	    default:
		Debug.abort("bad parameter: " + tree[i]);
	    }
	}
	return ids;
    }

}
