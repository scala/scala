/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.ast;

import scalac.ApplicationError;
import scalac.util.Name;
import scalac.util.Names;
import scalac.symtab.Type;
import scalac.symtab.Symbol;
import scalac.symtab.Modifiers;

import java.util.LinkedList;

public class TreeInfo {

    public static boolean isTerm(Tree tree) {
	return tree.isTerm();
    }

    public static boolean isType(Tree tree) {
	return tree.isType();
    }

    public static boolean isOwnerDefinition(Tree tree) {
	switch (tree) {
	case PackageDef(_, _):
	case ClassDef(_, _, _, _, _, _):
	case ModuleDef(_, _, _, _):
	case DefDef(_, _, _, _, _, _):
	case Import(_, _):
	    return true;
	default:
	    return false;
	}
    }

    public static boolean isDefinition(Tree tree) {
	switch (tree) {
	case PackageDef(_, _):
	case ClassDef(_, _, _, _, _, _):
	case ModuleDef(_, _, _, _):
	case DefDef(_, _, _, _, _, _):
	case ValDef(_, _, _, _):
	case AbsTypeDef(_, _, _, _):
	case AliasTypeDef(_, _, _, _):
	case Import(_, _):
	    return true;
	default:
	    return false;
	}
    }

    public static boolean isDeclaration(Tree tree) {
	switch (tree) {
	case DefDef(_, _, _, _, _, Tree rhs):
	    return rhs == Tree.Empty;
	case ValDef(_, _, _, Tree rhs):
	    return rhs == Tree.Empty;
	case AbsTypeDef(_, _, _, _):
	case AliasTypeDef(_, _, _, _):
	    return true;
	default:
	    return false;
	}
    }

    /** Is tree a pure definition?
     */
    public static boolean isPureDef(Tree tree) {
	switch (tree) {
	case Tree.Empty:
	case ClassDef(_, _, _, _, _, _):
	case ModuleDef(_, _, _, _):
	case DefDef(_, _, _, _, _, _):
	case AbsTypeDef(_, _, _, _):
	case AliasTypeDef(_, _, _, _):
	case Import(_, _):
	    return true;
	case ValDef(int mods, _, _, Tree rhs):
	    return (mods & Modifiers.MUTABLE) == 0 && isPureExpr(rhs);
	default:
	    return false;
	}
    }

    /** Is tree a stable & pure expression?
     */
    public static boolean isPureExpr(Tree tree) {
	switch (tree) {
	case Empty:
	case This(_):
	case Super(_, _):
	    return true;
	case Ident(_):
	    assert tree.type != null : tree.toString();
	    return tree.symbol().isStable();
	case Select(Tree qual, _):
	    return tree.symbol().isStable() && isPureExpr(qual);
	case Apply(Tree fn, Tree[] args):
	    return isPureExpr(fn) && args.length == 0;
	case TypeApply(Tree fn, Tree[] targs):
	    return isPureExpr(fn);
	case Typed(Tree expr, _):
	    return isPureExpr(expr);
	case Literal(_):
	    return true;
	default:
	    return false;
	}
    }

    /** Is tree a pure constructor?
     */
    public static boolean isPureConstr(Tree tree) {
	switch (tree) {
	case Ident(_):
	case Select(_, _):
 	    return tree.symbol() != null && tree.symbol().isPrimaryConstructor();
	case TypeApply(Tree constr, _):
	    return isPureConstr(constr);
	case Apply(Tree fn, Tree[] args):
	    return args.length == 0 && isPureConstr(fn);
	default:
	    return false;
	}
    }

    /** Is tree a self constructor call?
     */
    public static boolean isSelfConstrCall(Tree tree) {
	switch (tree) {
	case Ident(Name name):
	    return name == Names.CONSTRUCTOR;
	case TypeApply(Tree constr, _):
	    return isSelfConstrCall(constr);
	case Apply(Tree constr, _):
	    return isSelfConstrCall(constr);
	default:
	    return false;
	}
    }

    /** Is tree a variable pattern
     */
    public static boolean isVarPattern(Tree pat) {
	switch (pat) {
	case Ident(Name name):
	    return name.isVariable();
	default:
	    return false;
	}
    }

    /** Is tree a this node which belongs to `enclClass'?
     */
    public static boolean isSelf(Tree tree, Symbol enclClass) {
	switch (tree) {
	case This(_):
	    return tree.symbol() == enclClass;
	default:
	    return false;
	}
    }

    /** The method symbol of an application node, or Symbol.NONE, if none exists.
     */
    public static Symbol methSymbol(Tree tree) {
	Tree meth = methPart(tree);
	if (meth.hasSymbol()) return meth.symbol();
	else return Symbol.NONE;
    }

    /** The method part of an application node
     */
    public static Tree methPart(Tree tree) {
	switch (tree) {
	case Apply(Tree fn, _):
	    return methPart(fn);
	case TypeApply(Tree fn, _):
	    return methPart(fn);
	case AppliedType(Tree fn, _):
	    return methPart(fn);
	default:
	    return tree;
	}
    }


      /** returns true if the tree is a sequence-valued pattern.
       *  precondition: tree is a pattern.
       *  calls isSequenceValued( Tree, List ) because needs to remember bound
       *  values.
       */
      public static boolean isSequenceValued( Tree tree ) {
            return isSequenceValued( tree, new LinkedList() );
      }

      /** returns true if the tree is a sequence-valued pattern.
       *  precondition: tree is a pattern
       */
      public static boolean isSequenceValued( Tree tree, LinkedList recVars ) {
            switch( tree ) {
            case Bind(_, Tree t):
                  recVars.addFirst( tree.symbol() );
                  boolean res = isSequenceValued( t );
                  recVars.removeFirst();
                  return res;
            case Sequence(_):
                  return true;
            case Alternative(Tree[] ts):
                  for( int i = 0; i < ts.length; i++ ) {
                        if( isSequenceValued( ts[ i ] ) )
                              return true;
                  }
                  return false;
            case Apply( _, _ ):
            case Literal( _ ):
                  return false;
            case Ident(Name n): // if Ident is a recursive var, then true
                  return recVars.contains( tree.symbol() );
	    case Select(_,_):
		return false;
            default:
                  throw new scalac.ApplicationError("Unexpected pattern "+tree.getClass());
            }
      }

    /** returns true if the argument is an empty sequence pattern
     */
    public static boolean isEmptySequence( Tree tree ) {
	switch( tree ) {
	case Sequence( Tree ts[] ):
	    return ( ts.length == 0 );
	default:
	    return false;
	}
    }
}
