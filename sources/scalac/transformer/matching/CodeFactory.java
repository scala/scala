/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.transformer.matching;

import scala.tools.util.Position;

import scalac.*;
import scalac.ast.*;
import scalac.atree.AConstant.*;
import scalac.util.*;
import scalac.symtab.*;
import PatternNode.*;
import Tree.*;

public class CodeFactory extends PatternTool {

    public int pos = Position.FIRSTPOS ;

    public CodeFactory( CompilationUnit unit, int pos ) {
	super( unit );
	this.pos = pos;
    }

    // --------- these are new

    /** a faked switch statement
     */
    public Tree Switch( Tree condition[],
		 Tree body[],
		 Tree defaultBody ) {
	assert condition != null:"cond is null";
	assert body != null:"body is null";
	assert defaultBody != null:"defaultBody is null";
	Tree result = defaultBody;

	for( int i = condition.length-1; i >= 0; i-- )
	    result = gen.If(condition[i], body[i], result);

	return result ;
    }

    /** returns  `List[ Tuple2[ scala.Int, <elemType> ] ]' */
      public Type SeqTraceType( Type elemType ) {
          return defs.LIST_TYPE(pairType(defs.int_TYPE(), elemType));
      }

    /**  returns `Iterator[ elemType ]' */
    public Type _seqIterType( Type elemType ) {
        return defs.ITERATOR_TYPE(elemType);
    }

    /**  returns `<seqObj.elements>' */
    public Tree newIterator( Tree seqObj, Type elemType ) {
	return gen.mkApply__(gen.Select(seqObj, defs.ITERABLE_ELEMENTS()));
    }

    /** returns code `<seqObj>.elements'
     *  the parameter needs to have type attribute `Sequence[<elemType>]'
     */
    public Tree newIterator( Tree seqObj ) {
	return newIterator( seqObj, getElemType_Sequence( seqObj.getType() ));
    }

    // EXPERIMENTAL
    Tree newRef( Tree init ) {
	//System.out.println( "hello:"+refSym().type() );
	return gen.New( gen.mkApplyTV( gen.mkPrimaryConstructorGlobalRef( pos, defs.REF_CLASS),
                                                                    new Type[] { init.getType() },
                                                                    new Tree[] { init } ));
    }

    /** returns A for T <: Sequence[ A ]
     */
    public Type getElemType_Sequence( Type tpe ) {
	//System.err.println("getElemType_Sequence("+tpe.widen()+")");
	Type tpe1 = tpe.widen().baseType( defs.SEQ_CLASS );

	if( tpe1 == Type.NoType )
	    throw new ApplicationError("arg "+tpe+" not subtype of Seq[ A ]");

	return tpe1.typeArgs()[ 0 ];
    }

    /** returns A for T <: Iterator[ A ]
     */
    Type getElemType_Iterator( Type tpe ) {
	//System.err.println("getElemType_Iterator("+tpe+")");

	Type tpe1 = tpe.widen().baseType( defs.ITERATOR_CLASS );

	switch( tpe1 ) {
	case TypeRef(_,_,Type[] args):
	    return args[ 0 ];
	default:
	    throw new ApplicationError("arg "+tpe+" not subtype of Iterator[ A ]");
	}

    }

    /** `it.next()'
     */
    public Tree _next( Tree iter ) {
	return gen.mkApply__(gen.Select(iter, defs.ITERATOR_NEXT()));
    }

    /** `it.hasNext()'
     */
    public Tree _hasNext( Tree iter ) {
	return gen.mkApply__(gen.Select(iter, defs.ITERATOR_HASNEXT()));
    }

    /** `!it.hasCur()'
     */
    public Tree _not_hasNext( Tree iter ) {
	return gen.mkApply__(gen.Select(_hasNext(iter), defs.BOOLEAN_NOT()));
    }

      /** `trace.isEmpty'
       */
      public Tree isEmpty( Tree iter ) {
          return gen.mkApply__(gen.Select(iter, defs.LIST_ISEMPTY()));
      }

    public Tree SeqTrace_headElem( Tree arg ) { // REMOVE SeqTrace
	Tree t = gen.mkApply__(gen.Select(arg, defs.LIST_HEAD()));
	return gen.mkApply__(gen.Select(t, defs.TUPLE_FIELD(2, 2)));
    }

    public Tree SeqTrace_headState( Tree arg ) { // REMOVE SeqTrace
	Tree t = gen.mkApply__(gen.Select(arg, defs.LIST_HEAD()));
	return gen.mkApply__(gen.Select(t, defs.TUPLE_FIELD(2, 1)));

    }

    public Tree SeqTrace_tail( Tree arg ) { // REMOVE SeqTrace
	return gen.mkApply__(gen.Select(arg, defs.LIST_TAIL()));
    }

    /** `<seqlist>.head()'
     */
    public Tree SeqList_head( Tree arg ) {
	return gen.mkApply__(gen.Select(arg, defs.LIST_HEAD()));
    }

     // unused
       public Tree Negate(Tree tree) {
       switch (tree) {
       case Literal(BOOLEAN(boolean value)):
       return gen.mkBooleanLit(tree.pos, !value);
       }
       return gen.mkApply__(gen.Select(tree, defs.BOOLEAN_NOT()));
       }

    /*protected*/ public Tree And(Tree left, Tree right) {
        switch (left) {
	case Literal(BOOLEAN(boolean value)):
	    return value ? right : left;
        }
        switch (right) {
	case Literal(BOOLEAN(boolean value)):
	    if (value) return left;
        }
        return gen.mkApply_V(gen.Select(left, defs.BOOLEAN_AND()), new Tree[]{right});
    }

    /*protected*/ public Tree Or(Tree left, Tree right) {
        switch (left) {
	case Literal(BOOLEAN(boolean value)):
	    return value ? left : right;
        }
        switch (right) {
	case Literal(BOOLEAN(boolean value)):
	    if (!value) return left;
        }
        return gen.mkApply_V(gen.Select(left, defs.BOOLEAN_OR()), new Tree[]{right});
    }

    // used by Equals
    private Symbol getCoerceToInt(Type left) {
        Symbol sym = left.lookupNonPrivate(Names.coerce);
        assert sym != Symbol.NONE : Debug.show(left);
        Symbol[] syms = sym.alternativeSymbols();
        for (int i = 0; i < syms.length; i++) {
            switch (syms[i].info()) {
            case MethodType(Symbol[] vparams, Type restpe):
                if (vparams.length == 0 && restpe.isSameAs(defs.INT_TYPE()))
                    return syms[i];
            }
        }
        assert false : Debug.show(left);
        return null;
    }

    // used by Equals
    private Symbol getEqEq(Type left, Type right) {
        Symbol sym = left.lookupNonPrivate(Names.EQEQ);
        assert sym != Symbol.NONE
            : Debug.show(left) + "::" + Debug.show(left.members());
        Symbol fun = null;
        Type ftype = defs.ANY_TYPE();
        Symbol[] syms = sym.alternativeSymbols();
        for (int i = 0; i < syms.length; i++) {
            Symbol[] vparams = syms[i].valueParams();
            if (vparams.length == 1) {
                Type vptype = vparams[0].info();
                if (right.isSubType(vptype) && vptype.isSubType(ftype)) {
                    fun = syms[i];
                    ftype = vptype;
                }
            }
        }
        assert fun != null : Debug.show(sym.info());
        return fun;
    }

    /*protected*/ public Tree Equals(Tree left, Tree right) {
        Type ltype = left.type.widen(), rtype = right.type.widen();
        if (ltype.isSameAs(rtype)
            && (ltype.isSameAs(defs.CHAR_TYPE())
                || ltype.isSameAs(defs.BYTE_TYPE())
                || ltype.isSameAs(defs.SHORT_TYPE())))
            {
                right = gen.mkApply__(gen.Select(right, getCoerceToInt(rtype)));
                rtype = defs.INT_TYPE();
            }
        Symbol eqsym = getEqEq(ltype, rtype);
        return gen.mkApply_V(gen.Select(left, eqsym), new Tree[]{right});
    }

    /*protected*/ public Tree ThrowMatchError(int pos, Type type) {
        return gen.mkApplyTV(
			     gen.mkGlobalRef(pos, defs.MATCHERROR_FAIL()),
                             new Tree[]{gen.mkType(pos, type)},
                             new Tree[]{
                                 gen.mkStringLit(pos, unit.toString()),
                                 gen.mkIntLit(pos, Position.line(pos))
                             });
    }

    /*protected*/ public Tree ThrowMatchError(int pos, Type type, Tree tree) {
        return gen.mkApplyTV(
			     gen.mkGlobalRef(pos, defs.MATCHERROR_REPORT()),
                             new Tree[]{gen.mkType(pos, type)},
                             new Tree[]{
                                 gen.mkStringLit(pos, unit.toString()),
                                 gen.mkIntLit(pos, Position.line(pos)),
                                 tree
                             });
    }

    /*protected*/ public Tree Error(int pos, Type type) {
        return gen.mkApplyTV(
			     gen.mkGlobalRef(pos, defs.MATCHERROR_FAIL()),
                             new Tree[]{gen.mkType(pos, type)},
                             new Tree[]{
                                 gen.mkStringLit(pos, unit.toString()),
                                 gen.mkIntLit(pos, Position.line(pos))
                             });
    }


    public Type pairType( Type left, Type right ) {
	return defs.TUPLE_TYPE(new Type[] { left, right } );
    }

    public Tree newPair( Tree left, Tree right ) {
 	return gen.New(gen.mkApplyTV( gen.mkPrimaryConstructorGlobalRef( pos, defs.TUPLE_CLASS[2]),
                                                                   new Type[] { left.getType(), right.getType() },
                                                                   new Tree[] { left, right }));

    }

}
