/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.transformer.matching;

import ch.epfl.lamp.util.Position;

import scalac.*;
import scalac.ast.*;
import scalac.util.*;
import scalac.symtab.*;
import PatternNode.*;
import Tree.*;

class CodeFactory extends PatternTool {

    public int pos = Position.FIRSTPOS ;

    public CodeFactory( Unit unit, int pos ) {
	super( unit );
	this.pos = pos;
    }

    // --------- these are new

    /** a faked switch statement
     */
    Tree Switch( Tree selector,
		 Tree condition[],
		 Tree body[],
		 Tree defaultBody ) {
	assert selector != null:"selector is null";
	assert condition != null:"cond is null";
	assert body != null:"body is null";
	assert defaultBody != null:"defaultBody is null";
	Tree result = defaultBody;

	for( int i = condition.length-1; i >= 0; i-- )
	    result = gen.If(condition[i], body[i], result);

	return result ;
    }

    /** returns  `List[ Tuple2[ scala.Int, <elemType> ] ]' */
      Type SeqTraceType( Type elemType ) {
          return defs.LIST_TYPE(pairType(defs.INT_TYPE(), elemType));
      }

    /**  returns `Iterator[ elemType ]' */
    Type _seqIterType( Type elemType ) {
        return defs.ITERATOR_TYPE(elemType);
    }

    /**  returns `<seqObj.elements>' */
    Tree newIterator( Tree seqObj, Type elemType ) {
	return gen.mkApply__(gen.Select(seqObj, defs.ITERABLE_ELEMENTS()));
    }

    /** returns code `<seqObj>.elements'
     *  the parameter needs to have type attribute `Sequence[<elemType>]'
     */
    Tree newIterator( Tree seqObj ) {
	return newIterator( seqObj, getElemType_Sequence( seqObj.type() ));
    }

    // EXPERIMENTAL
    Tree newRef( Tree init ) {
	//System.out.println( "hello:"+refSym().type() );
	return gen.New(gen.mkPrimaryConstr(pos, defs.REF_CLASS,
			                    new Type[] { init.type() },
			                    new Tree[] { init } ));
    }

    /** returns A for T <: Sequence[ A ]
     */
    Type getElemType_Sequence( Type tpe ) {
	//System.err.println("getElemType_Sequence("+tpe.widen()+")");
	Type tpe1 = tpe.widen().baseType( defs.SEQ_CLASS );

	if( tpe1 == Type.NoType )
	    throw new ApplicationError("arg "+tpe+" not subtype of Sequence[ A ]");

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

    Tree SeqTrace_headElem( Tree arg ) { // REMOVE SeqTrace
	Tree t = gen.mkApply__(gen.Select(arg, defs.LIST_HEAD()));
	return gen.mkApply__(gen.Select(t, defs.TUPLE_FIELD(2, 2)));
    }

    Tree SeqTrace_headState( Tree arg ) { // REMOVE SeqTrace
	Tree t = gen.mkApply__(gen.Select(arg, defs.LIST_HEAD()));
	return gen.mkApply__(gen.Select(t, defs.TUPLE_FIELD(2, 1)));

    }

    Tree SeqTrace_tail( Tree arg ) { // REMOVE SeqTrace
	return gen.mkApply__(gen.Select(arg, defs.LIST_TAIL()));
    }

    /** `<seqlist>.head()'
     */
    Tree SeqList_head( Tree arg ) {
	return gen.mkApply__(gen.Select(arg, defs.LIST_HEAD()));
    }

     // unused
       public Tree Negate(Tree tree) {
       switch (tree) {
       case Literal(Object value):
       return gen.mkBooleanLit(tree.pos, !((Boolean)value).booleanValue());
       }
       return gen.mkApply__(gen.Select(tree, defs.BOOLEAN_NOT()));
       }

    protected Tree And(Tree left, Tree right) {
        switch (left) {
	case Literal(Object value):
	    return ((Boolean)value).booleanValue() ? right : left;
        }
        switch (right) {
	case Literal(Object value):
	    if (((Boolean)value).booleanValue()) return left;
        }
        return gen.mkApply_V(gen.Select(left, defs.BOOLEAN_AND()), new Tree[]{right});
    }

    protected Tree Or(Tree left, Tree right) {
        switch (left) {
	case Literal(Object value):
	    return ((Boolean)value).booleanValue() ? left : right;
        }
        switch (right) {
	case Literal(Object value):
	    if (!((Boolean)value).booleanValue()) return left;
        }
        return gen.mkApply_V(gen.Select(left, defs.BOOLEAN_OR()), new Tree[]{right});
    }

    protected Tree Equals(Tree left, Tree right) {
        Symbol fun = unit.global.definitions.ANY_EQEQ;
        return gen.mkApply_V(gen.Select(left, fun), new Tree[]{right});
    }

    protected Tree ThrowMatchError(int pos, Type type) {
        return gen.mkApplyTV(
			     gen.mkRef(pos, defs.MATCHERROR_FAIL()),
                             new Tree[]{gen.mkType(pos, type)},
                             new Tree[]{
                                 gen.mkStringLit(pos, unit.toString()),
                                 gen.mkIntLit(pos, Position.line(pos))
                             });
    }


    Type pairType( Type left, Type right ) {
	return defs.TUPLE_TYPE(new Type[] { left, right } );
    }

    Tree newPair( Tree left, Tree right ) {
 	return gen.New(gen.mkPrimaryConstr(pos, defs.TUPLE_CLASS[2],
			                   new Type[] { left.type(), right.type() },
			                   new Tree[] { left, right }));

    }

}
