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
import scalac.typechecker.*;
import PatternNode.*;
import Tree.*;

class CodeFactory extends PatternTool {

    public int pos = Position.FIRSTPOS ;

    static final Name TUPLE2_N       = Name.fromString("scala.Tuple2");

    static final Name LEFT_N = Name.fromString("_1");
    static final Name RIGHT_N = Name.fromString("_2");
    static final Name HEAD_N       = Name.fromString("head");
    static final Name TAIL_N       = Name.fromString("tail");
    static final Name ISEMPTY_N        = Name.fromString("isEmpty");

    Symbol refSym() { // delete Names.Ref
	return defs.getType( Names.scala_Ref ).symbol() ;
    }

    Symbol seqSym() {
	return defs.getType( Names.scala_Seq ).symbol() ;
    }

    Symbol intSym() {
	return defs.getType( Names.scala_Int ).symbol() ;
    }

    Symbol tuple2Sym() {
	return defs.getType( TUPLE2_N ).symbol() ;
    }

    Symbol iteratorSym() {
	return defs.getType( Names.scala_Iterator ).symbol() ;
    }

    Symbol seqListSym() {
	return defs.getType( Names.scala_List ).symbol() ;
    }

    Symbol seqListSym_isEmpty() {
	return seqListSym().lookup( ISEMPTY_N );
    }

    Symbol seqListSym_head() {
	return seqListSym().lookup( HEAD_N );
    }

    Symbol seqListSym_tail() {
	return seqListSym().lookup( TAIL_N );
    }

    /*
    Symbol seqConsSym() {
	return defs.getType( Names.scala_COLONCOLON ).symbol() ;
    }

    Symbol seqNilSym() {
	return defs.getType( Names.scala_Nil ).symbol().module(); // no need for TypeApply anymore!x
    }
    */
    Symbol seqIterSym() {
	return defs.getType( Names.scala_Iterator ).symbol();
    }

    Symbol seqIterSym_next() {
	Scope scp = seqIterSym().members(); return scp.lookup( Names.next );
    }

    Symbol seqIterSym_hasNext() {
	Scope scp = seqIterSym().members(); return scp.lookup( Names.hasNext );
    }
    /*

    Symbol seqTraceSym() {
	return defs.getType( Name.fromString( "scala.SeqTrace" ) ).symbol();
    }
    Symbol seqTraceConsSym() {
	return defs.getType( Name.fromString( "scala.SeqTraceCons" ) ).symbol();
    }

    Symbol seqTraceNilSym() {
	return defs.getType( Name.fromString( "scala.SeqTraceNil" ) ).symbol();
    }
    */
    Symbol iterableSym() {
	return defs.getType( Names.scala_Iterable ).symbol();
    }

    Symbol newIterSym() {
	Scope scp = iterableSym().members(); return scp.lookup( Names.elements );
    }

    Symbol andSym() {
        return defs.BOOLEAN_CLASS.lookup(AND_N);
    }

    Symbol orSym() {
        return defs.BOOLEAN_CLASS.lookup(OR_N);
    }

    Symbol failSym() {
        return defs.SCALA_CLASS.lookup(MATCHERROR_N).lookup(FAIL_N);
    }


    public CodeFactory( Unit unit, Infer infer, int pos ) {
	super( unit, infer );
	this.pos = pos;
    }

    // --------- these are new

    /** If  ... pos, type is copied from thenBody
     */
    Tree If( Tree cond, Tree thenBody, Tree elseBody ) {
	assert cond != null:"cond is null";
	assert thenBody != null:"thenBody is null";
	assert elseBody != null:"elseBody is null";
	return gen.If( thenBody.pos, cond, thenBody, elseBody );
    }

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
	    result = If( condition[ i ],
			 body[ i ],
			 result
			 ).setType( result.type );

	return result ;
    }

    /** returns `List[ elemType ]' */
      Type SeqListType( Type elemType ) {
            return Type.TypeRef( defs.SCALA_TYPE,
                                 seqListSym(),
                                 new Type[] { elemType });
      }

    /** returns  `List[ Tuple2[ scala.Int, <elemType> ] ]' */
      Type SeqTraceType( Type elemType ) {
	  Type t = Type.TypeRef( defs.SCALA_TYPE,
                                 seqListSym(),
                                 new Type[] { pairType( defs.INT_TYPE,
							elemType ) });
	  //System.err.println("CodeFactory::SeqTraceType -"+ t );
	  return t;
      }

    /**  returns `Iterator[ elemType ]' */
    Type _seqIterType( Type elemType ) {
	Symbol seqIterSym = defs.getType( Names.scala_Iterator ).symbol();

	return Type.TypeRef( defs.SCALA_TYPE, seqIterSym(),
			     new Type[] { elemType });
    }

    /**  returns `<seqObj.elements>' */
    Tree newIterator( Tree seqObj, Type elemType ) {
	Symbol newIterSym = newIterSym();
	Tree t1 = gen.Select( pos, seqObj, newIterSym)
	    .setType( Type.MethodType(new Symbol[] {},_seqIterType( elemType )));

	Tree theIterator = gen.Apply(seqObj.pos,
				     t1,
				     Tree.EMPTY_ARRAY)
	    .setType( _seqIterType( elemType ) );

	return theIterator;

    }

    /** returns code `<seqObj>.elements'
     *  the parameter needs to have type attribute `Sequence[<elemType>]'
     */
    Tree newIterator( Tree seqObj ) {
	return newIterator( seqObj, getElemType_Sequence( seqObj.type() ));
    }

    /** code `Nil'
        Tree _seqTraceNil( Type elemType ) {
	return newSeqNil( null );
    }
     */

      //                       `SeqCons[ elemType ]'
    /*

      Type _seqConsType( Type elemType ) {
            return Type.TypeRef( defs.SCALA_TYPE,
                                 seqConsSym(),
                                 new Type[] { elemType });
      }

    Tree newSeqNil( Type tpe ) {
	return gen.Select(gen.Ident(pos, defs.SCALA), seqNilSym());
    }
    */

    // EXPERIMENTAL
    Tree newRef( Tree init ) {
	//System.out.println( "hello:"+refSym().type() );
	return gen.New(gen.mkPrimaryConstr(pos, refSym(),
			                    new Type[] { init.type() },
			                    new Tree[] { init } ));
    }

    /** returns A for T <: Sequence[ A ]
     */
    Type getElemType_Sequence( Type tpe ) {
	//System.err.println("getElemType_Sequence("+tpe.widen()+")");
	Type tpe1 = tpe.widen().baseType( seqSym() );

	if( tpe1 == Type.NoType )
	    throw new ApplicationError("arg "+tpe+" not subtype of Sequence[ A ]");

	return tpe1.typeArgs()[ 0 ];
    }

    /** returns A for T <: Iterator[ A ]
     */
    Type getElemType_Iterator( Type tpe ) {
	//System.err.println("getElemType_Iterator("+tpe+")");

	Type tpe1 = tpe.widen().baseType( iteratorSym() );

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
	return gen.mkApply__(gen.Select(iter, seqIterSym_next()));
    }

    /** `it.hasNext()'
     */
    public Tree _hasNext( Tree iter ) {
	return gen.mkApply__(gen.Select(iter, seqIterSym_hasNext()));
    }

    /** `!it.hasCur()'
     */
    public Tree _not_hasNext( Tree iter ) {
	return gen.mkApply__(gen.Select(_hasNext(iter), notSym));
    }

      /** `trace.isEmpty'
       */
      public Tree isEmpty( Tree iter ) {
          return gen.mkApply__(gen.Select(iter, seqListSym_isEmpty()));
      }

    Tree SeqTrace_headElem( Tree arg ) { // REMOVE SeqTrace
	Scope scp = seqListSym().members();
	Symbol headSym = scp.lookup ( HEAD_N );
	assert headSym != Symbol.NONE;
	scp = tuple2Sym().members();
	Symbol leftSym = scp.lookup ( RIGHT_N );
	assert leftSym != Symbol.NONE;
	Tree t =  gen.Apply( gen.Select( pos, arg, headSym ),
			     Tree.EMPTY_ARRAY );
	return gen.Apply( gen.Select( pos, t, leftSym ),
			  Tree.EMPTY_ARRAY );
    }

    Tree SeqTrace_headState( Tree arg ) { // REMOVE SeqTrace
	Scope scp = seqListSym().members();
	Symbol headSym = scp.lookup ( HEAD_N );
	assert headSym != Symbol.NONE;
	scp = tuple2Sym().members();
	Symbol leftSym = scp.lookup ( LEFT_N );
	assert leftSym != Symbol.NONE;
	Tree t =  gen.Apply( gen.Select( pos, arg, headSym ),
			     Tree.EMPTY_ARRAY );
	t =  gen.Apply( gen.Select( pos, t, leftSym ),
			Tree.EMPTY_ARRAY );
	return t;

    }

    Tree SeqTrace_tail( Tree arg ) {
	Scope scp = seqListSym()/*seqTraceSym()*/.members(); // REMOVE SeqTrace
	Symbol tailSym = scp.lookup ( TAIL_N );
	assert tailSym != Symbol.NONE;
	//System.err.println( "CodeFactory::SeqTrace_tail " + tailSym +" "+ tailSym.type() );
	return gen.Apply( gen.Select( pos, arg, tailSym ),
			  Tree.EMPTY_ARRAY );
    }

    /** `<seqlist>.head()'
     */
    Tree SeqList_head( Tree arg ) {
	Scope scp = seqListSym().members();
	Symbol headSym = scp.lookup ( HEAD_N );
	assert headSym != Symbol.NONE;
	return gen.Apply( gen.Select( pos, arg, headSym ),
			  Tree.EMPTY_ARRAY );
    }

    /** return the analyzed type
     */
    public Type typeOf(Symbol sym) {
        return sym.type();
        //return sym.typeAt(unit.global.ANALYZER_PHASE.id);
    }


    protected Tree Block(int pos, Tree[] ts, Type tpe) {
        if (ts.length == 1)
            return ts[0];
        else if (ts.length > 1)
            switch (ts[ts.length - 1]) {
	    case Block(Tree[] ts0):
		Tree[] ts1 = new Tree[ts0.length + ts.length - 1];
		System.arraycopy(ts, 0, ts1, 0, ts.length - 1);
		System.arraycopy(ts0, 0, ts1, ts.length - 1, ts0.length);
		return Block(pos, ts1, tpe);
            }
        return gen.Block(pos, ts);
    }

     // unused
       public Tree Negate(Tree tree) {
       switch (tree) {
       case Literal(Object value):
       return gen.mkBooleanLit(tree.pos, !((Boolean)value).booleanValue());
       }
       return gen.Apply(tree.pos, gen.Select(tree, notSym));
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
        return gen.mkApply_V(gen.Select(left, andSym()), new Tree[]{right});
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
        return gen.Apply(gen.Select(left, orSym()), new Tree[]{right});
    }

    protected Tree Equals(Tree left, Tree right) {
        Symbol fun = unit.global.definitions.EQEQ;
        return gen.mkApply_V(gen.Select(left, fun), new Tree[]{right});
    }

    protected Tree ThrowMatchError(int pos, Type type) {
        return gen.mkApplyTV(
			     gen.mkRef(pos, failSym()),
                             new Tree[]{gen.mkType(pos, type)},
                             new Tree[]{
                                 gen.mkStringLit(pos, unit.toString()),
                                 gen.mkIntLit(pos, Position.line(pos))
                             });
    }


    Type pairType( Type left, Type right ) {
	return Type.TypeRef( defs.SCALA_TYPE,
			     tuple2Sym() ,
			     new Type[] { left, right } );
    }

    Tree newPair( Tree left, Tree right ) {
 	return gen.New(gen.mkPrimaryConstr(pos, tuple2Sym(),
			                   new Type[] { left.type(), right.type() },
			                   new Tree[] { left, right }));

    }

}
