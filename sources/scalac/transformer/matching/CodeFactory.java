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

    private int pos = Position.FIRSTPOS ;

      static final Name TUPLE2_N       = Name.fromString("scala.Tuple2");

    static final Name LEFT_N = Name.fromString("_1");
    static final Name RIGHT_N = Name.fromString("_2");
      static final Name HEAD_N       = Name.fromString("head");

      static final Name TAIL_N       = Name.fromString("tail");

      static final Name SEQ_TRACE_N      = Name.fromString("scala.SeqTrace");

      static final Name HEADSTATE_N       = Name.fromString("headState");

      static final Name HEADELEM_N        = Name.fromString("headElem");

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

    Symbol seqConsSym() {
	return defs.getType( Names.scala_COLONCOLON ).symbol() ;
    }

    Symbol seqNilSym() {
	return defs.getType( Names.scala_Nil ).symbol(); // no need for TypeApply anymore!x
    }

    Symbol seqIterSym() {
	return defs.getType( Names.scala_Iterator ).symbol();
    }

    Symbol seqIterSym_next() {
	Scope scp = seqIterSym().members(); return scp.lookup( Names.next );
    }

    Symbol seqIterSym_hasNext() {
	Scope scp = seqIterSym().members(); return scp.lookup( Names.hasNext );
    }

    Symbol seqTraceSym() {
	return defs.getType( Name.fromString( "scala.SeqTrace" ) ).symbol();
    }

    Symbol seqTraceConsSym() {
	return defs.getType( Name.fromString( "scala.SeqTraceCons" ) ).symbol();
    }

    Symbol seqTraceNilSym() {
	return defs.getType( Name.fromString( "scala.SeqTraceNil" ) ).symbol();
    }

    Symbol iterableSym() {
	return defs.getType( Names.scala_Iterable ).symbol();
    }

    Symbol newIterSym() {
	Scope scp = iterableSym().members(); return scp.lookup( Names.elements );
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
	return make.If( thenBody.pos,
			cond,
			thenBody,
			elseBody ).setType( elseBody.type() );
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

    /** code for a real switch statement
     */
	Tree Switch(Tree selector,
	            int[] tags,
	            Tree[] bodies,
	            Tree defaultBody) {
		return make.Switch(selector.pos, selector, tags, bodies, defaultBody);
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

    /** FIXME - short type
     */
      Tree ignoreValue( Type asType ) {
            if( asType.isSameAs(defs.BYTE_TYPE ))
                  return make.Literal(pos, new Integer( 0 ))
                        .setType( defs.INT_TYPE );
            else if( asType.isSameAs( defs.CHAR_TYPE ))
                  return make.Literal(pos, new Character( 'a' ))
                        .setType( defs.CHAR_TYPE );
            else if( asType.isSameAs(defs.SHORT_TYPE ))
		return make.Literal(pos, new Integer/*Short?*/( 0 ))
                        .setType( defs.SHORT_TYPE );
            else if( asType.isSameAs(defs.INT_TYPE ))
                  return Int( 0 );
            else if( asType.isSameAs(defs.LONG_TYPE ))
                  return make.Literal(pos, new Long( 0 ))
                        .setType( defs.LONG_TYPE );
            else if( asType.isSameAs(defs.FLOAT_TYPE ))
                  return make.Literal(pos, new Float( 0 ))
                        .setType( defs.FLOAT_TYPE );
            else if( asType.isSameAs(defs.DOUBLE_TYPE ))
                  return make.Literal(pos, new Double( 0 ))
                        .setType( defs.DOUBLE_TYPE );
            else if( asType.isSameAs(defs.BOOLEAN_TYPE ))
                  return gen.mkBooleanLit(pos, false);
            else if( asType.isSameAs(defs.STRING_TYPE ))
                  return make.Literal(pos, "")
                        .setType( defs.STRING_TYPE );
            /** FIX ME FOR THE NEW VERSION*/
	    else
		return /*gen.Apply( */Null( asType )/*,
						      Tree.EMPTY_ARRAY)*/;

	    //throw new ApplicationError("don't know how to handle "+asType);
      }

    /** code `null'
     */
    Tree Null( Type asType ) {
	return gen.Ident(pos, defs.NULL );
    }

    // the caller needs to set the type !
    Tree  _applyNone( Tree arg ) {
	return make.Apply(pos, arg, Tree.EMPTY_ARRAY );
    }

    /** code `Nil'
     */

    Tree _seqTraceNil( Type elemType ) {
	return newSeqNil( null );
    }


      /** creates an scala.Int constant
       */
      Tree Int( int val ) {
            return Int( new Integer( val ));
      }

      Tree Int( Integer valI ) {
            return make.Literal( pos, valI )
                  .setType( defs.INT_TYPE );

      }

      /** code `new SeqTraceCons[ elemType ]( state, head, tail )'
       */
      Tree newSeqTraceCons(  Integer state, Tree head, Tree tail ) {
	  return newSeqCons( newPair( Int(state),head ),tail );
      }

      //                       `SeqCons[ elemType ]'

      Type _seqConsType( Type elemType ) {
            return Type.TypeRef( defs.SCALA_TYPE,
                                 seqConsSym(),
                                 new Type[] { elemType });
      }

    Tree newSeqNil( Type tpe ) {
	return gen.Select(gen.Ident(pos, defs.SCALA), Names.Nil/*seqNilSym()*/);
    }

    // EXPERIMENTAL
    Tree newRef( Tree init ) {
	//System.out.println( "hello:"+refSym().type() );
	return gen.New( pos, defs.SCALA_TYPE, refSym(),
			new Type[] { init.type() },
			new Tree[] { init } );
    }

    Tree newSeqCons( Tree head, Tree tail ) {
	return gen.New( pos, defs.SCALA_TYPE, seqConsSym(),
			new Type[] { head.type() },
			new Tree[] { head, tail });
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
	Type elemType = getElemType_Iterator( iter.type() );
	Symbol nextSym = seqIterSym_next();
	return _applyNone( gen.Select( iter, nextSym )).setType( elemType );
    }

    /** `it.hasNext()'
     */
    public Tree _hasNext( Tree iter ) {

	Symbol hasNextSym = seqIterSym_hasNext();

	return _applyNone( gen.Select( iter, hasNextSym )).setType( defs.BOOLEAN_TYPE );
    }

    /** `!it.hasCur()'
     */
    public Tree _not_hasNext( Tree iter ) {
	return _applyNone( gen.Select( _hasNext( iter ), notSym ))
	    .setType( defs.BOOLEAN_TYPE );
    }

      /** `trace.isEmpty'
       */
      public Tree isEmpty( Tree iter ) {
	  Scope scp = seqListSym()/*TraceSym()*/.members();
            Symbol isEmptySym = scp.lookup ( ISEMPTY_N );
            return _applyNone( gen.Select( iter, isEmptySym ))
                  .setType( defs.BOOLEAN_TYPE );
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
            return gen.Apply( make.Select( pos,
                                           arg,
                                           HEAD_N )
                              .setType( headSym.type() )
                              .setSymbol( headSym ),
                              Tree.EMPTY_ARRAY);
      }

    /** return the analyzed type
     */
    public Type typeOf(Symbol sym) {
        return sym.type();
        //return sym.typeAt(unit.global.ANALYZER_PHASE.id);
    }

    /** return the analyzed type
     */
    public Type typeOf0(Symbol sym) {
        return sym.typeAt(unit.global.PHASE.ANALYZER.id());
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
        return make.Block(pos, ts).setType(tpe);
    }

     // unused
       public Tree Negate(Tree tree) {
       switch (tree) {
       case Literal(Object value):
       return gen.mkBooleanLit(tree.pos, !((Boolean)value).booleanValue());
       }
       return make.Apply(
       tree.pos,
       gen.Select(tree, NOT_N),
       Tree.EMPTY_ARRAY).setType(defs.BOOLEAN_TYPE);
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
        Symbol fun = left.type.lookup(AND_N);
        return make.Apply(
			  left.pos,
			  make.Select(
				      left.pos,
				      left,
				      AND_N).setType(typeOf(fun)).setSymbol(fun),
			  new Tree[]{right}).setType(defs.BOOLEAN_TYPE);
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
        Symbol fun = left.type.lookup(OR_N);
        return make.Apply(
			  left.pos,
			  make.Select(
				      left.pos,
				      left,
				      OR_N).setType(typeOf(fun)).setSymbol(fun),
			  new Tree[]{right}).setType(defs.BOOLEAN_TYPE);
    }

    protected Tree Is(Tree tree, Type type) {
        return
            make.Apply(
		       tree.pos,
		       make.TypeApply(
				      tree.pos,
				      make.Select(
						  tree.pos,
						  tree,
						  defs.IS.name).setType(typeOf(defs.IS)).setSymbol(defs.IS),
				      new Tree[]{gen.mkType(tree.pos, type)})
		       .setType(Type.MethodType(Symbol.EMPTY_ARRAY, defs.BOOLEAN_TYPE)),
		       Tree.EMPTY_ARRAY).setType(defs.BOOLEAN_TYPE);
    }

    protected Tree As(Tree tree, Type type) {
        return
            make.Apply(
		       tree.pos,
		       make.TypeApply(
				      tree.pos,
				      make.Select(
						  tree.pos,
						  tree,
						  defs.AS.name).setType(typeOf(defs.AS)).setSymbol(defs.AS),
				      new Tree[]{gen.mkType(tree.pos, type)})
		       .setType(Type.MethodType(Symbol.EMPTY_ARRAY, type)),
		       Tree.EMPTY_ARRAY).setType(type);
    }

    protected Tree Equals(Tree left, Tree right) {
        Symbol fun = left.type.lookup(EQUALS_N);
        switch (typeOf(fun)) {
	case OverloadedType(Symbol[] alts, Type[] alttypes):
	    //System.out.println("**** " + left.type);
	    Tree t = make.Select(left.pos, left, EQUALS_N);
	    //for (int i = 0; i < alttypes.length; i++)
	    //    System.out.println(alts[i] + ": " + alttypes[i]);
	    infer.methodAlternative(t, alts, alttypes,
				    new Type[]{right.type}, defs.BOOLEAN_TYPE);
	    return make.Apply(left.pos, t, new Tree[]{right}).setType(defs.BOOLEAN_TYPE);
	default:
	    //System.out.println("#### " + left.type + ": " + fun);
	    return make.Apply(
			      left.pos,
			      make.Select(
					  left.pos,
					  left,
					  EQUALS_N).setType(typeOf(fun)).setSymbol(fun),
			      new Tree[]{right}).setType(defs.BOOLEAN_TYPE);
        }
    }

    protected Tree ThrowMatchError(int pos, Type type) {
        Symbol matchErrorModule = defs.SCALA.members().lookup(MATCHERROR_N);
        outer: switch (typeOf(matchErrorModule)) {
	case OverloadedType(Symbol[] alts, Type[] alttypes):
	    for (int i = 0; i < alts.length; i++)
		switch (alttypes[i]) {
		case TypeRef(_, _, _):
		    matchErrorModule = alts[i];
		    break outer;
		}
        }
        Symbol failMethod = typeOf(matchErrorModule).lookup(FAIL_N);
        return
	    make.Apply(
		       pos,
		       make.TypeApply(
				      pos,
				      make.Select(
						  pos,
						  make.Select(
							      pos,
							      make.Ident(pos, Names.scala).setType(typeOf(defs.SCALA)).setSymbol(defs.SCALA),
							      MATCHERROR_N)
						  .setSymbol(matchErrorModule)
						  .setType(typeOf(matchErrorModule)),
						  FAIL_N).setType(typeOf(failMethod)).setSymbol(failMethod),
				      new Tree[]{gen.mkType(pos, type)})
		       .setType(((Type.PolyType) typeOf(failMethod)).result.subst(
										  typeOf(failMethod).typeParams(),
										  new Type[]{type})),
		       new Tree[]{
			   make.Literal(pos, unit.toString()).setType(defs.STRING_TYPE),
			   make.Literal(pos, new Integer(Position.line(pos))).setType(defs.INT_TYPE)
		       }).setType(type);
    }


    Type pairType( Type left, Type right ) {
	return Type.TypeRef( defs.SCALA_TYPE,
			     tuple2Sym() ,
			     new Type[] { left, right } );
    }

    Tree newPair( Tree left, Tree right ) {
 	return gen.New( pos, defs.SCALA_TYPE, tuple2Sym(),
			new Type[] { left.type(), right.type() },
			new Tree[] { left, right });

    }

}
