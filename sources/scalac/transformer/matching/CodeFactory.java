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

    private int pos = Position.NOPOS ;

      static final Name HEAD_N       = Name.fromString("head");

      static final Name TAIL_N       = Name.fromString("tail");

      static final Name SEQ_TRACE_N      = Name.fromString("scala.SeqTrace");

      static final Name HEADSTATE_N       = Name.fromString("headState");

      static final Name HEADELEM_N        = Name.fromString("headElem");

      static final Name ISEMPTY_N        = Name.fromString("isEmpty");



    Symbol seqListSym() {
	return defs.getType( Name.fromString("scala.List") ).symbol() ;
    }

    Symbol seqConsSym() {
	return defs.getType( Name.fromString("scala.::") ).symbol() ;
    }

    Symbol seqNilSym() {
	return defs.getType( Name.fromString( "scala.Nil" ) ).symbol(); // no need for TypeApply anymore!x
    }

    Symbol seqIterSym() {
	return defs.getType( Name.fromString( "scala.Iterator" ) ).symbol();
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

    public CodeFactory( Unit unit, Infer infer ) {
	super( unit, infer );
    }

    // --------- these are new

    /** If  ... pos, type is copied from thenBody
     */
    Tree If( Tree cond, Tree thenBody, Tree elseBody ) {
	//assert( thenBody.type().equals( elseBody.type() )
	//       || thenBody.type().isSubType( elseBody.type() )
	//        /*|| elseBody.type().isSubType( thenBody.type()  BUG  */)
	//    : "you try to construct a naughty if "
	//     + "thenBody: "+thenBody.type+" elseBody:"+elseBody.type;
	assert cond != null:"cond is null";
	assert thenBody != null:"thenBody is null";
	assert elseBody != null:"elseBody is null";
	return make.If( thenBody.pos,
			cond,
			thenBody,
			elseBody ).setType( elseBody.type() );
    }

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

      //                       `SeqList[ elemType ]'
      Type SeqListType( Type elemType ) {
            return Type.TypeRef( defs.SCALA_TYPE,
                                 seqListSym(),
                                 new Type[] { elemType });
      }

      //                       `SeqTrace[ elemType ]'
      Type SeqTraceType( Type elemType ) {
            return Type.TypeRef( defs.SCALA_TYPE,
                                 seqTraceSym(),
                                 new Type[] { elemType });
      }

    /**                       `SequenceIterator[ elemType ]' // TODO: Move to TypeFactory
     */
    Type _seqIterType( Type elemType ) {
	Symbol seqIterSym = defs.getType( Names.scala_Iterator ).symbol();

	return Type.TypeRef( defs.SCALA_TYPE/*PREFIX*/,
			     seqIterSym(),
			     new Type[] { elemType });
    }

    /** returns code `<seqObj>.elements'
     *  the parameter needs to have type attribute `Sequence[<elemType>]'
     *  it is not checked whether seqObj really has type `Sequence'
     */
    Tree newIterator( Tree seqObj ) {
	Type elemType = getElemType( seqObj.type() );
	//System.out.println( "elemType:"+elemType );

	//Tree t1 = gen.Select(seqObj, newIterSym);

	Scope scp = defs.getClass( Names.scala_Iterable ) /* sequenceSym */
	    .members();
	Symbol newIterSym = scp.lookup/*Term */( Names.elements );

	Tree t1 = make.Select( Position.NOPOS, seqObj, newIterSym.name ) /*todo: newIterSym() */
	    .setSymbol( newIterSym )
	    .setType( Type.MethodType(new Symbol[] {},_seqIterType( elemType )));

	//System.out.println( "t1.type:"+t1.type() );

	Tree theIterator = gen.Apply(seqObj.pos,
				     t1,
				     Tree.EMPTY_ARRAY)
	    .setType( _seqIterType( elemType ) );

	//System.out.println( "theit.type:"+theIterator.type() );

	return theIterator;

    }

      Tree ignoreValue( Type asType ) {
            if( asType.isSameAs(defs.BYTE_TYPE ))
                  return make.Literal(Position.NOPOS, new Long( 0 ))
                        .setType( defs.INT_TYPE );
            else if( asType.isSameAs( defs.CHAR_TYPE ))
                  return make.Literal(Position.NOPOS, new Long( 0 ))
                        .setType( defs.CHAR_TYPE );
            else if( asType.isSameAs(defs.SHORT_TYPE ))
                  return make.Literal(Position.NOPOS, new Long( 0 ))
                        .setType( defs.SHORT_TYPE );
            else if( asType.isSameAs(defs.INT_TYPE ))
                  return Int( 0 );
            else if( asType.isSameAs(defs.LONG_TYPE ))
                  return make.Literal(Position.NOPOS, new Long( 0 ))
                        .setType( defs.LONG_TYPE );
            else if( asType.isSameAs(defs.FLOAT_TYPE ))
                  return make.Literal(Position.NOPOS, new Long( 0 ))
                        .setType( defs.FLOAT_TYPE );
            else if( asType.isSameAs(defs.DOUBLE_TYPE ))
                  return make.Literal(Position.NOPOS, new Long( 0 ))
                        .setType( defs.DOUBLE_TYPE );
            else if( asType.isSameAs(defs.BOOLEAN_TYPE ))
                  return gen.mkBooleanLit(Position.NOPOS, false);
            else if( asType.isSameAs(defs.STRING_TYPE ))
                  return make.Literal(Position.NOPOS, "")
                        .setType( defs.STRING_TYPE );
            /** FIX ME FOR THE NEW VERSION
		else
                  return gen.Apply( Null( asType ),
                                    Tree.EMPTY_ARRAY);
	    */
	    return null;  // should not happen FIXME
      }

    /** FIX ME FOR THE NEW VERSION
      Tree Null( Type asType ) {
            return gen.TypeApply(pos, gen.mkId(pos, defs.NULL ),
                                 new Tree[] { gen.mkType(pos, asType) } );
      }
    */

    // the caller needs to set the type !
    Tree  _applyNone( Tree arg ) {
	return make.Apply(Position.NOPOS, arg, Tree.EMPTY_ARRAY );
    }

    /** code `new SeqTraceNil[ elemType ]()'
     */

    Tree _seqTraceNil( Type elemType ) {
	assert elemType != null : "elemType = null??";
	return gen.New( Position.NOPOS, defs.SCALA_TYPE, seqTraceNilSym(),
			new Type[] { elemType },
			new Tree[] {});
    }


      /** creates an scala.Int constant
       */
      Tree Int( int val ) {
            return make.Literal(Position.NOPOS, new Long((long)val))
                  .setType( defs.INT_TYPE );
      }

      Tree Int( Integer valI ) {
            return Int( valI.intValue() );
      }

      /** code `new SeqTraceCons[ elemType ]( state, head, tail )'
       */
      Tree newSeqTraceCons(  Integer state, Tree head, Tree tail ) {
            return gen.New( Position.NOPOS, defs.SCALA_TYPE, seqTraceConsSym(),
                            new Type[] { head.type() },
                            new Tree[] { Int( state ), head, tail });
      }


      /*
      Type _seqTraceConsType( Type elemType ) {
            return Type.TypeRef( defs.SCALA_TYPE,
                                 seqTraceConsSym,
                                 new Type[] { elemType });
      }
      */

      //                       `SeqCons[ elemType ]'

      Type _seqConsType( Type elemType ) {
            return Type.TypeRef( defs.SCALA_TYPE,
                                 seqConsSym(),
                                 new Type[] { elemType });
      }

    Tree newSeqNil( Type tpe ) {
	assert tpe != null :"tpe = null !?";
	return gen.New( Position.NOPOS, defs.SCALA_TYPE, seqNilSym(),
			new Type[] { tpe },
			new Tree[] {});
    }

    Tree newSeqCons( Tree head, Tree tail ) {
	return gen.New( Position.NOPOS, defs.SCALA_TYPE, seqConsSym(),
			new Type[] { head.type() },
			new Tree[] { head, tail });
    }

    Type getElemType( Type seqType ) {
	System.err.println("getElemType("+seqType+")");
	Symbol seqClass = defs.getType( Name.fromString("scala.Seq") ).symbol();
	assert seqClass != Symbol.NONE : "did not find Seq";

	Type seqType1 = seqType.baseType( seqClass );

	switch( seqType1 ) {
	case TypeRef(_,_,Type[] args):
	    assert (args.length==1) : "weird type:"+seqType;
	    return args[ 0 ];
	default:
	    return seqType;
	}
    }

    /** `it.next()'
     */
    public Tree _next( Tree iter ) {
	Type elemType = getElemType( iter.type() );

	Scope scp = defs.getType( Names.scala_Iterator ).symbol() /* seqIterSym */
	    .members();

	Symbol nextSym = scp.lookup( Names.next );

	return _applyNone( gen.Select( iter, nextSym )) /* todo: nextSym() */
	    .setType( iter.type() );
    }

    /** `it.hasNext()'
     */
    public Tree _hasNext( Tree iter ) {

	Scope scp = defs.getType( Names.scala_Iterator ).symbol() /* seqIterSym */
	    .members();

	Symbol hasNextSym = scp.lookup( Names.hasNext );

	return _applyNone( gen.Select( iter, hasNextSym )) /* todo: hasNextSym() */
	    .setType( defs.BOOLEAN_TYPE );
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
            Scope scp = seqTraceSym().members();
            Symbol isEmptySym = scp.lookup ( ISEMPTY_N );
            return _applyNone( gen.Select( iter, isEmptySym ))
                  .setType( defs.BOOLEAN_TYPE );
      }

      Tree SeqTrace_headElem( Tree arg ) {
            Scope scp = seqTraceSym().members();
            Symbol headSym = scp.lookup ( HEADELEM_N );
            assert headSym != Symbol.NONE;
            return gen.Apply( gen.Select( pos, arg, headSym ),
                              Tree.EMPTY_ARRAY );
      }

      Tree SeqTrace_headState( Tree arg ) {
            Scope scp = seqTraceSym().members();
            Symbol headSym = scp.lookup ( HEADSTATE_N );
            assert headSym != Symbol.NONE;
            return gen.Apply( gen.Select( pos, arg, headSym ),
                              Tree.EMPTY_ARRAY );
      }

      Tree SeqTrace_tail( Tree arg ) {
            Scope scp = seqTraceSym().members();
            Symbol tailSym = scp.lookup ( TAIL_N );
            assert tailSym != Symbol.NONE;
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
        return sym.typeAt(unit.global.PHASE.ANALYZER.id);
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

    /* // unused
       protected Tree Negate(Tree tree) {
       switch (tree) {
       case Literal(Object value):
       return gen.mkBooleanLit(tree.pos, !((Boolean)value).booleanValue());
       }
       return make.Apply(
       tree.pos,
       gen.Select(tree, NOT_N),
       Tree.EMPTY_ARRAY).setType(defs.BOOLEAN_TYPE);
       }
    */
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

}
