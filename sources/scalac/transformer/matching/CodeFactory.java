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

    /**                       `SequenceIterator[ elemType ]' // TODO: Move to TypeFactory
     */
    Type _seqIterType( Type elemType ) {
	Symbol seqIterSym = defs.getType( Names.scala_Iterator ).symbol();

	return Type.TypeRef( defs.SCALA_TYPE/*PREFIX*/,
			     seqIterSym,
			     new Type[] { elemType });
    }

    /** returns code `<seqObj>.newIterator'
     *  the parameter needs to have type attribute `Sequence[<elemType>]'
     *  it is not checked whether seqObj really has type `Sequence'
     */
    Tree newIterator( Tree seqObj ) {
	Type args[] = seqObj.type().typeArgs();
	assert ( args.length== 1 ) : "seqObj does not have right type";
	Type elemType = args[ 0 ];
	//System.out.println( "elemType:"+elemType );

	//Tree t1 = gen.Select(seqObj, newIterSym);

	Scope scp = defs.getClass( Names.scala_Seq ) /* sequenceSym */
	    .members();
	Symbol newIterSym = scp.lookup/*Term */( Names.elements );

	Tree t1 = make.Select( Position.NOPOS, seqObj, newIterSym.name )
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

    // the caller needs to set the type !
    Tree  _applyNone( Tree arg ) {
	return make.Apply(Position.NOPOS, arg, Tree.EMPTY_ARRAY );
    }

    // todo: more defensive checking
    Type getElemType( Type seqType ) {
	Type[] args = seqType.typeArgs(); /*use typeArgs instead of args*/
	assert (args.length==1);
	return args[ 0 ];

    }

    /** `it.next()'
     */
    public Tree _next( Tree iter ) {
	Type elemType = getElemType( iter.type() );

	Scope scp = defs.getType( Names.scala_Iterator ).symbol() /* seqIterSym */
	    .members();

	Symbol nextSym = scp.lookup( Names.next );

	return _applyNone( gen.Select( iter, nextSym ))
	    .setType( iter.type() );
    }

    /** `it.hasNext()'
     */
    public Tree _hasNext( Tree iter ) {

	Scope scp = defs.getType( Names.scala_Iterator ).symbol() /* seqIterSym */
	    .members();

	Symbol hasNextSym = scp.lookup( Names.hasNext );

	return _applyNone( gen.Select( iter, hasNextSym ))
	    .setType( defs.BOOLEAN_TYPE );
    }

    /** `!it.hasCur()'
     */
    public Tree _not_hasNext( Tree iter ) {
	return _applyNone( gen.Select( _hasNext( iter ), notSym ))
	    .setType( defs.BOOLEAN_TYPE );
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
