package scalac.transformer.matching ;

import scalac.*;
import scalac.ast.*;
import scalac.symtab.*;
import Tree.*;
import scalac.util.Name;
import scalac.util.Names;

import scalac.transformer.TransMatch.Matcher ;

import java.util.* ;

import ch.epfl.lamp.util.Position;

public class LeftTracerInScala extends TracerInScala {

      Tree selector;

      /** symbol of the accumulator ( scala.SequenceList )
       */
      Symbol accumSym;

      Type _accumType( Type elemType ) {
            return cf.SeqTraceType( elemType );
      }


      Matcher _m ;
      public LeftTracerInScala( DetWordAutom dfa,
                                Type elementType,
                                Matcher m,
                                CodeFactory cf ) {

	  super( dfa, elementType, m.owner, cf );
	  this._m = m;
	  this.selector = m.selector;
	  //helpMap = new HashMap(); moved up
	  helpVarDefs = new Vector();

      }

    Vector  helpVarDefs;

    Symbol makeHelpVar( Symbol realVar ) {
	Symbol helpVar = new TermSymbol( pos,
					 cf.fresh.newName( realVar.name
							   .toString() ),
					 owner,
					 0)
	    .setType( cf.SeqListType( elementType ) ) ;

	helpMap.put( realVar, helpVar );

	Tree varDef = gen.ValDef(helpVar, gen.Nil(cf.pos /*cf.newSeqNil elementType*/ ));
	// set mutable flag of symbol helpVar ??
	helpVarDefs.add( varDef );
	return helpVar;
    }

    Symbol makeHelpVarSEQ( Tree pat ) {
	String  helpName = String.valueOf( pat.hashCode() ); //wicked, in'it ?
	Symbol helpVar =
	    new TermSymbol( pos,
			    cf.fresh.newName(Name.fromString( helpName )),
			    owner,
			    0)
	    .setType( cf.SeqListType( pat.type() )) ;

	ValDef varDef = (ValDef) gen.ValDef( helpVar,
					     gen.mkDefaultValue(cf.pos,
								cf.SeqListType( pat.type() ))
					     //cf.ignoreValue( )
					     );
	helpVarDefs.add( varDef );
	return helpVar;
    }

    protected void initializeSyms() {
	funSymName = "leftTracer";

	nestedMap = new HashMap();

	super.initializeSyms();

	this.accumSym = newParam("accum")                    // accumulator
	    .setType( _accumType( elementType ));

	this.funSym
	    .setType( new Type.MethodType( new Symbol[] {
		accumSym, iterSym, stateSym},
					   _accumType( elementType )));

	// 2 do: rename switchresultsym to something else...

	this.resultSym = new TermSymbol( //Kinds.VAR,
					pos,
					cf.fresh.newName("trace"),
					owner,
					0 )
	    .setType( _accumType( elementType ) ) ;
    }

    // should throw an exception here really, e.g. MatchError
    Tree code_fail() {
	return gen.Ident( accumSym.pos, accumSym );

    }

    /** returns translation of transition with label from i.
     *  returns null if there is no such transition(no translation needed)
     */
    Tree code_delta( int i, Label label ) {
	Integer target = dfa.delta( i, label );
	/*
	  System.out.println("LeftTracer:calling dfa.delta("+i+","+label+")");
	  System.out.println("result: "+target);
	*/
	if( target == null )
	    return null;

	// (optimization) that one is a dead state (does not make sense for tracer)
	/*
	  if( target == dfa.nstates - 1 )
	  return code_fail();
	*/

	/*
	Tree newAcc = cf.newSeqTraceCons(new Integer(i),
					 currentElem(),
					 _ref( accumSym ));
	*/
	Tree hd = cf.newPair( gen.mkIntLit(cf.pos, i), currentElem() );
	Tree newAcc = gen.Cons(cf.pos,
			       hd.type,
			       hd,
			       gen.Ident( cf.pos, accumSym ));

	return callFun( new Tree[] { newAcc , _iter(), gen.mkIntLit( cf.pos, target )} );
    }


    public Tree code_body() {

	Tree body = code_fail(); // never reached at runtime.

	// state [ nstates-1 ] is the dead state, so we skip it

	//`if( state == q ) <code_state> else {...}'
	for( int i = dfa.nstates-2; i >= 0; i-- ) {
	    body = code_state( i, body );
	}

	return loadCurrentElem( body );

    }

    /** return code for state i of the dfa SAME AS IN SUPER, ONLY SINK IS GONE
     */
    Tree code_state( int i, Tree elseBody ) {

	Tree runFinished; // holds result of the run
	int  finalSwRes;

	runFinished = run_finished( i );

	Tree stateBody ; // action(delta) for one particular label/test

	// default action (fail if there is none)

	stateBody = code_delta( i, Label.DefaultLabel);

	if( stateBody == null )
	    stateBody = code_fail();
	// transitions of state i

	HashMap trans = ((HashMap[])dfa.deltaq)[ i ];

	for( Iterator labs = ((HashMap)dfa.deltaq( i )).keySet().iterator();
	     labs.hasNext() ; ) {
	    Object label = labs.next();
	    Integer next = (Integer) trans.get( label );

	    Tree action = code_delta( i, (Label) label );

	    if( action != null ) {
		stateBody = gen.If( _cur_eq( _iter(), (Label) label ),
				    action,
				    stateBody);
	    }
	}
	stateBody = gen.If( cf.Negate( gen.Ident( cf.pos, hasnSym )),
			    runFinished,
			    stateBody );
	return gen.If( cf.Equals( _state(), gen.mkIntLit(cf.pos, i )),
		       stateBody ,
		       elseBody );
    }

    Tree[] getTrace() {

	initializeSyms();
	Tree tb = code_body();
	theDefDef = gen.DefDef( this.funSym,
				tb );

	Vector v = new Vector();

	v.addAll( helpVarDefs );

	//
	// `def leftTracer(...) = ...'                 the function definition
	v.add( theDefDef );

	Tree emptyAcc    = gen.Nil( cf.pos ); //cf._seqTraceNil( elementType );

	// the valdef is needed, because passing emptyAcc as a parameter
	//   results in a ClassCastException at runtime (?!)

	Symbol emptyAccSym = new TermSymbol( pos,
					     cf.fresh.newName("acc"),
					     owner,
					     0 )
	    .setType( _accumType( elementType ) ) ;

	// `val acc = SeqNil[ elementType ]'                 init accumulator
	v.add( gen.ValDef( emptyAccSym, emptyAcc) );

	Tree run = callFun( new Tree[] {
	    gen.Ident( pos, emptyAccSym ),
	    cf.newIterator( selector, selector.type() ),
	    gen.mkIntLit( cf.pos, 0 )  });

	run = gen.ValDef( resultSym, run );

	v.add( run );

	// vars...
	for( Iterator it = helpMap.keySet().iterator(); it.hasNext(); ) {
	    v.add( bindVar( (Symbol) it.next()) );
	}

	/* IF YOU NEED DEBUG OUTPUT AT RUNTIME
	   v.add( cf.debugPrintRuntime( "the trace is" ) );
	   v.add( cf.debugPrintRuntime( gen.Ident( pos, resultSym ) ) );
	   v.add( cf.debugPrintNewlineRuntime( "" ) );
	*/

	Tree res[] = new Tree[ v.size() ];
	int j = 0;
	for( Iterator it = v.iterator(); it.hasNext(); )
	    res[ j++ ] = (Tree) it.next();

	_m.varMap = nestedMap;

	return res;

    }

    public HashMap nestedMap;

    // calling the AlgebraicMatcher here
    Tree _cur_match( Tree pat ) {
	//System.out.println("calling algebraic matcher on type:"+pat.type);

	Matcher m = new Matcher( funSym,
				 currentElem(),
				 defs.BOOLEAN_TYPE );

	if( CollectVariableTraverser.containsBinding( pat )) {
	    switch( pat ) {
	    case Sequence(Tree[] pats):
		//System.out.println("ouch! v Left");
		Symbol hv = makeHelpVarSEQ( pat );
		nestedMap.put( pat, hv );
		Tree stm  = gen.Assign( gen.Ident(Position.FIRSTPOS, hv), currentElem() );
		m.stms = new Tree[2];
		m.stms[0] = stm;
		m.stms[1] = gen.mkBooleanLit(Position.FIRSTPOS, true);
		return gen.mkBlock(m.stms);
	    }
	}

	HashMap helpMap = FreshVariableTraverser.getVars( pat, owner, cf.fresh );
	//System.out.println("varMap: "+helpMap );

	m.varMap = helpMap;

	//replaceVars( pat );

	am.construct( m, new CaseDef[] {
	    cf.gen.CaseDef( pat,
                            gen.mkBooleanLit( cf.pos, true )),
	    cf.gen.CaseDef( cf.gen.Ident(pat.pos, defs.PATTERN_WILDCARD),
                            gen.mkBooleanLit( cf.pos, false)) },
            false);
	Tree res = am.toTree();
	return res;
    }


    /** return the accumulator + last state
     */
    Tree run_finished( int state ) {
	Tree hd = cf.newPair( gen.mkIntLit( cf.pos, state ),
			      gen.mkDefaultValue(cf.pos,
						 elementType)
			      /*cf.ignoreValue( elementType )*/);
	//System.err.println(hd.type);
	return gen.Cons(  cf.pos,
			  hd.type(),
			  hd,
			  gen.Ident( cf.pos, accumSym ));
    }

}
