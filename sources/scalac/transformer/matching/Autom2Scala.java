package scalac.transformer.matching ;

import scalac.* ;
import scalac.typechecker.* ;
import scalac.symtab.Symbol ;
import scalac.symtab.Type ;
import scalac.symtab.TermSymbol ;
import scalac.symtab.Definitions ;
import scalac.ast.Tree;
import scalac.ast.TreeGen;
import scalac.util.Name;
import scalac.util.Names;
import Tree.*;

import scalac.transformer.TransMatch.Matcher ;
import java.util.* ;

import ch.epfl.lamp.util.Position;

public class Autom2Scala  {

    protected boolean optimize = true;

      static final Name HASNEXT = Name.fromString("hasnext");
      static final Name CURRENT_ELEM = Name.fromString("cur");

      final int FAIL = -1;

      DetWordAutom dfa;
      protected CodeFactory cf;

    //Vector freeVars;
    //Vector mdefs;

      Definitions defs;
      TreeGen     gen;

      /** owner of the pattern matching expression */
      protected Symbol owner;

      /** symbol of the matcher fun */
      Symbol funSym;

      /** symbol of the iterator ( scala.SequenceIterator ) */
      Symbol iterSym;

      /** symbol of the switching result ( scala.Int ) */
      Symbol resultSym;

      /** symbol of the state variable ( scala.Int ) */
      Symbol stateSym;

      protected Type elementType;

      public int pos;

      String funSymName;

      Symbol newFunSym( String prefix ) {
	  return new TermSymbol( pos,
				 cf.fresh.newName( prefix ),
				 owner,
				 0);
      }

      Symbol newParam( String prefix ) {
	  return new TermSymbol( /*Kinds.VAL, */
                                   pos,
                                   cf.fresh.newName( prefix ),
                                   funSym,
                                   0);
      }

      Type funRetType() {
            switch( funSym.type() ) {
            case MethodType( _, Type retType ):
                  return retType;
            }
            throw new RuntimeException();
      }


      Tree callFun( Tree[] args ) {
            return gen.Apply( pos,
                              gen.Ident( pos, funSym ),
                              args).setType( funRetType() );
      }


      /** init funSym, iterSym, stateSym, resultSym + allocate mdefs.
       *  a subclass overriding initializeSyms may change these
       *   (esp. funSym)
       */
      protected void initializeSyms() {
            if( funSymName == null )
                  funSymName = "matcher";
            // the function that does the matching

            this.funSym = newFunSym( funSymName );

            this.iterSym = newParam("iter")
                  .setType( cf._seqIterType( elementType ) ) ;

            this.stateSym = newParam("q")
                  .setType( defs.INT_TYPE ) ;

            this.resultSym = new TermSymbol( pos,
					     cf.fresh.newName("swRes"),
					     owner,
					     0 )
                  .setType( defs.INT_TYPE ) ;

            this.funSym
                  .setType( new Type.MethodType( new Symbol[] {
                        iterSym, stateSym },  defs.INT_TYPE ));

            this.curSym = new TermSymbol( pos,
                                          CURRENT_ELEM,
                                          funSym,
                                          0)
                  .setType( elementType );

            this.hasnSym = new TermSymbol( pos,
					   HASNEXT,
					   funSym,
					   0)
                  .setType( defs.BOOLEAN_TYPE );

      }


      public Autom2Scala( DetWordAutom dfa,
                          Type elementType,
                          Symbol owner,
                          CodeFactory cf ) {
            this.dfa = dfa;
            this.elementType = elementType;
            this.defs = cf.defs;
            this.gen = cf.gen;
            this.owner = owner;
            this.pos = Position.FIRSTPOS;
            this.cf = cf;
            this.am = new AlgebraicMatcher( cf.unit, cf.infer );
            //this.mdefs = new Vector();

            //this.freeVars = new Vector();
      }

      public Tree theDefDef;

      Symbol curSym;
      Symbol hasnSym;

    // overridden in TracerInScala
    Tree loadCurrentElem( Tree body ) {
	return cf.Block( Position.FIRSTPOS, new Tree[] {
	    cf.gen.ValDef( this.hasnSym,
			   cf._hasNext( _iter() ) ),
	    cf.gen.ValDef( this.curSym,
			   cf.If( _ref( hasnSym ),//cf._hasNext( _iter() ),
				  cf._next( _iter() ),
				  cf.ignoreValue( curSym.type() ))),
	    body },
			 body.type() );
    }

    Tree currentElem() {
	return gen.Ident(Position.FIRSTPOS, curSym);
    }

      Tree currentMatches( Label label ) {
            return _cur_eq( _iter(), label );
      }

      //
      // translation of automata to scala code
      //


      /** creates an int variable
       */
      Tree _intvar( Symbol sym, Tree init ) {
	  return gen.ValDef( sym, init );
      }

      // the caller needs to set the type !
      Tree  _applyNone( Tree arg ) {
	  return cf.make.Apply(pos, arg, Tree.EMPTY_ARRAY/*None*/ );
      }

    Tree _scala() {
	return gen.Ident(pos, defs.SCALA );
    }


      /** `<switchResult>'
       */
      public Tree _swres() { return gen.Ident( pos, resultSym );}

      /** `<state>'
       */
      public Tree _state() { return gen.Ident( pos, stateSym ); }

      /** code to reference the iterator
       */
      Tree _iter() {         return gen.Ident( pos, iterSym );  }

      /** body of the matcherDefFun
       */

    public Tree code_body_NEW() {
	int[] tags = new int[dfa.nstates];
	Tree[] bodies = new Tree[dfa.nstates];
	for( int i = 0; i<dfa.nstates; i++ ) {
	    tags[ i ]   = i;
	    if( dfa.isSink( i ))
		bodies[ i ] = run_finished( i ); // state won't change!
	    else
		bodies[ i ] = cf.If( cf.Negate( _ref( hasnSym )),//cf._not_hasNext( _iter() ),
				     run_finished( i ),
				     code_state_NEW( i ));
	}
	if( optimize )
	    return loadCurrentElem( cf.Switch( _state(),
					       tags,
					       bodies,
					       cf.Int( -1 )).setType( funRetType() ) );

	Tree res = code_fail();
	for( int i = dfa.nstates-2; i>= 0; i-- )
	    res = cf.If( cf.Equals( _state(), gen.mkIntLit( cf.pos, i )),
			 bodies[ i ] ,
			 res );

	return loadCurrentElem( res );

    }

      Tree _cur_eq( Tree iter, Label label ) {
            switch( label ) {
            case TreeLabel( Tree pat ):
                  return _cur_match( pat );
            case SimpleLabel( Tree.Literal lit ):
                  return cf.Equals( currentElem(), lit );
            }
            throw new ApplicationError("expected either algebraic or simple label:"+label);
      }

      AlgebraicMatcher am;

    /*
      void handleVars(  ) {
      }
    */
      // calling the /*AlgebraicMatcher*/PatternMatcher here
      Tree _cur_match( Tree pat ) {
            Matcher m = new Matcher( funSym,//this.funSym,
                                     currentElem(),
                                     defs.BOOLEAN_TYPE );

            am.construct( m, new CaseDef[] {
                  (CaseDef) cf.make.CaseDef( pat.pos,
                                             pat,
                                             Tree.Empty,
                                             gen.mkBooleanLit( Position.FIRSTPOS, true )),
                        (CaseDef) cf.make.CaseDef( pat.pos,
                                                   cf.make.Ident(pat.pos, Names.WILDCARD)
                                                   //.setSymbol( Symbol.NONE )
                                                   .setType(pat.type()),
                                                   Tree.Empty,
                                                   gen.mkBooleanLit( pat.pos, false )) },
			  false
                          );
            Tree res = am.toTree().setType( defs.BOOLEAN_TYPE );
            return res;
      }

      Tree code_delta( int i, Label label ) {
            throw new RuntimeException();
      }

      Tree code_fail() {
            return gen.mkIntLit(Position.FIRSTPOS, FAIL );
      }

      /** code for the return value of the automaton translation
       */
      Tree run_finished( int state ) {
            if( dfa.isFinal( state )) {
                  return gen.mkIntLit(Position.FIRSTPOS, ((Integer) dfa.finals.get( new Integer( state ) )).intValue() );
            }
            return gen.mkIntLit(Position.FIRSTPOS, FAIL );
      }

    /*
    Tree ifInputHasNext() {
	return cf.If( cf._hasNext( _iter() ),
		      cf.Block( stateBody.pos,
				new Tree[] {
				    loadCurrentElem(),
				    stateBody},
				stateBody.type()) );
    }
    */

    Tree code_state_NEW( int i ) {
	Tree stateBody = code_delta(i, Label.DefaultLabel );
	HashMap trans = ((HashMap[])dfa.deltaq)[ i ];

	for( Iterator labs = dfa.labels.iterator(); labs.hasNext() ; ) {
	    Object label = labs.next();
	    Integer next = (Integer) trans.get( label );


	    Tree action = code_delta( i, (Label) label );

	    if( action != null ) {

		stateBody = cf.If( currentMatches((Label) label ),
				   action,
				   stateBody);
	    }
	}
	return stateBody;
    }

    /** code to reference a variable
     */
    Tree _ref( Symbol sym ) {
	return gen.Ident( pos, sym );
    }


}
