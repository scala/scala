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
import Tree.*;

import scalac.transformer.TransMatch.Matcher ;
import java.util.* ;

import ch.epfl.lamp.util.Position;

public class Autom2Scala  {

      static final Name WILDCARD_N   = Name.fromString("_");
      static final Name CURRENT_ELEM = Name.fromString("cur");

      final int FAIL = -1;

      DetWordAutom dfa;
      protected CodeFactory cf;

      Vector freeVars;
      Vector mdefs;
      //Tree   matcherDef;
      //Tree   matcherSwitch;

      Definitions defs;
      TreeGen     gen;

      /** owner of the pattern matching expression
       */
      protected Symbol owner;

      /** symbol of the matcher fun
       */
      Symbol funSym;

      /** symbol of the iterator ( scala.SequenceIterator )
       */
      Symbol iterSym;

      /** symbol of the switching result ( scala.Int )
       */
      Symbol resultSym;

      /** symbol of the state variable ( scala.Int )
       */
      Symbol stateSym;

      protected Type elementType;

      public int pos;

      String funSymName;

      Symbol newFunSym( String prefix ) {
	  return new TermSymbol( /*Kinds.FUN, */
                                   pos,
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

            this.resultSym = new TermSymbol( /*Kinds.VAR,*/
                                                   pos,
                                                   cf.fresh.newName("swRes"),
                                                   owner,
                                                   0 )
                  .setType( defs.INT_TYPE ) ;

            this.funSym
                  .setType( new Type.MethodType( new Symbol[] {
                        iterSym, stateSym },  defs.INT_TYPE ));

            this.curSym = new TermSymbol( /*Kinds.VAL, */
                                          pos,
                                          CURRENT_ELEM,
                                          funSym,//clazzOwner,
                                          0)
                  .setType( elementType );

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
            this.pos = 0;
            this.cf = cf;
            this.am = new /*AlgebraicMatcher*/PatternMatcher( cf.unit, cf.infer );
            this.mdefs = new Vector();

            this.freeVars = new Vector();
      }

      public Tree theDefDef;

      Symbol curSym;

      Tree loadCurrentElem() {
            return cf.Block( Position.NOPOS, new Tree[] {
		cf.gen.ValDef( 0,
			       this.curSym,
			       cf._next( _iter() )) }, Type.NoType );
      }

      Tree currentElem() {
            return gen.Ident(0, curSym);
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
            return gen.ValDef( pos, sym, init );
            /*
                                Kinds.VAR,
                                new ModifierSet(),
                                sym.name,
                                gen.mkType(pos, defs.INT_TYPE),
                                init)
                  .setType( defs.UNIT_TYPE )
                  .symbol( sym );*/
      }

      /** `<sym> = val'
      Tree code_assignInt( Symbol sym, Integer val ){
            return make.Assign(pos,
                               code_ref( sym ),
                               gen.mkIntLit(Position.NOPOS, val ))
                  .setType( defs.UNIT_TYPE );
      }
       */



      // the caller needs to set the type !
      Tree  _applyNone( Tree arg ) {
	  return cf.make.Apply(pos, arg, Tree.EMPTY_ARRAY/*None*/ );
      }







      Tree _scala() {
            // return gen.mkId( defs.SCALA ) // try this
            return gen.Ident(pos, defs.SCALA ); /*make.Ident( pos,
                               SCALA_N )
                               .setType( defs.SCALA.type() ).symbol( defs.SCALA );*/
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
      public Tree code_body() {

            Tree body = code_fail(); // never reached at runtime.

            // state [ nstates-1 ] is the dead state, so we skip it

            //`if( state == q ) <code_state> else {...}'
            for( int i = dfa.nstates-2; i >= 0; i-- ) {
                  body = code_state( i, body );
            }
            return body;

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

      /*AlgebraicMatcher*/PatternMatcher am;

      void handleVars(  ) {
      }

      // returns a Tree whose type is boolean.
      Tree handleBody( Object help ) {
            // don't care about free vars
            return gen.mkBooleanLit( Position.NOPOS, true );
      }

      // calling the /*AlgebraicMatcher*/PatternMatcher here
      Tree _cur_match( Tree pat ) {
            //System.out.println("calling algebraic matcher on type:"+pat.type);

            Matcher m = new Matcher( funSym,//this.funSym,
                                     currentElem(),
                                     defs.BOOLEAN_TYPE );

            am.construct( m, new CaseDef[] {
                  (CaseDef) cf.make.CaseDef( pat.pos,
                                             pat,
                                             Tree.Empty,
                                             handleBody( freeVars )),
                        (CaseDef) cf.make.CaseDef( pat.pos,
                                                   cf.make.Ident(pat.pos, WILDCARD_N)
                                                   .setSymbol( Symbol.NONE )
                                                   .setType(pat.type()),
                                                   Tree.Empty,
                                                   gen.mkBooleanLit( pat.pos, false )) }/*,
											  false*/
                          );
            Tree res = am.toTree().setType( defs.BOOLEAN_TYPE );
            return res;
      }

      Tree code_delta( int i, Label label ) {
            throw new RuntimeException();
      }

      Tree code_fail() {
            return gen.mkIntLit(Position.NOPOS, FAIL );
      }

      /** code for the return value of the automaton translation
       */
      Tree run_finished( int state ) {
            if( dfa.isFinal( state )) {
                  return gen.mkIntLit(Position.NOPOS, ((Integer) dfa.finals.get( new Integer( state ) )).intValue() );
            }
            return gen.mkIntLit(Position.NOPOS, FAIL );
      }

      Tree wrapStateBody0( Tree stateBody,
                           Tree elseBody,
                           int i ) {
            return cf.If( cf.Equals( _state(), gen.mkIntLit(Position.NOPOS, i )),
                          stateBody ,
                          elseBody );
      }

      // val cur := iter.cur()

      Tree wrapStateBody( Tree stateBody,
                          Tree elseBody,
                          Tree runFinished, int i ) {
            stateBody = cf.If( cf._not_hasNext( _iter() ),
                               runFinished,
                               cf.Block( stateBody.pos,
                                         new Tree[] {
                                               loadCurrentElem(),
                                               stateBody},
                                         stateBody.type()) );

            return wrapStateBody0( stateBody , elseBody, i );
      }

      /** return code for state i of the dfa
       */
      Tree code_state( int i, Tree elseBody ) {

            Tree runFinished; // holds result of the run
            int  finalSwRes;

            runFinished = run_finished( i );


            if( dfa.isSink( i ) )  // state won't change anymore (binding?)
                  return cf.If( cf.Equals( _state(), gen.mkIntLit(Position.NOPOS, i )),
                              runFinished,
                              elseBody );


            Tree stateBody ; // action(delta) for one particular label/test

            // default action (fail if there is none)

            stateBody = code_delta( i, Label.DefaultLabel);

            /*
            if( stateBody == null )
                  stateBody = code_fail();
            */

            // transitions of state i

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
            return wrapStateBody( stateBody,
                                  elseBody,
                                  runFinished,
                                  i );
      }

      /** code to reference a variable
       */
      Tree _ref( Symbol sym ) {
            return gen.Ident( pos, sym );
      }


}
