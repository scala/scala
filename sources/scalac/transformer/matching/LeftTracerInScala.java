package scalac.transformer.matching ;

import scalac.*;
import scalac.ast.*;
import scalac.symtab.*;
import Tree.*;
import scalac.util.Name;

import scalac.transformer.TransMatch.Matcher ;

import java.util.* ;

import ch.epfl.lamp.util.Position;

public class LeftTracerInScala extends Autom2Scala {

      Tree selector;

      /** symbol of the accumulator ( scala.SequenceList )
       */
      Symbol accumSym;


      Type _accumType( Type elemType ) {
            return new Type.TypeRef( defs.SCALA_TYPE,
                                     cf.seqTraceSym(),
                                     new Type[] { elemType });
      }


      Matcher _m ;
      public LeftTracerInScala( DetWordAutom dfa,
                                Type elementType,
                                Matcher m,
                                CodeFactory cf ) {
            // ignore clazzOwner!
            super( dfa, elementType, m.owner, cf );
            this._m = m;
            this.selector = m.selector;
            helpMap = new HashMap();
            helpVarDefs = new Vector();

      }


      // SAME AS IN RightTracerInScala,
      // todo create common abstract superclass Tracer/TransducerInScala,
      // move WordAutom down.
      HashMap helpMap ;
      Vector  helpVarDefs;

      Symbol makeHelpVar( Symbol realVar ) {
	  Symbol helpVar = new TermSymbol( //Kinds.VAR,
                                             pos,
                                             cf.fresh.newName( realVar.name
                                                               .toString() ),
                                             owner,
                                             0)
                  .setType( cf.SeqListType( elementType ) ) ;

            helpMap.put( realVar, helpVar );

            Tree varDef = gen.ValDef(helpVar, cf.newSeqNil( elementType ));
            //((ValDef) varDef).kind = Kinds.VAR;
            helpVarDefs.add( varDef );
            return helpVar;
      }

      Symbol makeHelpVarSEQ( Tree pat ) {
            String  helpName = String.valueOf( pat.hashCode() ); //wicked, in'it ?
            Symbol helpVar =
		new TermSymbol( /*Kinds.VAR, */pos,
                                  cf.fresh.newName(Name.fromString( helpName )),
                                  owner,
                                  0)
                  .setType( pat.type() ) ;

            ValDef varDef = (ValDef) gen.ValDef( helpVar,
                                                 cf.ignoreValue( pat.type() ));
            //varDef.kind = Kinds.VAR;
            helpVarDefs.add( varDef );
            return helpVar;
      }

      // SAME AS RIGHT
      Tree refHelpVar( Symbol realVar ) {
            Symbol hv = (Symbol)helpMap.get( realVar );
            assert hv != null : realVar;
            return gen.Ident( Position.NOPOS, hv );
      }

      // SAME AS RIGHT
      Tree assignToHelpVar( Symbol realVar, Tree rhs ) {
            Tree hv = refHelpVar( realVar );
            return gen.Assign( hv, rhs );
      }

      void handleVars( Vector freeVars ) {
            for(Iterator it = freeVars.iterator(); it.hasNext(); ) {
                  makeHelpVar( (Symbol) it.next() );
            }
      }

      Tree bindVar(Symbol realVar) {
            Tree hv = refHelpVar( realVar );
            System.out.println("realVar.name "+realVar.name+" type:"+realVar.type());
            if( realVar.type().isSameAs( elementType ))
                  return gen.ValDef( realVar, cf.SeqList_head( hv ));
            else
                  return gen.ValDef( realVar, hv);

      }


      /** returns a Tree whose type is boolean.
       *  now we care about free vars
       */
      /*
      Tree handleBody( HashMap helpMap ) {
            Tree res[] = new Tree[ helpMap.keySet().size() + 1 ];
            int j = 0;
            for( Iterator it = helpMap.keySet().iterator(); it.hasNext(); ) {
                  Symbol vsym = (Symbol) it.next();
                  Symbol hv   = (Symbol) helpMap.get( vsym );
                  hv.type( cf.SeqListType( elementType ) ) ;
                  Tree refv   = gen.Ident(Position.NOPOS, vsym);
                  Tree refhv  = gen.Ident(Position.NOPOS, hv);
                  res[ j++ ] = gen.Assign( refhv, refv );
            }

            res[ j ] = super.handleBody( freeVars ); // just `true'

            return cf.Block(Position.NOPOS, res, res[j].type() );
      }
*/
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

      /** do the translation
      public void translate() {
            initializeSyms();
            Tree tb = code_body();
            theDefDef = gen.DefDef( this.funSym,
                                    tb );
      }
       */

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
            Tree newAcc = cf.newSeqTraceCons(new Integer(i),
                                             currentElem(),
                                             _ref( accumSym ));

            return callFun( new Tree[] { newAcc , _iter(), cf.Int( target )} );
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

                        stateBody = cf.If( _cur_eq( _iter(), (Label) label ),
                                         action,
                                         stateBody);
                  }
            }

            return wrapStateBody( stateBody,
                                  elseBody,
                                  runFinished,
                                  i );

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

            Tree emptyAcc    = cf._seqTraceNil( elementType );

            // the valdef is needed, because passing emptyAcc as a parameter
            //   results in a ClassCastException at runtime (?!)

            Symbol emptyAccSym = new TermSymbol( //Kinds.VAR,
                                                 pos,
                                                 cf.fresh.newName("acc"),
                                                 owner,
                                                 0 )
                  .setType( _accumType( elementType ) ) ;

            // `val acc = SeqNil[ elementType ]'                 init accumulator
            v.add( gen.ValDef( pos, emptyAccSym, emptyAcc) );

            Tree run = callFun( new Tree[] {
                  gen.Ident( pos, emptyAccSym ),
                  cf.newIterator( selector ),
                  cf.Int( 0 )  });

            run = gen.ValDef( Position.NOPOS, resultSym, run );

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
                        Tree stm  = gen.Assign( gen.Ident(0, hv), currentElem() );
                        m.stms = new Tree[2];
                        m.stms[0] = stm;
                        m.stms[1] = gen.mkBooleanLit(Position.NOPOS, true);
                        return cf.Block( 0, m.stms, m.stms[1].type() );
                  }
            }

            FreshVariableTraverser fv = new FreshVariableTraverser(pos,
                                                                   owner,
                                                                   cf.fresh);
            fv.traverse( pat );
            HashMap helpMap = fv.helpMap;
            //System.out.println("varMap: "+helpMap );

            m.varMap = fv.helpMap;

            //replaceVars( pat );

            am.construct( m, new CaseDef[] {
                  (CaseDef) cf.make.CaseDef( pat.pos,
                                             pat,
                                             Tree.Empty,
                                             handleBody( fv.helpMap )),
                        (CaseDef) cf.make.CaseDef( pat.pos,
                                                   cf.make.Ident(pat.pos, WILDCARD_N)
                                                   .setSymbol( Symbol.NONE )
                                                   .setType(pat.type()),
                                                   Tree.Empty,
                                                   gen.mkBooleanLit(Position.NOPOS, false)) }/*,
											       false */
                          );
            Tree res = am.toTree().setType( defs.BOOLEAN_TYPE );
            //System.out.println("freeVars: "+freeVars);

            return res;
      }


      /** return the accumulator + last state
       */
      Tree run_finished( int state ) {
            return cf.newSeqTraceCons(new Integer( state ),
                                      cf.ignoreValue( elementType ),
                                      _ref( accumSym ));
      }

}
