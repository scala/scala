package scalac.transformer.matching ;

import scalac.*;
import scalac.ast.*;
import scalac.symtab.*;
import Tree.*;

import scalac.transformer.TransMatch.Matcher ;

import java.util.* ;
import Scope.SymbolIterator;

import scalac.ast.printer.TextTreePrinter ;

import scalac.util.Name ;
import scalac.util.Names ;

import ch.epfl.lamp.util.Position;

public class RightTracerInScala extends Autom2Scala  {

      //Scope  scp;
      //Symbol vars[];

      Vector seqVars;
      Vector allVars;

      Matcher _m;

      /** translate right tracer to code
       * @param dfa determinized left tracer
       * @param left nondeterm. left tracer
       * @param cf   ...
       * @param pat  ?
       * @param elementType ...
       */
      public RightTracerInScala( DetWordAutom dfa,
                                 NondetWordAutom left,
                                 Matcher m,
                                 CodeFactory cf,
                                 Tree pat,
                                 Type elementType ) {
            super( dfa, elementType, m.owner, cf );
            this._m = m;


            Vector seqVars = new Vector();

            for( int j = 0; j < left.nstates; j++ ) {
                  if( left.qbinders[ j ] != null )
                        for( Iterator it = left.qbinders[ j ].iterator();
                             it.hasNext() ; ) {
                              Symbol varSym = (Symbol) it.next();

                              if( !seqVars.contains( varSym ) )
                                    seqVars.add( varSym );
                        }
            }

            this.seqVars = seqVars;
            this.allVars = CollectVariableTraverser.collectVars( pat );

            helpMap = new HashMap();
            helpMap2 = new HashMap();
            helpVarDefs = new Vector();

            for( Iterator it = seqVars.iterator(); it.hasNext(); ) {
                  makeHelpVar( (Symbol) it.next() );
            }

            for( Iterator it = allVars.iterator(); it.hasNext(); ) {
                  Symbol varSym = (Symbol) it.next();
                  if( !seqVars.contains( varSym )) {
                        makeHelpVar( varSym, true );
                  }
            }

            initializeSyms();
      }

      //      Symbol funSym;

      Symbol elemSym;
      Symbol targetSym;

      HashMap helpMap ;
      HashMap helpMap2 ;
      Vector  helpVarDefs;

      void makeHelpVar( Symbol realVar ) {
            makeHelpVar( realVar, false );
      }

      void makeHelpVar( Symbol realVar, boolean keepType ) {
	  Symbol helpVar = new TermSymbol( //Kinds.VAR,
                                             pos,
                                             cf.fresh.newName( realVar.name
                                                               .toString() ),
                                             owner,
                                             0);

            //System.out.println("making helpvar : "+realVar+" -> "+helpVar);

            if( keepType )
                  helpVar.setType( realVar.type() );
            else
                  helpVar.setType( cf.SeqListType( elementType ) );


            helpMap.put( realVar, helpVar );

            Tree rhs;
            if( keepType )
                  rhs = cf.ignoreValue( realVar.type() );
            else
		rhs = /* cf.newRef( */ cf.newSeqNil( elementType ) /* ) */;
	    helpVar.flags |= Modifiers.MUTABLE;
            Tree varDef = gen.ValDef(helpVar, rhs);
            //((ValDef) varDef).kind = Kinds.VAR;
            helpVarDefs.add( varDef );

      }

      Tree refHelpVar( Symbol realVar ) {
            Symbol hv = (Symbol)helpMap.get( realVar );
            assert hv != null : realVar;
            return gen.Ident(Position.NOPOS, hv);
      }

      Tree prependToHelpVar( Symbol realVar, Tree elem ) {
            Tree hv = refHelpVar( realVar );
            return gen.Assign( hv, cf.newSeqCons( elem, hv ));
            /*
            return cf.Block(pos,
                            new Tree [] {
                  cf.debugPrintRuntime( "ASSIGN" ),
                  gen.Assign( hv, cf.newSeqCons( elem, hv ))
                        }, defs.UNIT_TYPE);
            */
      }

      Tree bindVar(Symbol realVar) {
            Tree hv = refHelpVar( realVar );
            //System.out.println("binding realVar.name "+realVar.name+" type:"+realVar.type()+" to smth");
            realVar.setOwner( owner );
            if( realVar.type().isSameAs( elementType ))
                  return gen.ValDef( realVar, cf.SeqList_head( hv ));
            else
                  return gen.ValDef( realVar, hv);

      }

      protected void initializeSyms() {

            this.funSym = newFunSym( "binder" );

            this.iterSym = new TermSymbol( //Kinds.VAL,
                                           pos,
                                           cf.fresh.newName("iter"),
                                           funSym,
                                           0)
                  .setType( cf.SeqTraceType( elementType ));

            this.stateSym = new TermSymbol( //Kinds.VAL,
                                            pos,
                                            cf.fresh.newName("q"),
                                            funSym,
                                            0 )
                  .setType( defs.INT_TYPE ) ;

            this.elemSym = new TermSymbol( //Kinds.VAL,
                                           pos,
                                           cf.fresh.newName("elem"),
                                           funSym,
                                           0)
                  .setType( elementType ) ;

            this.targetSym = new TermSymbol( //Kinds.VAL,
                                             pos,
                                             cf.fresh.newName("trgt"),
                                             funSym,
                                             0)
                  .setType( defs.INT_TYPE ) ;


            funSym.setType( new Type.MethodType( new Symbol[] {
                  iterSym, stateSym },  defs.UNIT_TYPE ));

      }

      // same as in LeftTracer
      Tree code_fail() {

            return cf.ThrowMatchError( _m.pos, defs.UNIT_TYPE );

      }

      public Tree code_body() {

            Tree body = code_fail(); // never reached at runtime.

            // state [ nstates-1 ] is the dead state, so we skip it

            //`if( state == q ) <code_state> else {...}'
            for( int i = dfa.nstates-1; i >= 0; i-- ) {
                  body = code_state( i, body );
            }


            Tree t3 = cf.If( cf.isEmpty( _iter() ),
                               run_finished( 0 ),
                               gen.Block( new Tree[] {
                                     gen.ValDef( targetSym,
                                                 cf.SeqTrace_headState( gen.Ident( pos, iterSym))),
                                     gen.ValDef( elemSym,
                                                 cf.SeqTrace_headElem( gen.Ident( pos, iterSym))),

                                     body }));

            /*
            t3 = gen.Block( new Tree[] {
                  cf.debugPrintRuntime("enter binderFun"),
                  cf.debugPrintRuntime(" state:"),
                  cf.debugPrintRuntime( gen.Ident( pos, stateSym )),
                  cf.debugPrintRuntime(" iter:"),
                  cf.debugPrintRuntime(_iter()),
                  cf.debugPrintNewlineRuntime(""),
                  t3 });
            */

            //System.out.println("enter RightTracerInScala:code_body()");// DEBUG
            //System.out.println("dfa.nstates"+dfa.nstates);// DEBUG
            return t3;
      }

      /** this one is special, we check the first element of the trace
       *  and choose the next state depending only on the state part
       */
      Tree code_state0( Tree elseBody ) { // careful, map Int to Int

            HashMap hmap    = (HashMap) dfa.deltaq( 0 ); // all the initial states

            Tree stateBody = code_fail(); // never happens

            for( Iterator it = hmap.keySet().iterator(); it.hasNext(); ) {
                  Integer targetL  = (Integer) it.next();
                  Integer targetR  = (Integer) hmap.get( targetL );

                  stateBody = cf.If( cf.Equals( gen.Ident( pos, targetSym ),
                                                cf.Int( targetL )),
                                     callFun( new Tree[] {
                                           cf.SeqTrace_tail( _iter() ),
                                           cf.Int( targetR ) }),
                                     stateBody );
            }

            return cf.If( cf.Equals( _state(), cf.Int( 0 )),
                          stateBody ,
                          elseBody );

      }

      Tree code_state( int i, Tree elseBody ) {

            if( i == 0 )
                  return code_state0( elseBody );

            int  finalSwRes;
            Tree stateBody ; // action(delta) for one particular label/test

            // default action (fail if there is none)

            stateBody = code_delta( i, Label.DefaultLabel);

            if( stateBody == null )
                  stateBody = code_fail();

            // transitions of state i

            HashMap trans = ((HashMap[])dfa.deltaq)[ i ];

            for( Iterator labs = dfa.labels.iterator(); labs.hasNext() ; ) {
                  Object label = labs.next();
                  Integer next = (Integer) trans.get( label );

                  Tree action = code_delta( i, (Label) label );

                  if( action != null ) {

                        stateBody = cf.If( _cur_eq( _iter(), (Label) label ),
                                           action,
                                           stateBody);
                  }
            }

            return wrapStateBody0( stateBody,
                                   elseBody,
                                   i );
      }

      /** returns a Tree whose type is boolean.
       *  now we care about free vars
       */

      Tree handleBody( Object o ) {
            HashMap helpMap = (HashMap) o;
            Tree res[] = new Tree[ helpMap.keySet().size() + 1 ];
            int j = 0;
            for( Iterator it = helpMap.keySet().iterator(); it.hasNext(); ) {
                  Symbol vsym = (Symbol) it.next();
                  Symbol hv   = (Symbol) helpMap.get( vsym );
                  hv.setType( cf.SeqListType( elementType ) ) ;
                  Tree refv   = gen.Ident(Position.NOPOS, vsym);
                  Tree refhv  = gen.Ident(Position.NOPOS, hv);
                  res[ j++ ] = gen.Assign( refhv, refv );
            }

            res[ j ] = super.handleBody( freeVars ); // just `true'

            return cf.Block(Position.NOPOS, res, res[j].type() );
      }

      // calling the AlgebraicMatcher here
      Tree _cur_match( Tree pat ) {

            //System.out.println("RTiS._cur_match("+TextTreePrinter.toString(pat)+")");

            //System.out.println("calling algebraic matcher on type:"+pat.type);

            Matcher m = new Matcher( funSym,//this.funSym,
                                     currentElem(),
                                     defs.BOOLEAN_TYPE );

            Vector varsToExport = NoSeqVariableTraverser.varsNoSeq( pat );

            //System.out.println("RightTracerInScala::varsToExport :"+varsToExport);

            for( Iterator it = varsToExport.iterator(); it.hasNext(); ) {
                  Symbol key = (Symbol) it.next();
                  this.helpMap2.put( key, helpMap.get( key ));
            }

	    // find this weird ? pattern matcher expects var. symbol for _ pattern FIXME ?!! CANNOT BE TRUE
	    //Symbol obfuscvble = new TermSymbol(0, Name.fromString("ga$ga$ga$"), _m.owner, 0).setType( pat.type() );

            am.construct( m, new CaseDef[] {
                  (CaseDef) cf.make.CaseDef( pat.pos,
                                             pat,
                                             Tree.Empty,
                                             handleBody( helpMap2 )),
                        (CaseDef) cf.make.CaseDef( pat.pos,
                                                   cf.make.Ident(pat.pos, Names.WILDCARD)
                                                   //.setSymbol( Symbol.NONE ) FIXED
						   .setType( pat.type() ),
                                                   Tree.Empty,
                                                   gen.mkBooleanLit( pat.pos, false )) },
                          true // do binding please
                          );
            Tree res = am.toTree().setType( defs.BOOLEAN_TYPE );
            //System.out.println("freeVars: "+freeVars);
            return res;
      }


      /** returns translation of transition with label from i.
       *  returns null if there is no such transition(no translation needed)
       */
      Tree code_delta( int i, Label label ) {
            HashMap hmap    = (HashMap) dfa.deltaq( i );
            Integer ntarget = (Integer) hmap.get( label );
            Tree    algMatchTree = null;
            if( ntarget == null )
                  return null;
            //System.out.println("delta("+i+","+label+")" );
            Label theLab = null;
            switch(label) {
            case Label.Pair( Integer state, Label lab2 ):
                  //assert ntarget == state;
                  theLab = lab2;
                  switch( lab2 ) {
                  case TreeLabel( Tree pat ):
                        algMatchTree = _cur_match( pat );
                        break;
                  }
                  break;
            case DefaultLabel:
                  throw new ApplicationError(); // should not happen
            }
            assert dfa.qbinders != null : "qbinders ?";

            Vector vars = dfa.qbinders[ i ];

            if( vars == null ) vars = new Vector(); // TODO: make this more consistent
            assert vars != null;

            //System.out.println("delta: theLab: " + theLab + " vars in current ="+ vars );

            if( ntarget == null )
                  return code_fail();

            Tree stms[] = new Tree[ vars.size()
                                    + ((algMatchTree != null )? 1 : 0 )
                                    + 1 ];
            int j = 0;
            for( Iterator it = vars.iterator(); it.hasNext(); ) {
                  Symbol var = (Symbol) it.next();

                  /*Tree field = gen.Select( gen.Ident( pos, accumSym ),
                                           var.name ).setSymbol( var );
                  */
                  //Tree field = gen.Ident( pos, var );
                  Tree rhs = gen.Ident( pos, elemSym ); //cf._cur( _iter() );

                  stms[ j++ ] = prependToHelpVar( var , rhs);
            }

            if( algMatchTree != null )
                  stms[ j++ ] = algMatchTree ;

            stms[ j ] = callFun( new Tree[] { cf.SeqTrace_tail( _iter() ),
                                              cf.Int( ntarget ) } );

            return cf.Block( pos, stms, funRetType() );
      }

      /* returns statements that do the work of the right-transducer
       */
      Tree[] getStms( Tree trace ) {

            //System.out.println( "!!getStms.helpVarDefs: "+helpVarDefs);

            Vector v = new Vector();

            for( Iterator it = helpVarDefs.iterator(); it.hasNext(); )
                  v.add( (Tree) it.next() );

            v.add( gen.DefDef( this.funSym, code_body() )  );
            v.add( callFun( new Tree[] {  trace, cf.Int( 0 )  }  )  );


            /*
            for(Iterator it = helpMap.keySet().iterator(); it.hasNext(); ) {
                  // DEBUG
                  Symbol var = (Symbol)it.next();
                  v.add( cf.debugPrintRuntime( var.name.toString() ));
                  v.add( cf.debugPrintRuntime( refHelpVar( var )) );
            }
            */

            for( Iterator it = seqVars.iterator(); it.hasNext(); ) {
                  v.add( bindVar( (Symbol) it.next() ) );
            }

            Tree result[] = new Tree[ v.size() ];
            int j = 0;
            for( Iterator it = v.iterator(); it.hasNext(); ) {
                  result[ j++ ] = (Tree) it.next();
            }

            // helpvarSEQ via _m.varMap

            return result;

      }

      /** return the accumulator. (same as in LeftTracerInScala)
       */
      Tree run_finished( int state ) {
            //return gen.Ident( accumSym.pos, accumSym );
	  return gen.Block(0, Tree.EMPTY_ARRAY).setType( defs.UNIT_TYPE );
	      /* gen.Ident(0, defs.NULL); */
	  /*gen.New( pos, defs.SCALA_TYPE, defs.NULL, //UNIT_CLASS,
                            Type.EMPTY_ARRAY,
                            Tree.EMPTY_ARRAY);                             */


      }

      Tree current() {
            return gen.Ident( pos, targetSym );
      }

      Tree currentElem() {
            return gen.Ident( pos, elemSym );
      }

      Tree _cur_eq( Tree iter, Label label ) {
            //System.out.println("_cur_eq, thelab: "+label);
            switch( label ) {
            case Pair( Integer target, Label theLab ):
                  return cf.Equals( cf.Int( target ),
                                    current() );
            }
            throw new ApplicationError("expected Pair label");
      }


}
