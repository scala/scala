package scalac.transformer.matching ;

import scalac.*;
import scalac.ast.*;
import scalac.symtab.*;
import Tree.*;
import scalac.util.Name;
import scalac.util.Names;

import scalac.transformer.TransMatch.Matcher ;

import java.util.* ;

import scala.tools.util.Position;

public class LeftTracerInScala extends TracerInScala {

    Tree selector;

    /** symbol of the accumulator ( scala.SequenceList )
     */
    Symbol accumSym;
    Type accumType;
    Type accumTypeArg;

    Type _accumType( Type elemType ) {
        return cf.SeqTraceType( elemType );
    }

    public LeftTracerInScala( DetWordAutom dfa,
                              Type elementType,
                              Symbol owner,
                              Tree selector,
                              CodeFactory cf ) {

        super( dfa, elementType, owner, cf );
        this.selector = selector;
    }

    protected void initializeSyms() {
        this.funSym =  owner.newLabel( pos,
                                       cf.fresh.newName( "left" ));

        this.iterSym = owner.newVariable( pos,
                                          Modifiers.MUTABLE,
                                          cf.fresh.newName( "iter" ))
            .setType( cf._seqIterType( elementType ) ) ;

        this.stateSym = owner.newVariable( pos,
                                           Modifiers.MUTABLE,
                                          cf.fresh.newName( "q" ))
            .setType( defs.INT_TYPE() ) ;

        this.accumType = _accumType( elementType );
        this.accumTypeArg = accumType.typeArgs()[0];
        this.accumSym = owner.newVariable( pos,                  // accumulator
                                           Modifiers.MUTABLE,
                                           cf.fresh.newName( "acc" ))
            .setType( accumType );

        //this.funSym
        //    .setType( new Type.MethodType( new Symbol[] {
        //        accumSym, iterSym, stateSym},
        //                                   accumType));

        this.funSym
            .setType( new Type.MethodType( new Symbol[] {  // dummy symbol MethodType
                funSym.newVParam( pos, 0, cf.fresh.newName( "q" ), defs.INT_TYPE()),
                funSym.newVParam( pos, 0, cf.fresh.newName( "acc" ), accumType ) },
                                           accumType)); // result type = List[T]

        this.resultSym = owner.newVariable(pos,
                                           0,
                                           cf.fresh.newName("trace"))
            .setType( accumType ) ;

        this.curSym = owner.newVariable( pos, 0, CURRENT_ELEM )
            .setType( elementType );

        this.hasnSym = owner.newVariable( pos, 0, HASNEXT )
            .setType( defs.BOOLEAN_TYPE() );

    }

    /* should throw an exception here really, e.g. MatchError
     */
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
        Tree newAcc = gen.mkNewCons( cf.pos,
                                     accumTypeArg,
                                     hd,
                                     gen.Ident( cf.pos, accumSym ));

        //return callFun( new Tree[] { newAcc , _iter(), gen.mkIntLit( cf.pos, target )} );
        return callFun( new Tree[] { gen.mkIntLit( cf.pos, target.intValue() ), newAcc } );
    }


    public Tree code_body() {

        Tree body = code_error(); // never reached at runtime.

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
                stateBody = gen.If( currentMatches((Label) label),
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

    Tree getTrace() {

        initializeSyms();

        return cf.gen.mkBlock( cf.pos, new Tree[] {
            gen.ValDef( iterSym, cf.newIterator( selector, selector.getType() )),
            gen.ValDef( stateSym, gen.mkIntLit( cf.pos, 0) ),
            gen.ValDef( accumSym, gen.mkNil( cf.pos )),
            gen.ValDef( resultSym,
                        gen.LabelDef( this.funSym,
                                      new Ident[] {
                                          gen.Ident( pos, stateSym ),
                                          gen.Ident( pos, accumSym )
                                      }, code_body() /* code_body_new ? */ ))},
            gen.Ident( cf.pos, resultSym ));
    }

    // calling the AlgebraicMatcher here
    Tree _cur_match( Tree pat ) {
        //return gen.mkBooleanLit(cf.pos, true);

        //System.out.println("calling algebraic matcher on type:"+pat.type);

        Matcher m = new Matcher( owner,
                                 currentElem(),
                                 defs.BOOLEAN_TYPE() );

        if( CollectVariableTraverser.containsBinding( pat )) {
            switch( pat ) {
            case Sequence(Tree[] pats):
                return gen.mkBooleanLit(cf.pos, true);
            }
        }

        am.construct( m, new CaseDef[] {
            cf.gen.CaseDef( pat,
                            gen.mkBooleanLit( cf.pos, true )),
            cf.gen.CaseDef( cf.gen.Ident( pat.pos, defs.PATTERN_WILDCARD ),
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
                                                 elementType));
        //System.err.println(hd.type);
        return gen.mkNewCons(  cf.pos,
                               accumTypeArg,
                               hd,
                               gen.Ident( cf.pos, accumSym ));
    }

}
