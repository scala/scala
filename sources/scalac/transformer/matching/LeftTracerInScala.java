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
        funSymName = "leftTracer";

        super.initializeSyms();
        this.accumType = _accumType( elementType );
        this.accumTypeArg = accumType.typeArgs()[0];
        this.accumSym = newParam("accum")                    // accumulator
            .setType( accumType );

        this.funSym
            .setType( new Type.MethodType( new Symbol[] {
                accumSym, iterSym, stateSym},
                                           accumType));

        // 2 do: rename switchresultsym to something else...

        this.resultSym = new TermSymbol(pos,
                                        cf.fresh.newName("trace"),
                                        owner,
                                        0 )
            .setType( accumType ) ;
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
        Tree newAcc = gen.Cons(cf.pos,
                               accumTypeArg,
                               hd,
                               gen.Ident( cf.pos, accumSym ));

        return callFun( new Tree[] { newAcc , _iter(), gen.mkIntLit( cf.pos, target )} );
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

    Tree[] getTrace() {

        initializeSyms();
        //Tree tb = code_body();
        Tree tb = code_body_NEW();
        Tree theDefDef = gen.DefDef( this.funSym,
                                     tb );

        Vector v = new Vector();

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
            .setType( accumType ) ;

        // `val acc = SeqNil[ elementType ]'                 init accumulator
        v.add( gen.ValDef( emptyAccSym, emptyAcc) );

        Tree run = callFun( new Tree[] {
            gen.Ident( pos, emptyAccSym ),
            cf.newIterator( selector, selector.getType() ),
            gen.mkIntLit( cf.pos, 0 )  });

        run = gen.ValDef( resultSym, run );

        v.add( run );

        Tree res[] = new Tree[ v.size() ];
        int j = 0;
        for( Iterator it = v.iterator(); it.hasNext(); )
            res[ j++ ] = (Tree) it.next();

        return res;

    }

    // calling the AlgebraicMatcher here
    Tree _cur_match( Tree pat ) {
        //return gen.mkBooleanLit(cf.pos, true);

        //System.out.println("calling algebraic matcher on type:"+pat.type);

        Matcher m = new Matcher( funSym,
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
        return gen.Cons(  cf.pos,
                          accumTypeArg,
                          hd,
                          gen.Ident( cf.pos, accumSym ));
    }

}
