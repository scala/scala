package scalac.transformer.matching ;

import scalac.* ;
import scalac.symtab.Symbol ;
import scalac.symtab.Type ;
import scalac.symtab.Definitions ;
import scalac.symtab.Modifiers;
import scalac.ast.Tree;
import scalac.ast.TreeGen;
import scalac.util.Name;
import scalac.util.Names;
import Tree.*;

import java.util.* ;

import scala.tools.util.Position;

public class Autom2Scala  {

    protected boolean optimize = true;

    static final Name HASNEXT = Name.fromString("hasnext");
    static final Name CURRENT_ELEM = Name.fromString("cur");

    final int FAIL = -1;

    DetWordAutom dfa;
    protected CodeFactory cf;

    Definitions defs;// cf.defs only for convenience
    TreeGen     gen; // cf.gen  only for convenience

    /** owner of the pattern matching expression */
    protected Symbol owner;

    /** symbol of the matcher DefDef or Label */
    Symbol funSym;

    /** symbol of the iterator ( scala.SequenceIterator ) */
    Symbol iterSym;

    /** symbol of the switching result ( scala.Int ) */
    Symbol resultSym;

    /** symbol of the state variable ( scala.Int ) */
    Symbol stateSym;

    /** symbol of variable holding current label */
    Symbol curSym;

    /** symbol of boolean variable that indicates we have not reached end of sequence */
    Symbol hasnSym;

    protected Type elementType;

    public int pos;

    Type funRetType() {
        switch( funSym.type() ) {
        case MethodType( _, Type retType ):
            return retType;
        }
        throw new RuntimeException();
    }


    Tree callFun( Tree[] args ) {
        return gen.mkApply_V(gen.Ident(pos, funSym), args);
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
        this.am = new AlgebraicMatcher( cf.unit );
    }

    // overridden in RightTracerInScala
    Tree loadCurrentElem( Tree body ) {
        return gen.mkBlock( new Tree[] {
            cf.gen.ValDef( this.hasnSym,
                           cf._hasNext( _iter() ) ),
            cf.gen.ValDef( this.curSym,
                           gen.If( gen.Ident( pos, hasnSym ),
                                   cf._next( _iter() ),
                                   gen.mkDefaultValue(cf.pos,curSym.type())))},

            body );
    }

    /** bug ?? */
    Tree currentElem() { return gen.Ident( cf.pos, curSym ).setType( curSym.type() ); }

    Tree currentMatches( Label label ) {
        switch( label ) {
        case TreeLabel( Tree pat ):
            return _cur_match( pat );
        case SimpleLabel( Tree.Literal lit ):
            return cf.Equals( currentElem(), lit );
        }
        throw new ApplicationError("expected either algebraic or simple label:"+label);
    }

    //
    // translation of automata to scala code
    //


    /** `<switchResult>' */
    public Tree _swres() { return gen.Ident( pos, resultSym );}

    /** `<state>' param */
    public Tree _state() { return gen.Ident( pos, stateSym ); }

    /** `<iterator>' param */
    Tree _iter() {         return gen.Ident( pos, iterSym );  }

    /** simple optimization: if we are in a sink state, stop traversing sequence
     */
    Tree stateWrap(int i) {
        if( dfa.isSink( i ))
            return run_finished( i ); // state won't change! optimization
        else
            return gen.If( cf.Negate( gen.Ident( pos, hasnSym )),
                           run_finished( i ),
                           code_state_NEW( i ));
    }

    /** body of the matcherDefFun
     */
    public Tree code_body_NEW() {
        int[] tags = new int[dfa.nstates];
        Tree[] bodies = new Tree[dfa.nstates];
        for( int i = 0; i<dfa.nstates; i++ ) {
            tags[ i ]   = i;
            bodies[ i ] = stateWrap( i );
        }
        //if( optimize )
            return loadCurrentElem( gen.Switch( _state(),
                                                tags,
                                                bodies,
                                                code_error(), // cannot happen
                                                funRetType()));
            /*
        Tree res = code_error();
        for( int i = dfa.nstates-2; i>= 0; i-- )
            res = gen.If( cf.Equals( _state(), gen.mkIntLit( cf.pos, i )),
                          bodies[ i ] ,
                          res );

        return loadCurrentElem( res );
            */
    }

    AlgebraicMatcher am;

    /*
      void handleVars(  ) {
      }
    */
    // calling the /*AlgebraicMatcher*/PatternMatcher here
    Tree _cur_match( Tree pat ) {
        PartialMatcher m = new PartialMatcher( this.funSym,   /* owner*/
                                               currentElem(), /* root */
                                               defs.boolean_TYPE() /* restype */);

        am.construct( m, new CaseDef[] {
            cf.gen.CaseDef( pat,
                            gen.mkBooleanLit( pat.pos, true )),
            cf.gen.CaseDef( cf.gen.Ident(pat.pos, defs.PATTERN_WILDCARD),
                            gen.mkBooleanLit( pat.pos, false )) },
                      false);
        return am.toTree();
    }

    Tree code_delta( int i, Label label ) {
        throw new RuntimeException();
    }

    /** some error happened which is due to bug in translation/automaton
     */
    final Tree code_error() {
        return cf.ThrowMatchError( pos, funRetType() );
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
        return gen.mkIntLit( Position.FIRSTPOS, FAIL );
    }

    Tree code_state_NEW( int i ) {
        Tree stateBody = code_delta( i, Label.DefaultLabel );
        if( stateBody == null )
            stateBody = code_fail();
        HashMap trans = ((HashMap[])dfa.deltaq)[ i ];

        for( Iterator labs = dfa.labels.iterator(); labs.hasNext() ; ) {
            Object label = labs.next();
            Integer next = (Integer) trans.get( label );


            Tree action = code_delta( i, (Label) label );

            if( action != null ) {
                assert stateBody != null : "stateBody is null";
                stateBody = gen.If( currentMatches((Label) label ),
                                    action,
                                    stateBody);
            }
        }
        return stateBody;
    }
}
