/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.transformer.matching;

import ch.epfl.lamp.util.Position;

import scalac.*;
import scalac.ast.Tree;
import scalac.ast.TreeGen;
import scalac.symtab.Type;
import scalac.symtab.Symbol;
import scalac.symtab.Modifiers; // test
import scalac.transformer.TransMatch.Matcher;
//import scalac.typechecker.*;
import Tree.*;

import java.util.*;


/** translates a recognizer to scala code
 */
public class WordAutomInScala extends Autom2Scala {

    Tree theDefDef ;

    Tree getMatcherSwitch(Tree selector,
                          Tree failTree,
                          Tree body[],
                          Type resultType) {

        Tree result;

        boolean insane = true; // if you set this to false, you get some VerifyErrors

        if( insane ) { // cascading ifs

            Tree cond[] = new Tree[body.length];
            for( int i = body.length - 1; i >= 0; i-- ) {
                cond[i] = cf.Equals(_swres(), gen.mkIntLit( cf.pos, i ));
            }
            result = cf.Switch( cond, body, failTree );

        } else {        // real switch

            int tags[] = new int[body.length];
            for( int i = body.length - 1; i >= 0; i-- ) {
                tags[i] = i;
            }
            result = gen.Switch( _swres(), tags, body, failTree );

        }

        result = cf.gen.mkBlock( cf.pos, new Tree[] {
            gen.ValDef( iterSym, cf.newIterator( selector )),
            gen.ValDef( stateSym, gen.mkIntLit( cf.pos, 0) ),
            gen.ValDef( resultSym, theDefDef )},
            result );
        //unit.global.debugPrinter.print( result );
        return result;
    }

    protected void initializeSyms() { // TEST

        this.funSym = owner.newLabel( pos,
                                      cf.fresh.newName( "matcher" ));

        this.iterSym = owner.newVariable( pos,
                                          Modifiers.MUTABLE,
                                          cf.fresh.newName("iter"))
            .setType( cf._seqIterType( elementType ) ) ;

        this.stateSym = owner.newVariable( pos,
                                           Modifiers.MUTABLE,
                                        cf.fresh.newName("q"))
            .setType( defs.INT_TYPE() ) ;

        this.resultSym = owner.newVariable( pos,
                                            Modifiers.MUTABLE,
                                            cf.fresh.newName("swRes"))
            .setType( defs.INT_TYPE() ) ;

        this.funSym
            .setType( new Type.MethodType( new Symbol[] {
                funSym.newVParam( pos, 0, cf.fresh.newName("q"), defs.INT_TYPE())
            }, defs.INT_TYPE() ));

        this.curSym = owner.newVariable( pos, 0, CURRENT_ELEM )
            .setType( elementType );

        this.hasnSym = owner.newVariable( pos, 0, HASNEXT )
            .setType( defs.BOOLEAN_TYPE() );

    }

    /** code for the return value of the automaton translation
     */
    Tree run_finished( int state ) { // T E S T
        if( dfa.isFinal( state )) {
            return gen.mkIntLit(Position.FIRSTPOS, ((Integer) dfa.finals.get( new Integer( state ) )).intValue() );
        }
        return gen.mkIntLit( Position.FIRSTPOS, FAIL );
    }


    // calling the /*AlgebraicMatcher*/PatternMatcher here
    Tree _cur_match( Tree pat ) { // TE ST
        Matcher m = new Matcher( this.owner,   /* owner*/
                                 currentElem(), /* root */
                                 defs.BOOLEAN_TYPE() /* restype */);

        am.construct( m, new CaseDef[] {
            cf.gen.CaseDef( pat,
                            gen.mkBooleanLit( pat.pos, true )),
            cf.gen.CaseDef( cf.gen.Ident(pat.pos, defs.PATTERN_WILDCARD),
                            gen.mkBooleanLit( pat.pos, false )) },
                      false);
        return am.toTree();
    }


    /** do the translation
     */
    public void translate() {
        initializeSyms();
        Tree tb = code_body_NEW();
        //theDefDef = gen.DefDef(this.funSym, tb);
        theDefDef = gen.LabelDef(this.funSym, new Ident[] { /*(Ident)_iter(),*/ (Ident)_state() }, tb);
    }

    /** ...
     * @return returns translation of transition with label from i.
     * returns null if there is no such transition
     * (no translation needed)
     */
    Tree code_delta(int i, Label label) {
        Integer target = dfa.delta(i, label);

        if (target == null)
            switch (label) {
            case DefaultLabel:
                return code_error(); // this may not happen !
            default:
                return null; // not good
            }
        else if (target.intValue() == dfa.nstates - 1) // that one is a dead state
            return code_fail();

        return callFun(new Tree[] { gen.mkIntLit( cf.pos, target.intValue() )} );
    }

    /** constructor
     * @param dfa         the dfa that is to be translated
     * @param elementType type of the objects in the sequence
     * @param owner       the owner of the pattern match
     * @param cf          code factory
     * @param optim       flag that indicates whether to optimize
     * @return            an object that translates the dfa
     */
    public WordAutomInScala(DetWordAutom dfa,
                            Type elementType,
                            Symbol owner,
                            CodeFactory cf,
                            boolean optim) {
        super(dfa, elementType, owner, cf);
        this.optimize &= optim;

    }

}
