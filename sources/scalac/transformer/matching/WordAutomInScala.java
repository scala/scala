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

        Tree run = callFun( new Tree[] {
            cf.newIterator( selector ),
            gen.mkIntLit( cf.pos, 0 ) } );

        /* return code `var <swres>: scala.Int = <init>' */

        run = gen.ValDef( resultSym, run );

        Tree result;

        // conditions
        //int tags[] = new int[body.length];
        Tree cond[] = new Tree[body.length];
        //Tree bbody[] = new Tree[body.length];
        for( int i = body.length - 1; i >= 0; i-- ) {
            //tags[i] = i;
            cond[i] = cf.Equals(_swres(), gen.mkIntLit( cf.pos, i ));
        }
        result = cf.Switch( selector, cond, body, failTree );

        //result = gen.Switch( _swres(), tags, body, failTree );

        result = cf.gen.mkBlock( cf.pos, new Tree[] { theDefDef, run, result } );
        //unit.global.debugPrinter.print( result );
        return result;
    }

    /** do the translation
     */
    public void translate() {
        initializeSyms();
        Tree tb = code_body_NEW();
        theDefDef = gen.DefDef(this.funSym, tb);
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

        return callFun(new Tree[] { _iter(),
                                    gen.mkIntLit( cf.pos, target.intValue() )} );
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
