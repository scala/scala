package scalac.transformer.matching ;

import scalac.*;
import scalac.ast.Tree;
import scalac.ast.TreeGen;
import scalac.symtab.Type;
import scalac.symtab.Symbol;
import scalac.typechecker.*;
import Tree.*;

import scalac.transformer.TransMatch.Matcher ;
import java.util.* ;
import ch.epfl.lamp.util.Position;

public class WordAutomInScala extends Autom2Scala  {

      Tree getMatcherSwitch( Tree selector,
                             Tree failTree,
                             Tree body[],
                             Type resultType) {


            Tree run = callFun( new Tree[] {
                  cf.newIterator( selector ),
                  gen.mkIntLit(Position.NOPOS,0) } );

            /* return code `var <swres>: scala.Int = <init>' */

            run = _intvar( resultSym, run );

            Tree result;

            // conditions
            Tree cond[]  = new Tree[ body.length ];
            //Tree bbody[] = new Tree[ body.length ];
            for( int i = body.length - 1; i >= 0; i-- )
                  cond[ i ] = cf.Equals( _swres(), gen.mkIntLit(Position.NOPOS, i ));

            result = cf.Switch( selector, cond, body, failTree );

            result = cf.make.Block( pos, new Tree[] { theDefDef, run, result } )
                  .setType( resultType );
            //unit.global.debugPrinter.print( result );
            return result;


      }

      /** do the translation
       */
      public void translate() {
            initializeSyms();
            Tree tb = code_body();
            theDefDef = gen.DefDef( this.funSym,
                                    tb );
      }

      /** returns translation of transition with label from i.
       *  returns null if there is no such transition(no translation needed)
       */
      Tree code_delta( int i, Label label ) {
            Integer target = dfa.delta( i, label );

            if( target == null )
                  switch( label ) {
                  case DefaultLabel:
                        return code_fail(); // this may not happen !
                  default:
                        return null; // not good
                  }
            else if( target.intValue() == dfa.nstates - 1 ) // that one is a dead state
                  return code_fail();

            return callFun( new Tree[] {cf._next( _iter()), gen.mkIntLit(Position.NOPOS, target.intValue() )} );
      }

      public WordAutomInScala( DetWordAutom dfa,
                               Type elementType,
                               Symbol owner,
                               CodeFactory cf ) {
            super( dfa, elementType, owner, cf );
      }

}
