package scalac.transformer.matching ;

import scalac.ast.Tree ;
import scalac.symtab.Symbol ;
import scalac.symtab.TermSymbol ;
import scalac.* ;
import scalac.util.Name ;
import java.util.* ;
import scalac.ast.printer.TextTreePrinter ;

/** given Sequence pattern pat, create flatpatterns flatPat and
 *  extract the nested (not necessarily flat) patterns pat_1,...,pat_n */
public class SplitNested {

      CodeFactory cf;
      Symbol owner;

      Tree origPat ;
      Tree flatPat ;

      HashMap nestedVarToPats ;

      Tree split( Tree pat ) {
            //System.out.println("SplitNested::split("+
            //                   TextTreePrinter.toString(pat)+")");
            switch( pat ) {
            case Apply(Tree fun, Tree[] trees):
                  return new Tree.Apply( fun, split( trees ))
                        .setType( pat.type() );

            case Sequence(_):  // remove nested sequences, make vars
                  Name n = cf.fresh.newName("nestseq");
                  Symbol v = new TermSymbol( 0,
                                             n,
                                             owner,
                                             0)
                        .setType( pat.type() );
                  nestedVarToPats.put( v, pat );
                  return new Tree.ExtBind( n,
                                           cf.make.Ident( 0, Name.fromString("_"))
                                           .setType( v.type() ))
                        .setSymbol( v )
                        .setType( v.type() );

            case Bind(Name name, Tree subtree):  // remove nested sequences, make vars
                  return new Tree.ExtBind(name, split( subtree ))
                        .setType( pat.type() )
                        .setSymbol( pat.symbol() );

            case Alternative(Tree[] trees):
                  return new Tree.Alternative( split( trees )) ;


            case Subsequence(Tree[] trees):
                  return new Tree.Subsequence( split( trees )) ;

            default:
                  return pat;
            }
      }

      Tree[] split( Tree pats[] ) {
            Tree npats[] = new Tree[ pats.length ];
            for( int i = 0; i < pats.length ; i++ ) {
                  npats[ i ] = split( pats[ i ] );
            }
            return npats;
      }

      Tree split1( Tree pat ) {
            switch( pat ) {
            case Sequence( Tree[] trees ):
                  return cf.make.Sequence( pat.pos, split( trees ) )
                        .setType( pat.type() );
            default:
                  throw new ApplicationError("SequencePattern expected");
            }
      }

      SplitNested(Tree pat, Symbol owner, CodeFactory cf) {
            origPat = pat;
            this.cf = cf;
            this.owner = owner;
            nestedVarToPats = new HashMap();
            this.flatPat = split1( pat );
      }
}
