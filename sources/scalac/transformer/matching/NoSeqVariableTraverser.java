package scalac.transformer.matching ;

import scalac.util.Name ;
import scalac.ast.Tree ;
import scalac.ast.Traverser ;
import scalac.symtab.Symbol ;

import java.util.Vector;

class NoSeqVariableTraverser extends CollectVariableTraverser {

      public void traverse(Tree tree) {
            switch (tree) {
            case Sequence(_):
                  return ;
            default:
                  super.traverse( tree );
            }
      }


      public NoSeqVariableTraverser() {
            super();
      }

      static Vector varsNoSeq( Tree  pat ) {

            NoSeqVariableTraverser nvt = new NoSeqVariableTraverser();
            nvt.traverse( pat );
            return nvt.vars;

      }

      static Vector varsNoSeq( Tree[]  pats ) {

            NoSeqVariableTraverser nvt = new NoSeqVariableTraverser();
            for(int i = 0; i < pats.length; i++)
                  nvt.traverse( pats[ i ] );
            return nvt.vars;

      }

}
