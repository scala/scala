package scalac.transformer.matching ;

import scalac.util.Name ;
import scalac.ast.Tree ;
import scalac.symtab.Symbol ;

import java.util.Vector;

class CollectVariableTraverser extends VariableTraverser {

      protected Vector vars;

      boolean isVariableName( Name name ) {
            return ( name.toString().indexOf("$") == -1 )
                  && super.isVariableName( name );
      }


      void handleVariableSymbol( Symbol sym ) {
            vars.add( sym );
      }

      public CollectVariableTraverser() {
            this.vars = new Vector();
      }

      static boolean containsBinding( Tree  pat ) {

            CollectVariableTraverser cvt = new CollectVariableTraverser();
            cvt.traverse( pat );
            return !cvt.vars.isEmpty();

      }

    static Vector collectVars( Tree pat ) {
	CollectVariableTraverser cvt = new CollectVariableTraverser();
	cvt.traverse( pat );
	return cvt.vars;
    }

}
