package scalac.transformer.matching ;

import scalac.util.Name ;
import scalac.ast.Tree ;
import scalac.symtab.Symbol ;


import java.util.HashSet;

class CollectVariableTraverser extends VariableTraverser {

    protected HashSet nogeneratedVars;
    protected HashSet vars;
    /*
      boolean isVariableName( Name name ) {
            return ( name.toString().indexOf("$") == -1 )
                  && super.isVariableName( name );
      }
    */
      void handleVariableSymbol( Symbol sym ) {
            vars.add( sym );
            if( sym.name.toString().indexOf("$") == -1 ) {
                nogeneratedVars.add( sym );
            }
      }

      public CollectVariableTraverser() {
            this.vars = new HashSet();
            this.nogeneratedVars = new HashSet();
      }

      static boolean containsBinding( Tree  pat ) {

            CollectVariableTraverser cvt = new CollectVariableTraverser();
            cvt.traverse( pat );
            return !cvt.nogeneratedVars.isEmpty();

      }

    static HashSet collectVars( Tree pat ) {
	CollectVariableTraverser cvt = new CollectVariableTraverser();
	cvt.traverse( pat );
	return cvt.vars;
    }

}
