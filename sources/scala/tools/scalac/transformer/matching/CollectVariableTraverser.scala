
import scalac.util.Name ;
import scalac.ast.Tree ;
import scalac.symtab.Symbol ;


import java.util.HashSet;

package scala.tools.scalac.transformer.matching {

class CollectVariableTraverser extends VariableTraverser {

    protected var nogeneratedVars = new HashSet();
    protected var vars  = new HashSet();
    /*
      boolean isVariableName( Name name ) {
            return ( name.toString().indexOf("$") == -1 )
                  && super.isVariableName( name );
                  }
                  */
  def handleVariableSymbol(sym: Symbol): Unit  = {
    //Console.println("VT:"+sym); // DEBUG
    vars.add( sym );
    if( sym.name.toString().indexOf("$") == -1 ) {
      nogeneratedVars.add( sym );
    }
  }

  def containsBinding(pat: Tree): Boolean = {

    //CollectVariableTraverser cvt = new CollectVariableTraverser();
    //cvt.
    nogeneratedVars.clear();
    traverse( pat );
    //return !/*cvt.*/nogeneratedVars.isEmpty();
    !nogeneratedVars.isEmpty();

  }

  def collectVars( pat:Tree  ): HashSet = {
    vars.clear();
    traverse( pat );
    return vars;
  }

}
}
