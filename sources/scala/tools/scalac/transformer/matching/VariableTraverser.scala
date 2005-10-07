import scalac.ast.Tree;
import scalac.util.Name;
import scalac.symtab.Symbol ;
import scalac.ast.Traverser ;

import Tree.Ident;
import Tree.Bind;
import Tree.Select;

package scala.tools.scalac.transformer.matching {

abstract class VariableTraverser extends Traverser {

  def isVariableName(name: Name): Boolean = {
    ( name.isVariable() ) && ( name != Name.fromString("_") ) ;
  }

  def isVariableSymbol(sym: Symbol): Boolean = {
    ( sym != null )&&( !sym.isPrimaryConstructor() );
  }

  def handleVariableSymbol(sym: Symbol): Unit;

  override def traverse(tree: Tree): Unit = {
    tree match {
      case Ident(name)=>
        var sym: Symbol  = _;

        if( isVariableName( name )
           && isVariableSymbol( {sym = tree.symbol(); tree.symbol()} ) )
          handleVariableSymbol( sym );

        return;

      case Bind(name, subtree) =>
        var sym: Symbol = _;

        if( isVariableName( name )
           && isVariableSymbol( {sym = tree.symbol(); tree.symbol()} ))
          handleVariableSymbol( sym );

        traverse( subtree );

        return;

      case Select(_,_) =>
        return;

      case _ =>
        super.traverse( tree );
    }
  }


}
}
