package scalac.transformer.matching ;


import scalac.ast.Tree;
import scalac.util.Name;
import scalac.symtab.Symbol ;
import scalac.ast.Traverser ;

import Tree.Ident;
import Tree.Bind;


abstract class VariableTraverser extends Traverser {

      boolean isVariableName( Name name ) {
            return ( name.isVariable() ) && ( name != Name.fromString("_") ) ;

      }

      boolean isVariableSymbol( Symbol sym ) {
            return ( sym != null )&&( !sym.isPrimaryConstructor() );
      }

      abstract void handleVariableSymbol( Symbol sym ) ;

      public VariableTraverser() {
            super();
      }


      public void traverse(Tree tree) {
            switch (tree) {
            case Ident(Name name):
                  Symbol sym;

                  if( isVariableName( name )
                      && isVariableSymbol( sym = tree.symbol() ) )
                        handleVariableSymbol( sym );

                  return;

            case Bind(Name name, Tree subtree):
                  Symbol sym;

                  if( isVariableName( name )
                      && isVariableSymbol( sym = tree.symbol() ))
                        handleVariableSymbol( sym );

                  traverse( subtree );

                  return;

            case Select(_,_):
                  return;
            default:
                  super.traverse( tree );
            }
      }


}
