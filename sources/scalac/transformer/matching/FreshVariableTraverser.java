package scalac.transformer.matching ;

import scalac.ast.Traverser ;

import scalac.ast.Tree;
import Tree.Ident;
import Tree.Bind;

import scalac.util.Name;
import scalac.util.FreshNameCreator ;

import scalac.symtab.* ;

import java.util.HashMap;
import java.util.Vector;

class FreshVariableTraverser extends VariableTraverser {

      int    pos;
      Symbol owner;
      FreshNameCreator fresh;

      public HashMap helpMap ;

      public FreshVariableTraverser( int pos,
                                     Symbol owner,
                                     FreshNameCreator fresh) {
            this.pos   = pos;
            this.owner = owner;
            this.fresh = fresh;

            helpMap  =  new HashMap();
      }

      void handleVariableSymbol( Symbol sym ) {
            Symbol helpVar = new TermSymbol( pos,
                                             fresh.newName( sym.name
                                                            .toString() ),
                                             owner,
                                             0)
                  .setType( sym.type() );

            helpMap.put( sym, helpVar );
      }

}
