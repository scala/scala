

import scalac.util.Name ;
import scalac.ast.Tree ;
import scalac.ast.Traverser ;
import scalac.symtab.Symbol ;

import java.util.HashSet;

package scala.tools.scalac.transformer.matching {

class NoSeqVariableTraverser extends CollectVariableTraverser {

      override def traverse(tree:Tree ): Unit = {
        tree.match {
          case Tree.Sequence(_) =>
            return ;
          case _ =>
            super.traverse( tree );
        }
      }

  def varsNoSeq(pat: Tree):HashSet = {
    vars.clear();
    traverse( pat );
    return vars;

  }

  def varsNoSeq(pats: Array[Tree]): HashSet = {

    //NoSeqVariableTraverser nvt = new NoSeqVariableTraverser();
    var i = 0;
    while(i < pats.length) {
      traverse( pats( i ) );
      i = i + 1;
    }
    vars;

  }

}
}
