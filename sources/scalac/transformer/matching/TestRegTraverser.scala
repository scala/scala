/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.transformer.matching;

import scalac.ast._;
import scalac.util._;
import scalac.symtab._;

import java.util.{Set,HashSet};

object TestRegTraverser extends Traverser {

  var result:boolean = false;
  var variables:Set = null:Set;

  override def traverse( tree:Tree ):Unit = {

    tree.match {
      case Tree$Alternative(_) =>
	result = true;
      case Tree$Bind(_, pat) =>
	variables.add(tree.symbol());
	traverse(pat);
      case Tree$Ident( name ) =>
        result = ((name != Names.WILDCARD)
		  && variables.contains(tree.symbol()))
      case Tree$CaseDef(pat, _, _) =>
	traverse(pat);
      case _ =>
	super.traverse( tree );
    }
  }

  def apply(t:Tree):boolean = {
    variables = new HashSet();
    traverse(t);
    result
  }
}
