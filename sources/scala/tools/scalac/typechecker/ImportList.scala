/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**
** $Id$
\*                                                                      */

package scala.tools.scalac.typechecker;

import scalac._;
import scalac.util._;
import scalac.symtab._;
import scalac.ast._;

/////////////////////////////////////////////////////////////////////////////
// Import Lists
/////////////////////////////////////////////////////////////////////////////

/** A class for import expressions
*  @param tree        The import definition
*  @param enclScope   The scope in which the import occurs.
*  @param prev        The previous active import list.
*/
case class ImportList(tree: Tree, enclScope: Scope, prev: ImportList) {

  override def toString(): String = {
    var str = tree.symbol().toString();
    if (prev != null) str = "" + prev + "; " + str;
    str
  }

  def importPrefix(): Tree = tree match {
    case Tree$Import(expr, _) =>  expr
  }

  def importType(): Type =
    tree.symbol().getType();

  def sameImport(that: ImportList): boolean =
    this.importType().isSameAs(that.importType());

  def importedSymbol(name: Name): Symbol = {
    val t = this.importType();
    var renamed = false;
    tree match {
      case Tree$Import(expr, selectors) =>
	var i = 0;
	while (i < selectors.length) {
	  if (i + 1 < selectors.length && name.toTermName() == selectors(i+1)) {
	    if (name.isTypeName())
	      return t.lookupNonPrivate(selectors(i).toTypeName());
	    else
	      return t.lookupNonPrivate(selectors(i));
	  } else if (name.toTermName() == selectors(i)) {
	    renamed = true;
	  } else if (selectors(i) == Names.IMPORT_WILDCARD && !renamed) {
	    return t.lookupNonPrivate(name);
	  }
	  i = i + 2
	}
	Symbol.NONE
    }
  }
}
