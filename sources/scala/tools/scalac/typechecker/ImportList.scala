/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**
** $Id$
\*                                                                      */

import scalac._;
import scalac.util._;
import scalac.symtab._;
import scalac.ast._;

package scala.tools.scalac.typechecker {

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
    return TreeInfo.importedSymbol(tree, name);
  }
}
}
