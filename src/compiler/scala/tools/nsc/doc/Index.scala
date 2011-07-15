/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc.doc

import scala.collection._


trait Index {

  type SymbolMap = SortedMap[String, SortedSet[model.MemberEntity]]

  def firstLetterIndex: Map[Char, SymbolMap]

}
