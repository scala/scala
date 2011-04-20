package scala.tools.nsc.doc

import scala.collection._


trait Index {

  type SymbolMap = SortedMap[String, SortedSet[model.MemberEntity]]

  def firstLetterIndex: Map[Char, SymbolMap]

}
