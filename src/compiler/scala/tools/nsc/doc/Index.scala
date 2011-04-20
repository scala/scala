package scala.tools.nsc.doc

import scala.collection._


trait Index {

  type SymbolMap = SortedMap[String, SortedSet[model.TemplateEntity]]

  def firstLetterIndex: Map[Char, SymbolMap]

}