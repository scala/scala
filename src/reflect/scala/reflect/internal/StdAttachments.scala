package scala.reflect
package internal

trait StdAttachments {
  self: SymbolTable =>

  case object BackquotedIdentifierAttachment

  case class CompoundTypeTreeOriginalAttachment(parents: List[Tree], stats: List[Tree])

  case class MacroExpansionAttachment(original: Tree)
}