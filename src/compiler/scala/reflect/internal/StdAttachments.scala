package scala.reflect
package internal

trait StdAttachments {
  self: SymbolTable =>

  // this is the canonical place to put compiler-wide attachments

  // [Eugene++] the names for attachments are typically too long. what do we do with this?
  // cf. other `StdAttachments.scala` files

  case object BackquotedIdentifierAttachment

  case class CompoundTypeTreeOriginalAttachment(parents: List[Tree], stats: List[Tree])

  case class MacroExpansionAttachment(original: Tree)
}