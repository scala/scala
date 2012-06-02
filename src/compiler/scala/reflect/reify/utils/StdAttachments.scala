package scala.reflect.reify
package utils

trait StdAttachments {
  self: Utils =>

  import global._

  case class ReifyBindingAttachment(binding: Symbol)

  case class ReifyAliasAttachment(binding: Symbol, alias: TermName)
}