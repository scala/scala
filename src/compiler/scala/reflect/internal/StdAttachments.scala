package scala.reflect
package internal

import scala.reflect.makro.runtime.{Context => MacroContext}

trait StdAttachments {
  self: SymbolTable =>

  case class ReifyAttachment(original: Symbol)

  case class MacroAttachment(delayed: Boolean, context: Option[MacroContext])
}