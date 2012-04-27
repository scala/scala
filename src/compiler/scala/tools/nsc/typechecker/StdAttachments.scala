package scala.tools.nsc
package typechecker

import scala.reflect.makro.runtime.{Context => MacroContext}

trait StdAttachments {
  self: Analyzer =>

  case class MacroAttachment(delayed: Boolean, typerContext: Context, macroContext: Option[MacroContext])
}