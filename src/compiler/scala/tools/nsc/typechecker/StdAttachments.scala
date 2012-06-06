package scala.tools.nsc
package typechecker

trait StdAttachments {
  self: Analyzer =>

  type UnaffiliatedMacroContext = scala.reflect.makro.runtime.Context
  type MacroContext = UnaffiliatedMacroContext { val mirror: self.global.type }
  case class MacroAttachment(delayed: Boolean, typerContext: Context, macroContext: Option[MacroContext])
}