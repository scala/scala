package scala.tools.nsc
package typechecker

trait StdAttachments {
  self: Analyzer =>

  type UnaffiliatedMacroContext = scala.reflect.macros.runtime.Context
  type MacroContext = UnaffiliatedMacroContext { val universe: self.global.type }
  case class MacroRuntimeAttachment(delayed: Boolean, typerContext: Context, macroContext: Option[MacroContext])
}