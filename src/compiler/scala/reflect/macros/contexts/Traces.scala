package scala.reflect.macros
package contexts

trait Traces extends util.Traces {
  self: Context =>

  def globalSettings = universe.settings
}
