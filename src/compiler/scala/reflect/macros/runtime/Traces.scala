package scala.reflect.macros
package runtime

trait Traces extends util.Traces {
  self: Context =>

  def globalSettings = universe.settings
}
