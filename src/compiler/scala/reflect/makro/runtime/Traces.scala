package scala.reflect.makro
package runtime

trait Traces extends util.Traces {
  self: Context =>

  def globalSettings = universe.settings
}
