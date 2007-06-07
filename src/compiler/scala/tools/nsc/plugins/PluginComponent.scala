package scala.tools.nsc.plugins

/** A component that is part of a Plugin.
 *
 * @version 1.0
 * @author Lex Spoon, 2007/5/29
 */
abstract class PluginComponent extends SubComponent {
  /** the phase this plugin wants to run after */
  val runsAfter: String
}
