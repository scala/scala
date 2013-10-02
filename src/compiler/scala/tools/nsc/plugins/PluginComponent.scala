/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author Lex Spoon
 * Updated by Anders Bach Nielsen
 */

package scala.tools.nsc
package plugins

/** A component that is part of a Plugin.
 *
 * @author Lex Spoon
 * @version 1.1, 2009/1/2
 * Updated 2009/1/2 by Anders Bach Nielsen: Added features to implement SIP 00002
 */
abstract class PluginComponent extends SubComponent {

  /** By definition, plugin phases are externally provided. */
  final override val internal = false

  /** Only plugins are granted a reprieve from specifying whether they follow. */
  val runsRightAfter: Option[String] = None

  /** Useful for -Xshow-phases. */
  def description: String = ""

}
