/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc
package doc

import java.io.File
import java.lang.System

/** An extended version of compiler settings, with additional Scaladoc-specific options.
  * @param error A function that prints a string to the appropriate error stream. */
class Settings(error: String => Unit) extends scala.tools.nsc.Settings(error) {

  /** A setting that defines in which format the documentation is output. ''Note:'' this setting is currently always
    * `html`. */
  val docformat      = ChoiceSetting    ("-doc-format", "Selects in which format documentation is rendered", List("html"), "html")

  /** A setting that defines the overall title of the documentation, typically the name of the library being
    * documented. 'Note:'' This setting is currently not used. */
  val doctitle       = StringSetting    ("-doc-title", "doc-title", "The overall name of the Scaladoc site", "")

  /** A setting that defines the overall version number of the documentation, typically the version of the library being
    * documented. 'Note:'' This setting is currently not used. */
  val docversion     = StringSetting    ("-doc-version", "doc-version", "An optional version number, to be appended to the title", "")

  /** A setting that defines a URL to be concatenated with source locations and show a link to source files.
   * If needed the sourcepath option can be used to exclude undesired initial part of the link to sources */
  val docsourceurl   = StringSetting    ("-doc-source-url", "url", "The URL prefix where documentation will link to sources", "")

  // working around issue described in r18708.
  suppressVTWarn.value = true
}
