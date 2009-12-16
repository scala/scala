/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc
package doc

import java.io.File
import java.lang.System

class Settings(error: String => Unit) extends scala.tools.nsc.Settings(error) {

  /** scaladoc specific options */
  val docformat      = ChoiceSetting    ("-doc-format", "Selects to which format documentation is rendered", List("html"), "html")
  val doctitle       = StringSetting    ("-doc-title", "doc-title", "Include title for the overview page", "Scala 2 API")

  // working around issue described in r18708.
  suppressVTWarn.value = true


  // working around issue described in r18708.
  suppressVTWarn.value = true
}
