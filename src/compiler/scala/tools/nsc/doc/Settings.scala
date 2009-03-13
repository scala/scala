/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.doc

import java.io.File
import java.lang.System

class Settings(error: String => Unit) extends scala.tools.nsc.Settings(error) {
  /** scaladoc specific options */
  val memberaccess   = ChoiceSetting    ("-access", "Show only public, protected/public (default) or all classes and members",
                                         List("public", "protected", "private"), "protected")
  val pagebottom     = StringSetting    ("-bottom", "pagebottom", "Include bottom text for each page", "")
  val doccharset     = StringSetting    ("-charset", "doccharset", "Charset for cross-platform viewing of generated documentation.", "")
  val doctitle       = StringSetting    ("-doctitle", "doctitle", "Include title for the overview page", "Scala 2<br/>API Specification")
  val pagefooter     = StringSetting    ("-footer", "pagefooter", "Include footer text for each page", "")
  val pageheader     = StringSetting    ("-header", "pageheader", "Include header text for each page", "")
  val linksource     = BooleanSetting   ("-linksource", "Generate source in HTML")
  val nocomment      = BooleanSetting   ("-nocomment", "Suppress description and tags, generate only declarations.")
  val stylesheetfile = StringSetting    ("-stylesheetfile", "stylesheetfile", "File to change style of the generated documentation", "style.css")
  val pagetop        = StringSetting    ("-top", "pagetop", "Include top text for each page", "")
  val windowtitle    = StringSetting    ("-windowtitle", "windowtitle", "Specify window title of generated HTML documentation", "Scala 2")
}
