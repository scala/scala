/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author  David Bernard, Manohar Jonnalagedda
 */

package scala.tools.nsc
package doc
package html

import scala.reflect.internal.Reporter
import doclet._

/** The default doclet used by the scaladoc command line tool
  * when no user-provided doclet is provided. */
class Doclet(reporter: Reporter) extends Generator with Universer {

  @deprecated("Doclets should be created with the Reporter constructor. Otherwise logging reporters will not be shared by the creating parent", "2.12.0")
  def this() = this(null)

  def generateImpl() =
    new html.HtmlFactory(
      universe,
      if (reporter != null) reporter else new ScalaDocReporter(universe.settings)
    ).generate()
}
