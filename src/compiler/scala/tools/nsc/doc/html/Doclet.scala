/* NSC -- new Scala compiler
 * Copyright 2007-2012 LAMP/EPFL
 * @author  David Bernard, Manohar Jonnalagedda
 */

package scala.tools.nsc.doc
package html

import doclet._

/** The default doclet used by the scaladoc command line tool
  * when no user-provided doclet is provided. */
class Doclet extends Generator with Universer with Indexer {

  def generateImpl() {
    new html.HtmlFactory(universe, index).generate
  }

}
