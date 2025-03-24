/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
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
