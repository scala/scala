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
package doclet

/** A `Generator` may implement the `Universer` trait to gain access to a model of the documented program */
trait Universer extends Generator {

  protected var universeField: Universe = null

  def universe: Universe = universeField

  def setUniverse(u: Universe): Unit = {
    assert(universeField == null)
    universeField = u
  }

  checks += { () =>
    universeField != null
  }

}
