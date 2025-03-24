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

package scala.tools.partest

import scala.tools.nsc.doc.Universe

/** A class for testing scaladoc model generation on java sources. */
abstract class ScaladocJavaModelTest extends ScaladocModelTest {

  // overridden to pass explicit files to newDocFactory.makeUniverse (rather than code strings)
  // since the .java file extension is required
  override def model: Option[Universe] = {
    val path = s"$resourcePath/$resourceFile"
    newDocFactory.makeUniverse(Left(List(path)))
  }

}
