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

package scala.tools.nsc.interactive.tests.core

case class DuplicateTestMarker(msg: String) extends Exception(msg)

object TestMarker {
  import scala.collection.mutable.Map
  private val markers: Map[String, TestMarker] = Map.empty

  private def checkForDuplicate(marker: TestMarker): Unit = {
    markers.get(marker.marker) match {
      case None => markers(marker.marker) = marker
      case Some(otherMarker) =>
        val msg = "Marker `%s` is already used by %s. Please choose a different marker for %s".format(marker.marker, marker, otherMarker)
        throw new DuplicateTestMarker(msg)
    }
  }
}

abstract case class TestMarker(marker: String) {
  TestMarker.checkForDuplicate(this)
}

object TypeCompletionMarker extends TestMarker("/*!*/")

object ScopeCompletionMarker extends TestMarker("/*_*/")

object TypeMarker extends TestMarker("/*?*/")

object HyperlinkMarker extends TestMarker("/*#*/")
