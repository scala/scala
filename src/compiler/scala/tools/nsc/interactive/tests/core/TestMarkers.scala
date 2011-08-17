package scala.tools.nsc.interactive.tests.core

case class DuplicateTestMarker(msg: String) extends Exception(msg)

object TestMarker {
  import collection.mutable.Map
  private val markers: Map[String, TestMarker] = Map.empty

  private def checkForDuplicate(marker: TestMarker) {
    markers.get(marker.marker) match {
      case None => markers(marker.marker) = marker
      case Some(otherMarker) =>
        val msg = "Marker `%s` is already used by %s. Please choose a different marker for %s".format(marker.marker, marker, otherMarker)
        throw new DuplicateTestMarker(msg)
    }
  }
}

abstract case class TestMarker(val marker: String) {
  TestMarker.checkForDuplicate(this)
}

object CompletionMarker extends TestMarker("/*!*/")

object TypeMarker extends TestMarker("/*?*/")

object HyperlinkMarker extends TestMarker("/*#*/")