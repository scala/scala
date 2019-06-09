package scala.collection

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class MapViewTest {
  @Test
  def _toString(): Unit = {
    assertEquals("MapView(<not computed>)", Map(1 -> 2).view.toString)
  }
  @Test
  def testStringPrefixToString(): Unit = {
    val mapView = new collection.MapView[Int,Int] {
      override def stringPrefix = "FooMapView" // !
      def iterator: Iterator[(Int,Int)] = ???
      def get(key: Int) = None
    }
    assertEquals("FooMapView(<not computed>)", mapView.toString)
  }
  @Test
  def testClassNameToString(): Unit = {
    val mapView = new collection.MapView[Int,Int] {
      override def className = "FooMapView"         // !
      def iterator: Iterator[(Int,Int)] = ???
      def get(key: Int) = None
    }
    assertEquals("FooMapView(<not computed>)", mapView.toString)
  }
}
