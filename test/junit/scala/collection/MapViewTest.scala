package scala.collection

import org.junit.Assert._
import org.junit.Test

class MapViewTest {
  @Test
  def _toString(): Unit = {
    assertEquals("MapView(<not computed>)", Map(1 -> 2).view.toString)
  }

  @deprecated("Tests deprecated API", since="2.13")
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

  @Test
  def testKeysIsLazy(): Unit = {
    var counter = 0
    def assertLazy(): Unit = assertEquals(0, counter)

    val map = (1 to 10).map(i => i -> i).toMap
    val mapView = map.view.filterKeys(_ => { counter += 1; true })
    assertLazy()
    val keys = mapView.keys
    assert(keys.isInstanceOf[View[_]])
    assertLazy()
    val _ = keys.map(_ + 1)
    assertLazy()
  }

  @Test
  def testValuesIsLazy(): Unit = {
    var counter = 0
    def assertLazy(): Unit = assertEquals(0, counter)

    val map = (1 to 10).map(i => i -> i).toMap
    val mapView = map.view.mapValues(i => { counter += 1; i })
    assertLazy()
    val values = mapView.values
    assert(values.isInstanceOf[View[_]])
    assertLazy()
    val _ = values.map(_ + 1)
    assertLazy()
  }
}
