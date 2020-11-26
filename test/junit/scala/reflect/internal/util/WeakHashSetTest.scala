package scala.reflect.internal.util

import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class WeakHashSetTest {

  // a class guaranteed to provide hash collisions
  case class Collider(x : String) extends Comparable[Collider] with Serializable {
    override def hashCode = 0
    def compareTo(y : Collider) = this.x compareTo y.x
  }

  // basic emptiness check
  @Test
  def checkEmpty(): Unit = {
    val hs = new WeakHashSet[String]()
    assertEquals(0, hs.size)
    hs.diagnostics.fullyValidate()
  }

  // make sure += works
  @Test
  def checkPlusEquals(): Unit = {
    val hs = new WeakHashSet[String]()
    val elements = List("hello", "goodbye")
    elements foreach (hs += _)
    assertEquals(2, hs.size)
    assertTrue(hs contains "hello")
    assertTrue(hs contains "goodbye")
    hs.diagnostics.fullyValidate()
  }

  // make sure += works when there are collisions
  @Test
  def checkPlusEqualsCollisions(): Unit = {
    val hs = new WeakHashSet[Collider]()
    val elements = List("hello", "goodbye") map Collider
    elements foreach (hs += _)
    assertEquals(2, hs.size)
    assertTrue(hs contains Collider("hello"))
    assertTrue(hs contains Collider("goodbye"))
    hs.diagnostics.fullyValidate()
  }

  // add a large number of elements to force rehashing and then validate
  @Test
  def checkRehashing(): Unit = {
    val size = 200
    val hs = new WeakHashSet[String]()
    val elements = (0 until size).toList map ("a" + _)
    elements foreach (hs += _)
    elements foreach {i => assertTrue(hs contains i)}
    hs.diagnostics.fullyValidate()
  }

  // make sure rehashing works properly when the set is rehashed
  @Test
  def checkRehashCollisions(): Unit = {
    val size = 200
    val hs = new WeakHashSet[Collider]()
    val elements = (0 until size).toList map {x => Collider("a" + x)}
    elements foreach (hs += _)
    elements foreach {i => assertTrue(hs contains i)}
    hs.diagnostics.fullyValidate()
  }

  // test that unreferenced objects are removed
  // not run in an automated environment because gc behavior can't be relied on
  //@Test
  def checkRemoveUnreferencedObjects(): Unit = {
    val size = 200
    val hs = new WeakHashSet[Collider]()
    val elements = (0 until size).toList map {x => Collider("a" + x)}
    elements foreach (hs += _)
    // don't throw the following into a retained collection so gc
    // can remove them
    for (i <- 0 until size) {
      hs += Collider("b" + i)
    }
    System.gc()
    Thread.sleep(1000)
    assertEquals(200, hs.size)
    elements foreach {i => assertTrue(hs contains i)}
    for (i <- 0 until size) {
      assertFalse(hs contains Collider("b" + i))
    }
    hs.diagnostics.fullyValidate()
  }

  // make sure findOrUpdate returns the originally entered element
  @Test
  def checkFindOrUpdate(): Unit = {
    val size = 200
    val hs = new WeakHashSet[Collider]()
    val elements = (0 until size).toList map {x => Collider("a" + x)}
    elements foreach {x => assertTrue(hs.findEntryOrUpdate(x) eq x)}
    for (i <- 0 until size) {
      // when we do a lookup the result should be the same reference we
      // original put in
      assertTrue(hs.findEntryOrUpdate(Collider("a" + i)) eq elements(i))
    }
    hs.diagnostics.fullyValidate()
  }

  // check -= functionality
  @Test
  def checkMinusEquals(): Unit = {
    val hs = new WeakHashSet[String]()
    val elements = List("hello", "goodbye")
    elements foreach (hs += _)
    hs -= "goodbye"
    assertEquals(1, hs.size)
    assertTrue(hs contains "hello")
    assertFalse(hs contains "goodbye")
    hs.diagnostics.fullyValidate()
  }

  // check -= when there are collisions
  @Test
  def checkMinusEqualsCollisions(): Unit = {
    val hs = new WeakHashSet[Collider]
    val elements = List(Collider("hello"), Collider("goodbye"))
    elements foreach (hs += _)
    hs -= Collider("goodbye")
    assertEquals(1, hs.size)
    assertTrue(hs contains Collider("hello"))
    assertFalse(hs contains Collider("goodbye"))
    hs -= Collider("hello")
    assertEquals(0, hs.size)
    assertFalse(hs contains Collider("hello"))
    hs.diagnostics.fullyValidate()
  }

  // check that the clear method actually cleans everything
  @Test
  def checkClear(): Unit = {
    val size = 200
    val hs = new WeakHashSet[String]()
    val elements = (0 until size).toList map ("a" + _)
    elements foreach (hs += _)
    hs.clear()
    assertEquals(0, hs.size)
    elements foreach {i => assertFalse(hs contains i)}
    hs.diagnostics.fullyValidate()
  }

  // check that the iterator covers all the contents
  @Test
  def checkIterator(): Unit = {
    val hs = new WeakHashSet[String]()
    val elements = (0 until 20).toList map ("a" + _)
    elements foreach (hs += _)
    assertTrue(elements.iterator.toList.sorted == elements.sorted)
    hs.diagnostics.fullyValidate()
  }

  // check that the iterator covers all the contents even when there is a collision
  @Test
  def checkIteratorCollisions(): Unit = {
    val hs = new WeakHashSet[Collider]
    val elements = (0 until 20).toList map {x => Collider("a" + x)}
    elements foreach (hs += _)
    assertTrue(elements.iterator.toList.sorted == elements.sorted)
    hs.diagnostics.fullyValidate()
  }

}
