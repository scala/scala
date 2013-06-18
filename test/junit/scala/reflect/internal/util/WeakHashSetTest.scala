package scala.reflect.internal.util

import org.junit.Assert._
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
  def checkEmpty {
    val hs = new WeakHashSet[String]()
    assert(hs.size == 0)
    hs.diagnostics.fullyValidate
  }

  // make sure += works
  @Test
  def checkPlusEquals {
    val hs = new WeakHashSet[String]()
    val elements = List("hello", "goodbye")
    elements foreach (hs += _)
    assert(hs.size == 2)
    assert(hs contains "hello")
    assert(hs contains "goodbye")
    hs.diagnostics.fullyValidate
  }

  // make sure += works when there are collisions
  @Test
  def checkPlusEqualsCollisions {
    val hs = new WeakHashSet[Collider]()
    val elements = List("hello", "goodbye") map Collider
    elements foreach (hs += _)
    assert(hs.size == 2)
    assert(hs contains Collider("hello"))
    assert(hs contains Collider("goodbye"))
    hs.diagnostics.fullyValidate
  }

  // add a large number of elements to force rehashing and then validate
  @Test
  def checkRehashing {
    val size = 200
    val hs = new WeakHashSet[String]()
    val elements = (0 until size).toList map ("a" + _)
    elements foreach (hs += _)
    elements foreach {i => assert(hs contains i)}
    hs.diagnostics.fullyValidate
  }

  // make sure rehashing works properly when the set is rehashed
  @Test
  def checkRehashCollisions {
    val size = 200
    val hs = new WeakHashSet[Collider]()
    val elements = (0 until size).toList map {x => Collider("a" + x)}
    elements foreach (hs += _)
    elements foreach {i => assert(hs contains i)}
    hs.diagnostics.fullyValidate
  }

  // test that unreferenced objects are removed
  // not run in an automated environment because gc behavior can't be relied on
  //@Test
  def checkRemoveUnreferencedObjects {
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
    assert(hs.size == 200)
    elements foreach {i => assert(hs contains i)}
    for (i <- 0 until size) {
      assert(!(hs contains Collider("b" + i)))
    }
    hs.diagnostics.fullyValidate
  }

  // make sure findOrUpdate returns the originally entered element
  @Test
  def checkFindOrUpdate {
    val size = 200
    val hs = new WeakHashSet[Collider]()
    val elements = (0 until size).toList map {x => Collider("a" + x)}
    elements foreach {x => assert(hs findEntryOrUpdate x eq x)}
    for (i <- 0 until size) {
      // when we do a lookup the result should be the same reference we
      // original put in
      assert(hs findEntryOrUpdate(Collider("a" + i)) eq elements(i))
    }
    hs.diagnostics.fullyValidate
  }

  // check -= functionality
  @Test
  def checkMinusEquals {
    val hs = new WeakHashSet[String]()
    val elements = List("hello", "goodbye")
    elements foreach (hs += _)
    hs -= "goodbye"
    assert(hs.size == 1)
    assert(hs contains "hello")
    assert(!(hs contains "goodbye"))
    hs.diagnostics.fullyValidate
  }

  // check -= when there are collisions
  @Test
  def checkMinusEqualsCollisions {
    val hs = new WeakHashSet[Collider]
    val elements = List(Collider("hello"), Collider("goodbye"))
    elements foreach (hs += _)
    hs -= Collider("goodbye")
    assert(hs.size == 1)
    assert(hs contains Collider("hello"))
    assert(!(hs contains Collider("goodbye")))
    hs -= Collider("hello")
    assert(hs.size == 0)
    assert(!(hs contains Collider("hello")))
    hs.diagnostics.fullyValidate
  }

  // check that the clear method actually cleans everything
  @Test
  def checkClear {
    val size = 200
    val hs = new WeakHashSet[String]()
    val elements = (0 until size).toList map ("a" + _)
    elements foreach (hs += _)
    hs.clear()
    assert(hs.size == 0)
    elements foreach {i => assert(!(hs contains i))}
    hs.diagnostics.fullyValidate
  }

  // check that the iterator covers all the contents
  @Test
  def checkIterator {
    val hs = new WeakHashSet[String]()
    val elements = (0 until 20).toList map ("a" + _)
    elements foreach (hs += _)
    assert(elements.iterator.toList.sorted == elements.sorted)
    hs.diagnostics.fullyValidate
  }

  // check that the iterator covers all the contents even when there is a collision
  @Test
  def checkIteratorCollisions {
    val hs = new WeakHashSet[Collider]
    val elements = (0 until 20).toList map {x => Collider("a" + x)}
    elements foreach (hs += _)
    assert(elements.iterator.toList.sorted == elements.sorted)
    hs.diagnostics.fullyValidate
  }

}
