package strawman.collection.convert

import java.util.{concurrent => juc}
import java.{lang => jl, util => ju}

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import strawman.collection.JavaConverters._
import strawman.collection.{concurrent, mutable}

// scala/bug#9113: tests to insure that wrappers return null instead of wrapping it as a collection

@RunWith(classOf[JUnit4])
class NullSafetyToJavaTest {
  @Test def testIteratorDecoration(): Unit = {
    val nullIterator: strawman.collection.Iterator[AnyRef] = null

    assert(nullIterator.asJava == null)
  }

  @Test def testEnumerationDecoration(): Unit = {
    val nullEnumeration: strawman.collection.Iterator[AnyRef] = null

    assert(nullEnumeration.asJavaEnumeration == null)
  }

  @Test def testIterableDecoration(): Unit = {
    val nullIterable: strawman.collection.Iterable[AnyRef] = null

    assert(nullIterable.asJava == null)
  }

  @Test def testCollectionDecoration(): Unit = {
    val nullCollection: strawman.collection.Iterable[AnyRef] = null

    assert(nullCollection.asJavaCollection == null)
  }

  @Test def testBufferDecoration(): Unit = {
    val nullBuffer: mutable.Buffer[AnyRef] = null

    assert(nullBuffer.asJava == null)
  }

  @Test def testSetDecoration(): Unit = {
    val nullSet: strawman.collection.immutable.Set[AnyRef] = null

    assert(nullSet.asJava == null)
  }

  @Test def testMapDecoration(): Unit = {
    val nullMap: mutable.Map[AnyRef, AnyRef] = null

    assert(nullMap.asJava == null)
  }

  @Test def testConcurrentMapDecoration(): Unit = {
    val nullConMap: concurrent.Map[AnyRef, AnyRef] = null

    assert(nullConMap.asJava == null)
  }

  @Test def testDictionaryDecoration(): Unit = {
    val nullDict: mutable.Map[AnyRef, AnyRef] = null

    assert(nullDict.asJavaDictionary == null)
  }

  // Decorator conversion to ju.Properties is not available
}
