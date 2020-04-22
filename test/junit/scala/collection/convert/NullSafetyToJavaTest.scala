package scala.collection.convert

import java.{lang => jl}

import org.junit.Test

import scala.collection.{concurrent, mutable}
import scala.jdk.CollectionConverters._

// scala/bug#9113: tests to insure that wrappers return null instead of wrapping it as a collection
class NullSafetyToJavaTest {
  @Test def testIterableWrapping(): Unit = {
    val nullIterable: Iterable[AnyRef] = null
    val iterable: jl.Iterable[AnyRef] = nullIterable.asJava

    assert(iterable == null)
  }

  // Implicit conversion to ju.Properties is not available

  @Test def testIteratorDecoration(): Unit = {
    val nullIterator: Iterator[AnyRef] = null

    assert(nullIterator.asJava == null)
  }

  @Test def testEnumerationDecoration(): Unit = {
    val nullEnumeration: Iterator[AnyRef] = null

    assert(nullEnumeration.asJavaEnumeration == null)
  }

  @Test def testIterableDecoration(): Unit = {
    val nullIterable: Iterable[AnyRef] = null

    assert(nullIterable.asJava == null)
  }

  @Test def testCollectionDecoration(): Unit = {
    val nullCollection: Iterable[AnyRef] = null

    assert(nullCollection.asJavaCollection == null)
  }

  @Test def testBufferDecoration(): Unit = {
    val nullBuffer: mutable.Buffer[AnyRef] = null

    assert(nullBuffer.asJava == null)
  }

  @Test def testSetDecoration(): Unit = {
    val nullSet: Set[AnyRef] = null

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
