package scala.collection.convert

import java.util.{concurrent => juc}
import java.{lang => jl, util => ju}

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.JavaConverters._
import scala.collection.convert.ImplicitConversions._
import scala.collection.{concurrent, mutable}

// SI-9113: tests to insure that wrappers return null instead of wrapping it as a collection

@RunWith(classOf[JUnit4])
class NullSafetyToJavaTest {
  @Test def testIteratorWrapping(): Unit = {
    val nullIterator: Iterator[AnyRef] = null
    val jIterator: ju.Iterator[AnyRef] = nullIterator

    assert(jIterator == null)
  }

  @Test def testEnumerationWrapping(): Unit = {
    val nullEnumeration: Iterator[AnyRef] = null
    val enumeration: ju.Iterator[AnyRef] = nullEnumeration

    assert(enumeration == null)
  }

  @Test def testIterableWrapping(): Unit = {
    val nullIterable: Iterable[AnyRef] = null
    val iterable: jl.Iterable[AnyRef] = asJavaIterable(nullIterable)

    assert(iterable == null)
  }

  @Test def testCollectionWrapping(): Unit = {
    val nullCollection: Iterable[AnyRef] = null
    val collection: ju.Collection[AnyRef] = nullCollection

    assert(collection == null)
  }

  @Test def testBufferWrapping(): Unit = {
    val nullList: mutable.Buffer[AnyRef] = null
    val buffer: ju.List[AnyRef] = nullList

    assert(buffer == null)
  }

  @Test def testSetWrapping(): Unit = {
    val nullSet: mutable.Set[AnyRef] = null
    val set: ju.Set[AnyRef] = nullSet

    assert(set == null)
  }

  @Test def testMapWrapping(): Unit = {
    val nullMap: mutable.Map[AnyRef, AnyRef] = null
    val map: ju.Map[AnyRef, AnyRef] = nullMap

    assert(map == null)
  }

  @Test def testConcurrentMapWrapping(): Unit = {
    val nullConMap: concurrent.Map[AnyRef, AnyRef] = null
    val conMap: juc.ConcurrentMap[AnyRef, AnyRef] = nullConMap

    assert(conMap == null)
  }

  @Test def testDictionaryWrapping(): Unit = {
    val nullDict: mutable.Map[AnyRef, AnyRef] = null
    val dict: ju.Dictionary[AnyRef, AnyRef] = nullDict

    assert(dict == null)
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
