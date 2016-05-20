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
class NullSafetyToScalaTest {
  @Test def testIteratorWrapping(): Unit = {
    val nullJIterator: ju.Iterator[AnyRef] = null
    val iterator: Iterator[AnyRef] = nullJIterator

    assert(iterator == null)
  }

  @Test def testEnumerationWrapping(): Unit = {
    val nullJEnumeration: ju.Enumeration[AnyRef] = null
    val enumeration: Iterator[AnyRef] = nullJEnumeration

    assert(enumeration == null)
  }

  @Test def testIterableWrapping(): Unit = {
    val nullJIterable: jl.Iterable[AnyRef] = null
    val iterable: Iterable[AnyRef] = nullJIterable

    assert(iterable == null)
  }

  @Test def testCollectionWrapping(): Unit = {
    val nullJCollection: ju.Collection[AnyRef] = null
    val collection: Iterable[AnyRef] = nullJCollection

    assert(collection == null)
  }

  @Test def testBufferWrapping(): Unit = {
    val nullJList: ju.List[AnyRef] = null
    val buffer: mutable.Buffer[AnyRef] = nullJList

    assert(buffer == null)
  }

  @Test def testSetWrapping(): Unit = {
    val nullJSet: ju.Set[AnyRef] = null
    val set: mutable.Set[AnyRef] = nullJSet

    assert(set == null)
  }

  @Test def testMapWrapping(): Unit = {
    val nullJMap: ju.Map[AnyRef, AnyRef] = null
    val map: mutable.Map[AnyRef, AnyRef] = nullJMap

    assert(map == null)
  }

  @Test def testConcurrentMapWrapping(): Unit = {
    val nullJConMap: juc.ConcurrentMap[AnyRef, AnyRef] = null
    val conMap: concurrent.Map[AnyRef, AnyRef] = nullJConMap

    assert(conMap == null)
  }

  @Test def testDictionaryWrapping(): Unit = {
    val nullJDict: ju.Dictionary[AnyRef, AnyRef] = null
    val dict: mutable.Map[AnyRef, AnyRef] = nullJDict

    assert(dict == null)
  }


  @Test def testPropertyWrapping(): Unit = {
    val nullJProps: ju.Properties = null
    val props: mutable.Map[String, String] = nullJProps

    assert(props == null)
  }

  @Test def testIteratorDecoration(): Unit = {
    val nullJIterator: ju.Iterator[AnyRef] = null

    assert(nullJIterator.asScala == null)
  }

  @Test def testEnumerationDecoration(): Unit = {
    val nullJEnumeration: ju.Enumeration[AnyRef] = null

    assert(nullJEnumeration.asScala == null)
  }

  @Test def testIterableDecoration(): Unit = {
    val nullJIterable: jl.Iterable[AnyRef] = null

    assert(nullJIterable.asScala == null)
  }

  @Test def testCollectionDecoration(): Unit = {
    val nullJCollection: ju.Collection[AnyRef] = null

    assert(nullJCollection.asScala == null)
  }

  @Test def testBufferDecoration(): Unit = {
    val nullJBuffer: ju.List[AnyRef] = null

    assert(nullJBuffer.asScala == null)
  }

  @Test def testSetDecoration(): Unit = {
    val nullJSet: ju.Set[AnyRef] = null

    assert(nullJSet.asScala == null)
  }

  @Test def testMapDecoration(): Unit = {
    val nullJMap: ju.Map[AnyRef, AnyRef] = null

    assert(nullJMap.asScala == null)
  }

  @Test def testConcurrentMapDecoration(): Unit = {
    val nullJConMap: juc.ConcurrentMap[AnyRef, AnyRef] = null

    assert(nullJConMap.asScala == null)
  }

  @Test def testDictionaryDecoration(): Unit = {
    val nullJDict: ju.Dictionary[AnyRef, AnyRef] = null

    assert(nullJDict.asScala == null)
  }

  @Test def testPropertiesDecoration(): Unit = {
    val nullJProperties: ju.Properties = null

    assert(nullJProperties.asScala == null)
  }
}
