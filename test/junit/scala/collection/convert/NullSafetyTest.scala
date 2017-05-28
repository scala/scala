package scala.collection.convert

import java.{util => ju, lang => jl}
import ju.{concurrent => juc}

import org.junit.Test
import org.junit.experimental.runners.Enclosed
import org.junit.runner.RunWith

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.collection.{mutable, concurrent}

@RunWith(classOf[Enclosed])
object NullSafetyTest {

    /*
     * Pertinent: SI-9113
     * Tests to insure that wrappers return null instead of wrapping it as a collection
     */

    class ToScala {

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

    class ToJava {

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
}
