/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection.convert

import java.util.{
  Dictionary,
  Collections,
  Collection => JCollection,
  HashMap    => JMap,
  Hashtable  => JTable,
  Properties => JProperties,
}
import java.lang.{
  Iterable => JIterable
}
import java.util.concurrent.{
  ConcurrentHashMap => JCMap,
}

import org.junit.Assert.{assertEquals, assertNull, assertTrue}
import org.junit.Test

import scala.collection.mutable
import scala.collection.concurrent
import scala.jdk.CollectionConverters._
import scala.tools.testkit.AssertUtil.assertThrows

class CollectionConvertersTest {
  // Binding to null is represented as Some(null).
  // This is consistent with get, which is the basis for getOrElse, lift, unapply
  //
  @Test def `t11894 Map wrapper respects put contract`(): Unit = {
    val sut = new JMap[String, String].asScala
    assertTrue(sut.isInstanceOf[JavaCollectionWrappers.JMapWrapper[_, _]])
    assertThrows[NoSuchElementException](sut("one"))
    assertEquals(None, sut.unapply("one"))
    assertEquals(None, sut.put("one", null))
    assertNull(sut("one"))
    assertEquals(Some(null), sut.unapply("one"))
    assertEquals(Some(null), sut.lift("one"))
    assertEquals(Some(null), sut.put("one", "eins"))
    assertEquals(Some("eins"), sut.put("one", "uno"))
    assertEquals(Some("uno"), sut.remove("one"))
    assertEquals(None, sut.remove("one"))
    assertEquals(None, sut.put("one", null))
    assertEquals(Some(null), sut.remove("one"))
    assertEquals(None, sut.remove("one"))
  }
  @Test def `t11894 Map wrapper iterator witnesses null values`(): Unit = {
    val sut = new JMap[String, String].asScala
    assertTrue(sut.isInstanceOf[JavaCollectionWrappers.JMapWrapper[_, _]])
    assertEquals(None, sut.put("one", null))
    assertEquals(None, sut.put("deuce", "zwey"))
    assertTrue(sut.iterator.find { case (_, v) => v == null }.nonEmpty)
  }
  @Test def `t11894 Dictionary wrapper disallows nulls`(): Unit = {
    val tbl = new JTable[String, String]
    val sut = tbl.asInstanceOf[Dictionary[String, String]].asScala
    assertTrue(sut.isInstanceOf[JavaCollectionWrappers.JDictionaryWrapper[_, _]])
    assertThrows[NullPointerException](sut.put("any", null))
  }
  @Test def `t11894 Properties wrapper enforces Strings`(): Unit = {
    val ps  = new JProperties()
    val sut = ps.asScala
    assertTrue(sut.isInstanceOf[JavaCollectionWrappers.JPropertiesWrapper])
    assertThrows[NullPointerException](sut.put("any", null))
    assertEquals(None, sut.put("one", "eins"))
    assertEquals("eins", sut("one"))
    ps.put("one", new Object())
    assertThrows[ClassCastException](sut.put("one", "uno"))
  }
  @Test def `t11894 JDK concurrent Map impl disallows nulls`(): Unit = {
    val sut = new JCMap[String, String].asScala
    assertTrue(sut.isInstanceOf[JavaCollectionWrappers.JConcurrentMapWrapper[_, _]])
    assertThrows[NullPointerException](sut.put("any", null))
  }

  @Test def `All wrapper respect equals`(): Unit = {
    val thisJList = Collections.emptyList[String]()
    val thatJList = Collections.emptyList[String]()
    assertEquals(thisJList.asScala, thatJList.asScala)

    val thisJIterator = thisJList.iterator()
    val thatJIterator = thatJList.iterator()
    assertEquals(thisJIterator.asScala, thatJIterator.asScala)

    val thisJEnumeration = Collections.emptyEnumeration[String]()
    val thatJEnumeration = Collections.emptyEnumeration[String]()
    assertEquals(thisJEnumeration.asScala, thatJEnumeration.asScala)

    val thisJIterable = thisJList.asInstanceOf[JIterable[String]]
    val thatJIterable = thatJList.asInstanceOf[JIterable[String]]
    assertEquals(thisJIterable.asScala, thatJIterable.asScala)

    val thisJCollection = thisJList.asInstanceOf[JCollection[String]]
    val thatJCollection = thatJList.asInstanceOf[JCollection[String]]
    assertEquals(thisJCollection.asScala, thatJCollection.asScala)

    val thisJSet = Collections.emptySet[String]()
    val thatJSet = Collections.emptySet[String]()
    assertEquals(thisJSet.asScala, thatJSet.asScala)

    val thisJMap = Collections.emptyMap[String, String]()
    val thatJMap = Collections.emptyMap[String, String]()
    assertEquals(thisJMap.asScala, thatJMap.asScala)

    val thisJCMap = new JCMap[String, String]()
    val thatJCMap = new JCMap[String, String]()
    assertEquals(thisJCMap.asScala, thatJCMap.asScala)

    val thisIterator = Iterator.empty[String]
    val thatIterator = Iterator.empty[String]
    assertEquals(thisIterator.asJava, thatIterator.asJava)

    val thisIterable = Iterable.empty[String]
    val thatIterable = Iterable.empty[String]
    assertEquals(thisIterable.asJava, thatIterable.asJava)

    val thisBuffer = mutable.Buffer.empty[String]
    val thatBuffer = mutable.Buffer.empty[String]
    assertEquals(thisBuffer.asJava, thatBuffer.asJava)

    val thisSeq = mutable.Seq.empty[String]
    val thatSeq = mutable.Seq.empty[String]
    assertEquals(thisSeq.asJava, thatSeq.asJava)

    val thisMutableSet = mutable.Set.empty[String]
    val thatMutableSet = mutable.Set.empty[String]
    assertEquals(thisMutableSet.asJava, thatMutableSet.asJava)

    val thisSet = Set.empty[String]
    val thatSet = Set.empty[String]
    assertEquals(thisSet.asJava, thatSet.asJava)

    val thisMutableMap = mutable.Map.empty[String, String]
    val thatMutableMap = mutable.Map.empty[String, String]
    assertEquals(thisMutableMap.asJava, thatMutableMap.asJava)

    val thisMap = Map.empty[String, String]
    val thatMap = Map.empty[String, String]
    assertEquals(thisMap.asJava, thatMap.asJava)

    val thisConcurrentMap = concurrent.TrieMap.empty[String, String]
    val thatConcurrentMap = concurrent.TrieMap.empty[String, String]
    assertEquals(thisConcurrentMap.asJava, thatConcurrentMap.asJava)
  }
}
