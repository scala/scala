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
  HashMap    => JMap,
  Hashtable  => JTable,
  Properties => JProperties,
}
import java.util.concurrent.{
  ConcurrentHashMap => JCMap,
}

import org.junit.Assert.{assertEquals, assertNull, assertTrue}
import org.junit.Test

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
}
