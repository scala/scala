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

package scala

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class OptionConvertersTest {
  import java.util._
  import JavaConvertersOption._

  @Test
  def scalaToEverything(): Unit = {
    val o = Option("fish")
    val n = (None: Option[String])
    val od = Option(2.7)
    val nd = (None: Option[Double])
    val oi = Option(4)
    val ni = (None: Option[Int])
    val ol = Option(-1L)
    val nl = (None: Option[Long])
    assertEquals(o.asJava, Optional.of(o.get))
    assertEquals(n.asJava, Optional.empty[String])
    assertEquals(od.asJava.get: Double, Optional.of(od.get).get: Double, 0)
    assertEquals(nd.asJava, Optional.empty[Double])
    assertEquals(od.asPrimitive, OptionalDouble.of(od.get))
    assertEquals(nd.asPrimitive, OptionalDouble.empty)
    assertEquals(oi.asJava.get: Int, Optional.of(oi.get).get: Int)
    assertEquals(ni.asJava, Optional.empty[Int])
    assertEquals(oi.asPrimitive, OptionalInt.of(oi.get))
    assertEquals(ni.asPrimitive, OptionalInt.empty)
    assertEquals(ol.asJava.get: Long, Optional.of(ol.get).get: Long)
    assertEquals(nl.asJava, Optional.empty[Long])
    assertEquals(ol.asPrimitive, OptionalLong.of(ol.get))
    assertEquals(nl.asPrimitive, OptionalLong.empty)
  }

  @Test
  def javaGenericToEverything(): Unit = {
    val o = Optional.of("fish")
    val n = Optional.empty[String]
    val od = Optional.of(2.7)
    val nd = Optional.empty[Double]
    val oi = Optional.of(4)
    val ni = Optional.empty[Int]
    val ol = Optional.of(-1L)
    val nl = Optional.empty[Long]
    assertEquals(o.asScala, Option(o.get))
    assertEquals(n.asScala, Option.empty[String])
    assertEquals(od.asScala, Option(od.get))
    assertEquals(nd.asScala, Option.empty[Double])
    assertEquals(od.asPrimitive, OptionalDouble.of(od.get))
    assertEquals(nd.asPrimitive, OptionalDouble.empty)
    assertEquals(oi.asScala, Option(oi.get))
    assertEquals(ni.asScala, Option.empty[Int])
    assertEquals(oi.asPrimitive, OptionalInt.of(oi.get))
    assertEquals(ni.asPrimitive, OptionalInt.empty)
    assertEquals(ol.asScala, Option(ol.get))
    assertEquals(nl.asScala, Option.empty[Long])
    assertEquals(ol.asPrimitive, OptionalLong.of(ol.get))
    assertEquals(nl.asPrimitive, OptionalLong.empty)
  }

  @Test
  def javaOptionalDoubleToEverything(): Unit = {
    val o = OptionalDouble.of(2.7)
    val n = OptionalDouble.empty
    assertEquals(o.asScala, Option(o.getAsDouble))
    assertEquals(o.asGeneric.get: Double, Optional.of(o.getAsDouble).get: Double, 0)
    assertEquals(n.asScala, None: Option[Double])
    assertEquals(n.asGeneric, Optional.empty[Double])
  }

  @Test
  def javaOptionalIntToEverything(): Unit = {
    val o = OptionalInt.of(4)
    val n = OptionalInt.empty
    assertEquals(o.asScala, Option(o.getAsInt))
    assertEquals(o.asGeneric.get: Int, Optional.of(o.getAsInt).get: Int)
    assertEquals(n.asScala, None: Option[Int])
    assertEquals(n.asGeneric, Optional.empty[Int])
  }

  @Test
  def javaOptionalLongToEverything(): Unit = {
    val o = OptionalLong.of(-1)
    val n = OptionalLong.empty
    assertEquals(o.asScala, Option(o.getAsLong))
    assertEquals(o.asGeneric.get: Long, Optional.of(o.getAsLong).get: Long)
    assertEquals(n.asScala, None: Option[Long])
    assertEquals(n.asGeneric, Optional.empty[Long])
  }

  @Test
  def nonExtensionConversions(): Unit = {
    assertEquals(toScala(Optional.of("fish")), Option("fish"))
    assertEquals(toScala(Optional.empty[String]), None)
    assertEquals(toJava(Option("fish")), Optional.of("fish"))
    assertEquals(toJava(None: Option[String]), Optional.empty[String])
    assertEquals(toScala(OptionalDouble.of(2.7)), Option(2.7))
    assertEquals(toScala(OptionalDouble.empty), None)
    assertEquals(toScala(OptionalInt.of(4)), Option(4))
    assertEquals(toScala(OptionalInt.empty), None)
    assertEquals(toScala(OptionalLong.of(-1L)), Option(-1L))
    assertEquals(toScala(OptionalLong.empty), None)
  }
}
