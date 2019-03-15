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
import java.util._
import scala.jdk.OptionConverters.Ops._

@RunWith(classOf[JUnit4])
class OptionConvertersTest {
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
    assertEquals(o.toJava, Optional.of(o.get))
    assertEquals(n.toJava, Optional.empty[String])
    assertEquals(od.toJava.get: Double, Optional.of(od.get).get: Double, 0)
    assertEquals(nd.toJava, Optional.empty[Double])
    assertEquals(od.toJavaPrimitive, OptionalDouble.of(od.get))
    assertEquals(nd.toJavaPrimitive, OptionalDouble.empty)
    assertEquals(oi.toJava.get: Int, Optional.of(oi.get).get: Int)
    assertEquals(ni.toJava, Optional.empty[Int])
    assertEquals(oi.toJavaPrimitive, OptionalInt.of(oi.get))
    assertEquals(ni.toJavaPrimitive, OptionalInt.empty)
    assertEquals(ol.toJava.get: Long, Optional.of(ol.get).get: Long)
    assertEquals(nl.toJava, Optional.empty[Long])
    assertEquals(ol.toJavaPrimitive, OptionalLong.of(ol.get))
    assertEquals(nl.toJavaPrimitive, OptionalLong.empty)
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
    assertEquals(o.toScala, Option(o.get))
    assertEquals(n.toScala, Option.empty[String])
    assertEquals(od.toScala, Option(od.get))
    assertEquals(nd.toScala, Option.empty[Double])
    assertEquals(od.toJavaPrimitive, OptionalDouble.of(od.get))
    assertEquals(nd.toJavaPrimitive, OptionalDouble.empty)
    assertEquals(oi.toScala, Option(oi.get))
    assertEquals(ni.toScala, Option.empty[Int])
    assertEquals(oi.toJavaPrimitive, OptionalInt.of(oi.get))
    assertEquals(ni.toJavaPrimitive, OptionalInt.empty)
    assertEquals(ol.toScala, Option(ol.get))
    assertEquals(nl.toScala, Option.empty[Long])
    assertEquals(ol.toJavaPrimitive, OptionalLong.of(ol.get))
    assertEquals(nl.toJavaPrimitive, OptionalLong.empty)
  }

  @Test
  def javaOptionalDoubleToEverything(): Unit = {
    val o = OptionalDouble.of(2.7)
    val n = OptionalDouble.empty
    assertEquals(o.toScala, Option(o.getAsDouble))
    assertEquals(o.toJavaGeneric.get: Double, Optional.of(o.getAsDouble).get: Double, 0)
    assertEquals(n.toScala, None: Option[Double])
    assertEquals(n.toJavaGeneric, Optional.empty[Double])
  }

  @Test
  def javaOptionalIntToEverything(): Unit = {
    val o = OptionalInt.of(4)
    val n = OptionalInt.empty
    assertEquals(o.toScala, Option(o.getAsInt))
    assertEquals(o.toJavaGeneric.get: Int, Optional.of(o.getAsInt).get: Int)
    assertEquals(n.toScala, None: Option[Int])
    assertEquals(n.toJavaGeneric, Optional.empty[Int])
  }

  @Test
  def javaOptionalLongToEverything(): Unit = {
    val o = OptionalLong.of(-1)
    val n = OptionalLong.empty
    assertEquals(o.toScala, Option(o.getAsLong))
    assertEquals(o.toJavaGeneric.get: Long, Optional.of(o.getAsLong).get: Long)
    assertEquals(n.toScala, None: Option[Long])
    assertEquals(n.toJavaGeneric, Optional.empty[Long])
  }

  @Test
  def nonExtensionConversions(): Unit = {
    import scala.jdk.{OptionConverters => conv}
    assertEquals(conv.toScala(Optional.of("fish")), Option("fish"))
    assertEquals(conv.toScala(Optional.empty[String]), None)
    assertEquals(conv.toJava(Option("fish")), Optional.of("fish"))
    assertEquals(conv.toJava(None: Option[String]), Optional.empty[String])
    assertEquals(conv.toScala(OptionalDouble.of(2.7)), Option(2.7))
    assertEquals(conv.toScala(OptionalDouble.empty), None)
    assertEquals(conv.toScala(OptionalInt.of(4)), Option(4))
    assertEquals(conv.toScala(OptionalInt.empty), None)
    assertEquals(conv.toScala(OptionalLong.of(-1L)), Option(-1L))
    assertEquals(conv.toScala(OptionalLong.empty), None)
  }
}
