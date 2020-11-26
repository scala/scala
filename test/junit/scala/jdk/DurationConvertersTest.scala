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

package scala.jdk

import java.time.{Duration => JavaDuration}

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

import scala.AdaptedArrowAssocWorkaround.Tx
import scala.concurrent.duration._
import scala.jdk.DurationConverters._
import scala.jdk.javaapi.{DurationConverters => conv}
import scala.util.Try

class DurationConvertersTest {
  @Test
  def scalaNanosToJavaDuration(): Unit = {
    Seq[(Long, (Long, Int))](
      (Long.MinValue + 1) -> Tx(-9223372037L, 145224193), // because java duration nanos are offset from the "wrong" direction
      -1000000001L        -> Tx(-2, 999999999),
      -1L                 -> Tx(-1, 999999999),
      0L                  -> Tx(0, 0),
      1L                  -> Tx(0, 1),
      1000000001L         -> Tx(1,1),
      Long.MaxValue       -> Tx(9223372036L, 854775807)
    ).foreach { case (n, (expSecs, expNanos)) =>
      val result = n.nanos.toJava
      assertEquals(s"toJava($n nanos) -> $expSecs s)", expSecs, result.getSeconds)
      assertEquals(s"toJava($n nanos) -> $expNanos n)", expNanos, result.getNano)
    }
  }

  @Test
  def scalaMilliSecondsToJavaDuration(): Unit = {
    Seq[(Long, (Long, Int))](
      -9223372036854L -> Tx(-9223372037L, 146000000),
      -1L             -> Tx(-1L, 999000000),
      0L              -> Tx(0L,  0),
      1L              -> Tx(0L,  1000000),
      9223372036854L  -> Tx(9223372036L, 854000000)
    ).foreach { case (n, (expSecs, expNanos)) =>
      val result = n.millis.toJava
      assertEquals(s"toJava($n millis) -> $expSecs s)", expSecs, result.getSeconds)
      assertEquals(s"toJava($n millis) -> $expNanos n)", expNanos, result.getNano)
    }
  }

  @Test
  def scalaMicroSecondsToJavaDuration(): Unit = {
    Seq[(Long, (Long, Int))](
      -9223372036854775L -> Tx(-9223372037L, 145225000),
      -1L                -> Tx(-1L, 999999000),
      0L                 -> Tx(0L,  0),
      1L                 -> Tx(0L,  1000),
      9223372036854775L  -> Tx(9223372036L, 854775000)
    ).foreach { case (n, (expSecs, expNanos)) =>
      val result = n.micros.toJava
      assertEquals(s"toJava($n micros) -> $expSecs s)", expSecs, result.getSeconds)
      assertEquals(s"toJava($n micros) -> $expNanos n)", expNanos, result.getNano)
    }
  }

  @Test
  def scalaSecondsToJavaDuration(): Unit = {
    Seq[(Long, (Long, Int))](
      -9223372036L -> Tx(-9223372036L, 0),
      -1L          -> Tx(-1L, 0),
      0L           -> Tx(0L,  0),
      1L           -> Tx(1L,  0),
      9223372036L  -> Tx(9223372036L, 0)
    ).foreach { case (n, (expSecs, expNanos)) =>
      val result = n.seconds.toJava
      assertEquals(expSecs, result.getSeconds)
      assertEquals(expNanos, result.getNano)
    }
  }


  @Test
  def javaSecondsToScalaDuration(): Unit = {
    Seq[Long](-9223372036L, -1L, 0L, 1L, 9223372036L).foreach { n =>
      assertEquals(n, conv.toScala(JavaDuration.ofSeconds(n)).toSeconds)
    }
  }


  @Test
  def javaNanosPartToScalaDuration(): Unit = {
    val nanosPerSecond = 1000000000L
    Seq[Long](-nanosPerSecond - 1L, 0L, 1L, nanosPerSecond - 1L).foreach { n =>
      assertEquals(n, conv.toScala(JavaDuration.ofNanos(n)).toNanos)
    }
  }

  @Test
  def unsupportedJavaDurationThrows(): Unit = {
    Seq(JavaDuration.ofSeconds(-9223372037L), JavaDuration.ofSeconds(9223372037L)).foreach { d =>
      val res = Try { conv.toScala(d) }
      assertTrue(s"Expected exception for $d but got success", res.isFailure)
    }
  }
}
