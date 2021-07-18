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

package scala.tools.nsc.backend.jvm

import scala.collection.mutable

import org.junit.Assert._
import org.junit.Test

class DebugInfoTest {

  import scala.tools.nsc.backend.jvm.DebugInfoBuilder._
  import DebugInfoWriter._
  import JSR45._

  @Test
  def fileSectionWithoutLineSectionTest(): Unit = {
    val stratum = Stratum(name = "Scala",
                          fileSection = Seq(FileSectionEntry("MyAwesomeClass.scala")))

    val expected = ""

    assertEquals(expected, stratum.toString)
  }

  @Test
  def fileSectionWithoutPathTest(): Unit = {
    val stratum = Stratum(name = "Scala",
                          fileSection = Seq(FileSectionEntry("MyAwesomeClass.scala")),
                          lineSection = Seq(LineSectionEntry(inputStartLine = 1, outputStartLine = 1)))

    val expected = Seq(
      "*S Scala",
      "*F",
      "+ 1 MyAwesomeClass.scala",
      "*L",
      "1:1"
    ).mkString("\n")

    assertEquals(expected, stratum.toString)
  }

  @Test
  def fileSectionWithPathTest(): Unit = {
    val stratum = Stratum(name = "Scala",
                          fileSection = Seq(FileSectionEntry("MyAwesomeClass.scala",
                                                             Some("/foo/bar/MyAwesomeClass"))),
                          lineSection = Seq(LineSectionEntry(inputStartLine = 1, outputStartLine = 1)))

    val expected = Seq(
      "*S Scala",
      "*F",
      "+ 1 MyAwesomeClass.scala",
      "/foo/bar/MyAwesomeClass",
      "*L",
      "1:1"
      ).mkString("\n")

    assertEquals(expected, stratum.toString)
  }

  @Test
  def reverseMappingTest(): Unit = {
    val stratum = Stratum(name = "Scala",
                          fileSection = Seq.empty,
                          lineSection = Seq(
                            LineSectionEntry(123, None, None, 207, None),
                            LineSectionEntry(130, None, Some(3), 210, None),
                            LineSectionEntry(140, None, None, 250, Some(7)),
                            LineSectionEntry(160, None, Some(3), 300, Some(2))),
                          lineCount = 17)

    val rmappings = Map(
      207 -> 123,
      210 -> 130,
      211 -> 131,
      212 -> 132,
      250 -> 140,
      251 -> 140,
      252 -> 140,
      253 -> 140,
      254 -> 140,
      255 -> 140,
      256 -> 140,
      300 -> 160,
      301 -> 160,
      302 -> 161,
      303 -> 161,
      304 -> 162,
      305 -> 162
    )

    rmappings.foreach {
      case (to, from) =>
        val m = stratum.findReverseLineMapping(to)
        assertTrue(m.isDefined)
        assertTrue(m.get == RawLineMapping(from, to, None))
    }

    val shouldNotFind = Seq(
      206,
      208,
      209,
      213,
      249,
      257,
      299,
      306
    )

    shouldNotFind.foreach { to =>
      val m = stratum.findReverseLineMapping(to)
      assertFalse(m.isDefined)
    }

  }

  @Test
  def specExampleMappingTest(): Unit = {
    val writer = new DebugInfoBuilder.DebugInfoWriter.JSR45DebugInfoWriter(None)

    writer.writeInlineEntry(InlineMapping(0, 123, "Spec.scala", "/Spec"))(mutable.Map.empty[String, DebugInfoWriter])

    writer.writeInlineEntry(InlineMapping(0, 130, "Spec.scala", "/Spec"))(mutable.Map.empty[String, DebugInfoWriter])
    writer.writeInlineEntry(InlineMapping(0, 131, "Spec.scala", "/Spec"))(mutable.Map.empty[String, DebugInfoWriter])
    writer.writeInlineEntry(InlineMapping(0, 132, "Spec.scala", "/Spec"))(mutable.Map.empty[String, DebugInfoWriter])

    writer.writeInlineEntry(InlineMapping(0, 140, "Spec.scala", "/Spec"))(mutable.Map.empty[String, DebugInfoWriter])
    writer.writeInlineEntry(InlineMapping(0, 140, "Spec.scala", "/Spec"))(mutable.Map.empty[String, DebugInfoWriter])
    writer.writeInlineEntry(InlineMapping(0, 140, "Spec.scala", "/Spec"))(mutable.Map.empty[String, DebugInfoWriter])
    writer.writeInlineEntry(InlineMapping(0, 140, "Spec.scala", "/Spec"))(mutable.Map.empty[String, DebugInfoWriter])
    writer.writeInlineEntry(InlineMapping(0, 140, "Spec.scala", "/Spec"))(mutable.Map.empty[String, DebugInfoWriter])
    writer.writeInlineEntry(InlineMapping(0, 140, "Spec.scala", "/Spec"))(mutable.Map.empty[String, DebugInfoWriter])
    writer.writeInlineEntry(InlineMapping(0, 140, "Spec.scala", "/Spec"))(mutable.Map.empty[String, DebugInfoWriter])

    writer.writeInlineEntry(InlineMapping(0, 160, "Spec.scala", "/Spec"))(mutable.Map.empty[String, DebugInfoWriter])
    writer.writeInlineEntry(InlineMapping(0, 160, "Spec.scala", "/Spec"))(mutable.Map.empty[String, DebugInfoWriter])
    writer.writeInlineEntry(InlineMapping(0, 161, "Spec.scala", "/Spec"))(mutable.Map.empty[String, DebugInfoWriter])
    writer.writeInlineEntry(InlineMapping(0, 161, "Spec.scala", "/Spec"))(mutable.Map.empty[String, DebugInfoWriter])
    writer.writeInlineEntry(InlineMapping(0, 162, "Spec.scala", "/Spec"))(mutable.Map.empty[String, DebugInfoWriter])
    writer.writeInlineEntry(InlineMapping(0, 162, "Spec.scala", "/Spec"))(mutable.Map.empty[String, DebugInfoWriter])

    val expected = Seq(
      "SMAP",
      "",
      "Scala",
      "*S Scala",
      "*F",
      "+ 1 Spec.scala",
      "/Spec",
      "*L",
      "123#1:1",
      "130#1,3:2",
      "140#1:5,7",
      "160#1,3:12,2",
      "*E"
    ).mkString("\n")

    val smap = writer.SMAP()

    assertTrue(smap.isDefined)
    assertEquals(expected, writer.SMAP().get)
  }

}
