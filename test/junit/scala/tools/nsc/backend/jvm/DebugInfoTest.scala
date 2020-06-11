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

import org.junit.Test

import scala.tools.nsc.backend.jvm.DebugInfoBuilder.JSR45Stratum.{FileSectionEntry, ScalaStratum}

class DebugInfoTest {

  @Test
  def fileSectionWithoutPathTest(): Unit = {
    val stratum = new ScalaStratum

    stratum.addFileEntry(FileSectionEntry("MyAwesomeClass.scala"))

    val expectedLines = Seq(
      "*F",
      "1 MyAwesomeClass.scala"
    )

    assert(stratum.fileSectionLines == expectedLines)
  }

  @Test
  def fileSectionWithPathTest(): Unit = {
    val stratum = new ScalaStratum

    stratum.addFileEntry(FileSectionEntry("MyAwesomeClass.scala", Some("/foo/bar/MyAwesomeClass.scala")))

    val expectedLines = Seq(
      "*F",
      "+ 1 MyAwesomeClass.scala",
      "/foo/bar/MyAwesomeClass.scala"
      )

    assert(stratum.fileSectionLines == expectedLines)
  }

  @Test
  def emptyLineMappingStratumTest(): Unit = {
    val stratum = new ScalaStratum

    val expectedLines = Seq(
      "*L"
      )

    assert(stratum.lineSectionLines == expectedLines)
  }

  @Test
  def oneToOneMappingStratumTest(): Unit = {
    val stratum = new ScalaStratum

    stratum.addLineMapping(42, 142)

    val expectedLines = Seq(
      "*L",
      "42:142"
      )

    assert(stratum.lineSectionLines == expectedLines)
  }

  @Test
  def oneToManyMappingStratumTest(): Unit = {
    val stratum = new ScalaStratum

    stratum.addLineMapping(42, 142, 150)

    val expectedLines = Seq(
      "*L",
      "42:142,9"
      )

    assert(stratum.lineSectionLines == expectedLines)
  }

  @Test
  def zipMappingStratumTest(): Unit = {
    val stratum = new ScalaStratum

    stratum.addLineMapping(42, 142)
    stratum.addLineMapping(43, 143)
    stratum.addLineMapping(44, 144)
    stratum.addLineMapping(45, 145)
    stratum.addLineMapping(46, 146)

    val expectedLines = Seq(
      "*L",
      "42,5:142"
      )

    assert(stratum.lineSectionLines == expectedLines)
  }

  @Test
  def multiZipMappingStratumTest(): Unit = {
    val stratum = new ScalaStratum

    stratum.addLineMapping(42, 142, 144)
    stratum.addLineMapping(43, 145, 147)
    stratum.addLineMapping(44, 148, 150)
    stratum.addLineMapping(45, 151, 153)
    stratum.addLineMapping(46, 154, 156)

    val expectedLines = Seq(
      "*L",
      "42,5:142,3"
      )

    assert(stratum.lineSectionLines == expectedLines)
  }

  @Test
  def specExampleMappingStratumTest(): Unit = {
    val stratum = new ScalaStratum

    stratum.addLineMapping(123, 207)
    stratum.addLineMapping(130, 210)
    stratum.addLineMapping(131, 211)
    stratum.addLineMapping(132, 212)
    stratum.addLineMapping(140, 250, 256)
    stratum.addLineMapping(160, 300, 301)
    stratum.addLineMapping(161, 302, 303)
    stratum.addLineMapping(162, 304, 305)

    val expectedLines = Seq(
      "*L",
      "123:207",
      "130,3:210",
      "140:250,7",
      "160,3:300,2"
      )

    assert(stratum.lineSectionLines == expectedLines)
  }

  @Test
  def specExampleUnorderedMappingStratumTest(): Unit = {
    val stratum = new ScalaStratum

    stratum.addLineMapping(140, 250, 256)
    stratum.addLineMapping(130, 210)
    stratum.addLineMapping(162, 304, 305)
    stratum.addLineMapping(160, 300, 301)
    stratum.addLineMapping(131, 211)
    stratum.addLineMapping(123, 207)
    stratum.addLineMapping(161, 302, 303)
    stratum.addLineMapping(132, 212)

    val expectedLines = Seq(
      "*L",
      "123:207",
      "130,3:210",
      "140:250,7",
      "160,3:300,2"
      )

    assert(stratum.lineSectionLines == expectedLines)
  }
}
