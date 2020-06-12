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

import scala.tools.nsc.backend.jvm.DebugInfoBuilder.JSR45Stratum.{FileSectionEntry, RawLineMapping, ScalaStratum}

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

    stratum.addRawLineMapping(RawLineMapping(from = 42, toStart = 142, toEnd = 142))

    val expectedLines = Seq(
      "*L",
      "42:142"
      )

    assert(stratum.lineSectionLines == expectedLines)
  }

  @Test
  def oneToManyMappingStratumTest(): Unit = {
    val stratum = new ScalaStratum

    stratum.addRawLineMapping(RawLineMapping(from = 42, toStart = 142, toEnd = 150))

    val expectedLines = Seq(
      "*L",
      "42:142,9"
      )

    assert(stratum.lineSectionLines == expectedLines)
  }

  @Test
  def zipMappingStratumTest(): Unit = {
    val stratum = new ScalaStratum

    stratum.addRawLineMapping(RawLineMapping(from = 42, toStart = 142, toEnd = 142))
    stratum.addRawLineMapping(RawLineMapping(from = 43, toStart = 143, toEnd = 143))
    stratum.addRawLineMapping(RawLineMapping(from = 44, toStart = 144, toEnd = 144))
    stratum.addRawLineMapping(RawLineMapping(from = 45, toStart = 145, toEnd = 145))
    stratum.addRawLineMapping(RawLineMapping(from = 46, toStart = 146, toEnd = 146))

    val expectedLines = Seq(
      "*L",
      "42,5:142"
      )

    assert(stratum.lineSectionLines == expectedLines)
  }

  @Test
  def multiZipMappingStratumTest(): Unit = {
    val stratum = new ScalaStratum

    stratum.addRawLineMapping(RawLineMapping(from = 42, toStart = 142, toEnd = 144))
    stratum.addRawLineMapping(RawLineMapping(from = 43, toStart = 145, toEnd = 147))
    stratum.addRawLineMapping(RawLineMapping(from = 44, toStart = 148, toEnd = 150))
    stratum.addRawLineMapping(RawLineMapping(from = 45, toStart = 151, toEnd = 153))
    stratum.addRawLineMapping(RawLineMapping(from = 46, toStart = 154, toEnd = 156))

    val expectedLines = Seq(
      "*L",
      "42,5:142,3"
      )

    assert(stratum.lineSectionLines == expectedLines)
  }

  @Test
  def specExampleMappingStratumTest(): Unit = {
    val stratum = new ScalaStratum

    stratum.addRawLineMapping(RawLineMapping(from = 123, toStart = 207, toEnd = 207))
    stratum.addRawLineMapping(RawLineMapping(from = 130, toStart = 210, toEnd = 210))
    stratum.addRawLineMapping(RawLineMapping(from = 131, toStart = 211, toEnd = 211))
    stratum.addRawLineMapping(RawLineMapping(from = 132, toStart = 212, toEnd = 212))
    stratum.addRawLineMapping(RawLineMapping(from = 140, toStart = 250, toEnd = 256))
    stratum.addRawLineMapping(RawLineMapping(from = 160, toStart = 300, toEnd = 301))
    stratum.addRawLineMapping(RawLineMapping(from = 161, toStart = 302, toEnd = 303))
    stratum.addRawLineMapping(RawLineMapping(from = 162, toStart = 304, toEnd = 305))

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

    stratum.addRawLineMapping(RawLineMapping(from = 140, toStart = 250, toEnd = 256))
    stratum.addRawLineMapping(RawLineMapping(from = 130, toStart = 210, toEnd = 210))
    stratum.addRawLineMapping(RawLineMapping(from = 162, toStart = 304, toEnd = 305))
    stratum.addRawLineMapping(RawLineMapping(from = 160, toStart = 300, toEnd = 301))
    stratum.addRawLineMapping(RawLineMapping(from = 131, toStart = 211, toEnd = 211))
    stratum.addRawLineMapping(RawLineMapping(from = 123, toStart = 207, toEnd = 207))
    stratum.addRawLineMapping(RawLineMapping(from = 161, toStart = 302, toEnd = 303))
    stratum.addRawLineMapping(RawLineMapping(from = 132, toStart = 212, toEnd = 212))

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
