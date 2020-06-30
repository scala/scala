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

import scala.tools.nsc.backend.jvm.DebugInfoBuilder.JSR45Stratum.{FileSectionEntry, RawLineMapping, ScalaDebugStratum}

class DebugInfoTest {

  @Test
  def fileSectionWithoutPathTest(): Unit = {
    val stratum = new ScalaDebugStratum

    stratum.addFileEntry(FileSectionEntry("MyAwesomeClass.scala"))

    val expectedLines = Seq(
      "*F",
      "1 MyAwesomeClass.scala"
      )

    assert(stratum.fileSectionLines == expectedLines)
  }

  @Test
  def fileSectionWithPathTest(): Unit = {
    val stratum = new ScalaDebugStratum

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
    val stratum = new ScalaDebugStratum

    val expectedLines = Seq(
      "*L"
      )

    assert(stratum.lineSectionLines == expectedLines)
  }

  @Test
  def oneToOneMappingStratumTest(): Unit = {
    val stratum = new ScalaDebugStratum

    stratum.addRawLineMapping(RawLineMapping(from = 42, toStart = 142, toEnd = 142))

    val expectedLines = Seq(
      "*L",
      "42:142"
      )

    assert(stratum.lineSectionLines == expectedLines)
  }

  @Test
  def oneToOneMappingWithFileIdStratumTest(): Unit = {
    val stratum = new ScalaDebugStratum

    stratum.addRawLineMapping(RawLineMapping(from = 42, toStart = 142, toEnd = 142, sourceFileId = Some(1)))
    stratum.addRawLineMapping(RawLineMapping(from = 55, toStart = 155, toEnd = 155, sourceFileId = Some(2)))

    val expectedLines = Seq(
      "*L",
      "42#1:142",
      "55#2:155"
      )

    assert(stratum.lineSectionLines == expectedLines)
  }

  @Test
  def oneToManyMappingStratumTest(): Unit = {
    val stratum = new ScalaDebugStratum

    stratum.addRawLineMapping(RawLineMapping(from = 42, toStart = 142, toEnd = 150))

    val expectedLines = Seq(
      "*L",
      "42:142,9"
      )

    assert(stratum.lineSectionLines == expectedLines)
  }

  @Test
  def oneToManyMappingWithFileIdStratumTest(): Unit = {
    val stratum = new ScalaDebugStratum

    stratum.addRawLineMapping(RawLineMapping(from = 42, toStart = 142, toEnd = 150, sourceFileId = Some(1)))
    stratum.addRawLineMapping(RawLineMapping(from = 55, toStart = 155, toEnd = 163, sourceFileId = Some(2)))

    val expectedLines = Seq(
      "*L",
      "42#1:142,9",
      "55#2:155,9"
      )

    assert(stratum.lineSectionLines == expectedLines)
  }

  @Test
  def zipMappingStratumTest(): Unit = {
    val stratum = new ScalaDebugStratum

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
  def zipMappingWithFileIdStratumTest(): Unit = {
    val stratum = new ScalaDebugStratum

    stratum.addRawLineMapping(RawLineMapping(from = 42, toStart = 142, toEnd = 142, sourceFileId = Some(1)))
    stratum.addRawLineMapping(RawLineMapping(from = 43, toStart = 143, toEnd = 143, sourceFileId = Some(1)))
    stratum.addRawLineMapping(RawLineMapping(from = 44, toStart = 144, toEnd = 144, sourceFileId = Some(1)))
    stratum.addRawLineMapping(RawLineMapping(from = 45, toStart = 145, toEnd = 145, sourceFileId = Some(1)))

    stratum.addRawLineMapping(RawLineMapping(from = 46, toStart = 146, toEnd = 146, sourceFileId = Some(2)))
    stratum.addRawLineMapping(RawLineMapping(from = 47, toStart = 147, toEnd = 147, sourceFileId = Some(2)))

    stratum.addRawLineMapping(RawLineMapping(from = 48, toStart = 148, toEnd = 148, sourceFileId = Some(1)))
    stratum.addRawLineMapping(RawLineMapping(from = 49, toStart = 149, toEnd = 149, sourceFileId = Some(1)))

    val expectedLines = Seq(
      "*L",
      "42#1,4:142",
      "48#1,2:148",
      "46#2,2:146"
    )

    assert(stratum.lineSectionLines == expectedLines)
  }

  @Test
  def multiZipMappingStratumTest(): Unit = {
    val stratum = new ScalaDebugStratum

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
  def multiZipMappingWithBreakStratumTest(): Unit = {
    val stratum = new ScalaDebugStratum

    stratum.addRawLineMapping(RawLineMapping(from = 42, toStart = 142, toEnd = 144))
    stratum.addRawLineMapping(RawLineMapping(from = 43, toStart = 145, toEnd = 147))
    stratum.addRawLineMapping(RawLineMapping(from = 44, toStart = 148, toEnd = 151))
    stratum.addRawLineMapping(RawLineMapping(from = 45, toStart = 152, toEnd = 154))
    stratum.addRawLineMapping(RawLineMapping(from = 46, toStart = 155, toEnd = 157))
    stratum.addRawLineMapping(RawLineMapping(from = 47, toStart = 158, toEnd = 160))

    val expectedLines = Seq(
      "*L",
      "42,2:142,3",
      "44:148,4",
      "45,3:152,3"
      )

    assert(stratum.lineSectionLines == expectedLines)
  }

  @Test
  def multiZipMappingWithFileIdStratumTest(): Unit = {
    val stratum = new ScalaDebugStratum

    stratum.addRawLineMapping(RawLineMapping(from = 42, toStart = 142, toEnd = 144, sourceFileId = Some(1)))
    stratum.addRawLineMapping(RawLineMapping(from = 43, toStart = 145, toEnd = 147, sourceFileId = Some(1)))
    stratum.addRawLineMapping(RawLineMapping(from = 44, toStart = 148, toEnd = 150, sourceFileId = Some(1)))
    stratum.addRawLineMapping(RawLineMapping(from = 45, toStart = 151, toEnd = 153, sourceFileId = Some(1)))
    stratum.addRawLineMapping(RawLineMapping(from = 46, toStart = 154, toEnd = 156, sourceFileId = Some(1)))

    stratum.addRawLineMapping(RawLineMapping(from = 47, toStart = 157, toEnd = 159, sourceFileId = Some(2)))
    stratum.addRawLineMapping(RawLineMapping(from = 48, toStart = 160, toEnd = 162, sourceFileId = Some(2)))
    stratum.addRawLineMapping(RawLineMapping(from = 49, toStart = 163, toEnd = 165, sourceFileId = Some(2)))

    stratum.addRawLineMapping(RawLineMapping(from = 50, toStart = 166, toEnd = 169, sourceFileId = Some(1)))
    stratum.addRawLineMapping(RawLineMapping(from = 51, toStart = 170, toEnd = 173, sourceFileId = Some(1)))

    val expectedLines = Seq(
      "*L",
      "42#1,5:142,3",
      "50#1,2:166,4",
      "47#2,3:157,3"
      )

    assert(stratum.lineSectionLines == expectedLines)
  }

  @Test
  def specExampleMappingStratumTest(): Unit = {
    val stratum = new ScalaDebugStratum

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
    val stratum = new ScalaDebugStratum

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
