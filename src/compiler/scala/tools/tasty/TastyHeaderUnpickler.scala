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

package scala.tools.tasty

import java.util.UUID

import TastyFormat.{MajorVersion, MinorVersion, ExperimentalVersion, header}

class TastyHeaderUnpickler(reader: TastyReader) {
  import TastyHeaderUnpickler._
  import reader._

  def this(bytes: Array[Byte]) = this(new TastyReader(bytes))

  /** reads and verifies the TASTy version, extracting the UUID */
  def readHeader(): UUID = {

    for (i <- 0 until header.length)
      check(readByte() == header(i), "not a TASTy file")
    val fileMajor = readNat()
    if (fileMajor <= 27) { // old behavior before `tasty-core` 3.0.0-RC1
      val fileMinor = readNat()
      val signature = signatureString(fileMajor, fileMinor, 0)
      throw new UnpickleException(signature + backIncompatAddendum + toolingAddendum)
    }
    else {
      val fileMinor = readNat()
      val fileExperimental = readNat()
      val toolingLength = readNat()
      val toolingStart = {
        val start = currentAddr
        val end = start + toolingLength
        goto(end)
        start
      }

      val validVersion = TastyFormat.isVersionCompatible(
        fileMajor            = fileMajor,
        fileMinor            = fileMinor,
        fileExperimental     = fileExperimental,
        compilerMajor        = MajorVersion,
        compilerMinor        = MinorVersion,
        compilerExperimental = ExperimentalVersion
      )

      check(validVersion, {
        val signature = signatureString(fileMajor, fileMinor, fileExperimental)
        val toolingVersion = new String(bytes, toolingStart.index, toolingLength)
        val producedByAddendum = s"\nThe TASTy file was produced by $toolingVersion.$toolingAddendum"
        val msg = (
          if (fileExperimental != 0) unstableAddendum
          else if (fileMajor < MajorVersion) backIncompatAddendum
          else forwardIncompatAddendum
        )
        signature + msg + producedByAddendum
      })

      new UUID(readUncompressedLong(), readUncompressedLong())
    }
  }

  private def check(cond: Boolean, msg: => String): Unit = {
    if (!cond) throw new UnpickleException(msg)
  }
}

object TastyHeaderUnpickler {

  private def toolingAddendum = (
    if (ExperimentalVersion > 0)
      "\nNote that your tooling is currently using an unstable TASTy version."
    else
      ""
  )

  private def signatureString(fileMajor: Int, fileMinor: Int, fileExperimental: Int) = {
    def showMinorVersion(min: Int, exp: Int) = {
      val expStr = if (exp == 0) "" else s" [unstable release: $exp]"
      s"$min$expStr"
    }
    val minorVersion = showMinorVersion(MinorVersion, ExperimentalVersion)
    val fileMinorVersion = showMinorVersion(fileMinor, fileExperimental)
    s"""TASTy signature has wrong version.
      | expected: {majorVersion: $MajorVersion, minorVersion: $minorVersion}
      | found   : {majorVersion: $fileMajor, minorVersion: $fileMinorVersion}
      |
      |""".stripMargin
  }

  private def unstableAddendum =
    """This TASTy file was produced by an unstable release.
      |To read this TASTy file, your tooling must be at the same version.""".stripMargin

  private def backIncompatAddendum =
    """This TASTy file was produced by an earlier release that is not supported anymore.
      |Please recompile this TASTy with a later version.""".stripMargin

  private def forwardIncompatAddendum =
    """This TASTy file was produced by a more recent, forwards incompatible release.
      |To read this TASTy file, please upgrade your tooling.""".stripMargin
}
