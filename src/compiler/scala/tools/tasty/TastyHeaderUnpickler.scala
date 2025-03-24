/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
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

// /**
//  * The Tasty Header consists of four fields:
//  * - uuid
//  *   - contains a hash of the sections of the TASTy file
//  * - majorVersion
//  *   - matching the TASTy format version that last broke backwards compatibility
//  * - minorVersion
//  *   - matching the TASTy format version that last broke forward compatibility
//  * - experimentalVersion
//  *   - 0 for final compiler version
//  *   - positive for between minor versions and forward compatibility
//  *     is broken since the previous stable version.
//  * - toolingVersion
//  *   - arbitrary string representing the tooling that produced the TASTy
//  */
// sealed abstract case class TastyHeader(
//   uuid: UUID,
//   majorVersion: Int,
//   minorVersion: Int,
//   experimentalVersion: Int,
//   toolingVersion: String
// )

trait UnpicklerConfig {
  /** The TASTy version that this reader supports */
  def toolVersion: TastyVersion
  /** TASTy versions that this tool can pretend to be (e.g. for testing against Scala 3 RCs).
   *  Even though this can accept experimental versions, nsc will still forbid usage
   *  of experimental API (behave as a stable compiler).
   */
  def toolOverrides: List[TastyVersion]
  /** The description of the upgraded tool that can read the given TASTy version */
  def upgradeReaderHowTo(version: TastyVersion): String
  /** The description of the upgraded tool that can produce the given TASTy version */
  def upgradedProducerTool(version: TastyVersion): String
  /** Additional information to help a user fix the outdated TASTy problem */
  def recompileAdditionalInfo: String
  /** Additional information to help a user fix the more recent TASTy problem */
  def upgradeAdditionalInfo(fileVersion: TastyVersion): String
}

object UnpicklerConfig {

  /** A config where its major, minor and experimental versions are fixed to those in TastyFormat */
  trait DefaultTastyVersion extends UnpicklerConfig {
    override final val toolVersion: TastyVersion = TastyVersion(MajorVersion, MinorVersion, ExperimentalVersion)
  }
}

class TastyHeaderUnpickler(config: UnpicklerConfig, reader: TastyReader) {
  import TastyHeaderUnpickler._
  import reader._

  def this(config: UnpicklerConfig, bytes: Array[Byte]) = this(config, new TastyReader(bytes))
  // def this(reader: TastyReader) = this(UnpicklerConfig.generic, reader)
  // def this(bytes: Array[Byte]) = this(new TastyReader(bytes))

  /** reads and verifies the TASTy version, extracting the UUID */
  def readHeader(): UUID = {
    for (i <- 0 until header.length)
      check(readByte() == header(i), "not a TASTy file")
    val fileMajor = readNat()
    if (fileMajor <= 27) { // old behavior before `tasty-core` 3.0.0-M4
      val fileMinor = readNat()
      val fileVersion = TastyVersion(fileMajor, fileMinor, 0)
      val signature = signatureString(fileVersion, config.toolVersion, what = "Backward", tool = None)
      val fix = recompileFix(config.toolVersion.minStable)
      throw new UnpickleException(signature + fix + tastyAddendum)
    }
    else {
      val fileMinor = readNat()
      val fileExperimental = readNat()
      val toolingVersion = {
        val length = readNat()
        val start = currentAddr
        val end = start + length
        goto(end)
        new String(bytes, start.index, length)
      }

      val validVersion = TastyFormat.isVersionCompatible(
        fileMajor            = fileMajor,
        fileMinor            = fileMinor,
        fileExperimental     = fileExperimental,
        compilerMajor        = config.toolVersion.major,
        compilerMinor        = config.toolVersion.minor,
        compilerExperimental = config.toolVersion.experimental
      )

      val possibles = config.toolOverrides
      val validOverride = possibles.isEmpty || possibles.exists { overrideVersion =>
        TastyFormat.isVersionCompatible(
          fileMajor            = fileMajor,
          fileMinor            = fileMinor,
          fileExperimental     = fileExperimental,
          compilerMajor        = overrideVersion.major,
          compilerMinor        = overrideVersion.minor,
          compilerExperimental = overrideVersion.experimental
        )
      }

      check(validVersion || validOverride, {
        // failure means that the TASTy file cannot be read, therefore it is either:
        // - backwards incompatible major, in which case the library should be recompiled by the minimum stable minor
        //   version supported by this compiler
        // - any experimental in an older minor, in which case the library should be recompiled by the stable
        //   compiler in the same minor.
        // - older experimental in the same minor, in which case the compiler is also experimental, and the library
        //   should be recompiled by the current compiler
        // - forward incompatible, in which case the compiler must be upgraded to the same version as the file.
        val fileVersion = TastyVersion(fileMajor, fileMinor, fileExperimental)

        val compat = Compatibility.failReason(file = fileVersion, read = config.toolVersion)

        val what = if (compat < 0) "Backward" else "Forward"
        val signature = signatureString(fileVersion, config.toolVersion, what, tool = Some(toolingVersion))
        val fix = (
          if (compat < 0) {
            val newCompiler =
              if (compat == Compatibility.BackwardIncompatibleMajor) config.toolVersion.minStable
              else if (compat == Compatibility.BackwardIncompatibleExperimental) fileVersion.nextStable
              else config.toolVersion // recompile the experimental library with the current experimental compiler
            recompileFix(newCompiler)
          }
          else upgradeFix(fileVersion)
        )
        signature + fix + tastyAddendum
      })

      new UUID(readUncompressedLong(), readUncompressedLong())
    }
  }

  def isAtEnd: Boolean = reader.isAtEnd

  private def check(cond: Boolean, msg: => String): Unit = {
    if (!cond) throw new UnpickleException(msg)
  }

  private def signatureString(
      fileVersion: TastyVersion, toolVersion: TastyVersion, what: String, tool: Option[String]) = {
    val optProducedBy = tool.fold("")(t => s", produced by $t")
    s"""$what incompatible TASTy file has version ${fileVersion.show}$optProducedBy,
      |  expected ${toolVersion.validRange}.
      |""".stripMargin
  }

  private def recompileFix(producerVersion: TastyVersion) = {
    val addendum = config.recompileAdditionalInfo
    val newTool = config.upgradedProducerTool(producerVersion)
    s"""  The source of this file should be recompiled by $newTool.$addendum""".stripMargin
  }

  private def upgradeFix(fileVersion: TastyVersion) = {
    val addendum = config.upgradeAdditionalInfo(fileVersion)
    val newToolHowTo = config.upgradeReaderHowTo(fileVersion)
    s"""  To read this ${fileVersion.kind} file, $newToolHowTo.$addendum""".stripMargin
  }

  private def tastyAddendum: String = """
  |  Please refer to the documentation for information on TASTy versioning:
  |  https://docs.scala-lang.org/scala3/reference/language-versions/binary-compatibility.html""".stripMargin
}

object TastyHeaderUnpickler {

  private object Compatibility {
    final val BackwardIncompatibleMajor = -3
    final val BackwardIncompatibleExperimental = -2
    final val ExperimentalRecompile = -1
    final val ExperimentalUpgrade = 1
    final val ForwardIncompatible = 2

    /** Given that file can't be read, extract the reason */
    def failReason(file: TastyVersion, read: TastyVersion): Int =
      if (file.major == read.major && file.minor == read.minor && file.isExperimental && read.isExperimental) {
        if (file.experimental < read.experimental) ExperimentalRecompile // recompile library as compiler is too new
        else ExperimentalUpgrade // they should upgrade compiler as library is too new
      }
      else if (file.major < read.major)
        BackwardIncompatibleMajor // pre 3.0.0
      else if (file.isExperimental && file.major == read.major && file.minor <= read.minor)
        // e.g. 3.4.0 reading 3.4.0-RC1-NIGHTLY, or 3.3.0 reading 3.0.2-RC1-NIGHTLY
        BackwardIncompatibleExperimental
      else ForwardIncompatible
  }
}
