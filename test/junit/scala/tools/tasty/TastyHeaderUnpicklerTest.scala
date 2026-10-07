package scala.tools.tasty

import org.junit.Assert._
import org.junit.Test

import scala.collection.mutable.ArrayBuffer
import scala.tools.nsc.tasty.TastyUnpickler

class TastyHeaderUnpicklerTest {
  import TastyHeaderUnpicklerTest._

  val currentTool = TastyVersion(TastyFormat.MajorVersion, TastyFormat.MinorVersion, TastyFormat.ExperimentalVersion)

  @Test def readCurrentVersion(): Unit = {
    runTest(currentTool, toolingVersion = "Scala 3.7.4")
  }

  @Test def readOlderStableMinor(): Unit = {
    runTest(TastyVersion(TastyFormat.MajorVersion, 0, 0), toolingVersion = "Scala 3.0.0")
  }

  @Test def failNewerStableMinor(): Unit = {
    // scala/bug#13152: Scala 3.8+ TASTy must be rejected with an explanation that
    // the Scala 2 TASTy reader will never support it.
    val file = TastyVersion(TastyFormat.MajorVersion, TastyFormat.MinorVersion + 1, 0)
    expectUnpickleError(file, toolingVersion = s"Scala 3.${file.minor}.0") { msg =>
      assertTrue(msg, msg.contains(s"Forward incompatible TASTy file has version ${file.show}"))
      assertTrue(msg, msg.contains(s"expected stable TASTy from 28.0 to 28.${currentTool.minor}"))
      assertTrue(msg, msg.contains(s"only supports Scala 3 versions up to 3.${currentTool.minor}"))
    }
  }

  @Test def failNewerExperimentalMinor(): Unit = {
    // e.g. a library built with a Scala 3.8 nightly
    val file = TastyVersion(TastyFormat.MajorVersion, TastyFormat.MinorVersion + 1, 1)
    expectUnpickleError(file, toolingVersion = s"Scala 3.${file.minor}.0-RC1-bin-SNAPSHOT") { msg =>
      assertTrue(msg, msg.contains(s"only supports Scala 3 versions up to 3.${currentTool.minor}"))
      // must hit the "unsupported version" branch, not the generic experimental advice
      assertFalse(msg, msg.contains("use a stable version of the library"))
    }
  }

  @Test def failExperimentalSameMinor(): Unit = {
    // e.g. a library built with a Scala 3.7 nightly: recompile it with the stable release
    val file = TastyVersion(TastyFormat.MajorVersion, TastyFormat.MinorVersion, 1)
    expectUnpickleError(file, toolingVersion = s"Scala 3.${file.minor}.0-RC1-bin-SNAPSHOT") { msg =>
      assertTrue(msg, msg.contains("Backward incompatible TASTy file"))
      assertTrue(msg, msg.contains(s"recompiled by a Scala 3.${file.minor}.0 compiler or newer"))
    }
  }

  @Test def toolOverridesExtendAcceptedVersions(): Unit = {
    val overrideVersion = TastyVersion(TastyFormat.MajorVersion, TastyFormat.MinorVersion + 1, 1)
    val config = configWithOverrides(List(overrideVersion))
    // accepted via the override
    unpickle(overrideVersion, "Scala next nightly", config)
    // the tool's own version remains accepted
    unpickle(currentTool, "Scala 3.7.4", config)
    // other versions are still rejected
    val rejected = TastyVersion(TastyFormat.MajorVersion, TastyFormat.MinorVersion + 2, 0)
    try {
      unpickle(rejected, "Scala newer", config)
      fail("expected an UnpickleException")
    } catch { case _: UnpickleException => () }
  }
}

object TastyHeaderUnpicklerTest {

  def fillHeader(fileVersion: TastyVersion, toolingVersion: String): Array[Byte] = {
    val buf = new ArrayBuffer[Byte]
    TastyFormat.header.foreach(b => buf += b.toByte)
    writeNat(buf, fileVersion.major)
    writeNat(buf, fileVersion.minor)
    writeNat(buf, fileVersion.experimental)
    val tooling = toolingVersion.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    writeNat(buf, tooling.length)
    buf ++= tooling
    for (_ <- 0 until 16) buf += 0.toByte // uuid
    buf.toArray
  }

  // big endian base 128, bit 0x80 marks the last digit (see TastyReader.readNat)
  private def writeNat(buf: ArrayBuffer[Byte], x: Int): Unit = {
    assert(x >= 0)
    var digits = List((x & 0x7f) | 0x80)
    var rest = x >>> 7
    while (rest != 0) {
      digits ::= rest & 0x7f
      rest >>>= 7
    }
    digits.foreach(d => buf += d.toByte)
  }

  def configWithOverrides(overrides: List[TastyVersion]): UnpicklerConfig =
    new UnpicklerConfig with UnpicklerConfig.DefaultTastyVersion {
      val toolOverrides: List[TastyVersion] = overrides
      def upgradeReaderHowTo(version: TastyVersion): String = "upgrade the reader"
      def upgradedProducerTool(version: TastyVersion): String = "a newer producer"
      def recompileAdditionalInfo: String = ""
      def upgradeAdditionalInfo(fileVersion: TastyVersion): String = ""
    }

  def unpickle(fileVersion: TastyVersion, toolingVersion: String, config: UnpicklerConfig): Unit = {
    val bytes = fillHeader(fileVersion, toolingVersion)
    new TastyHeaderUnpickler(config, bytes).readHeader()
    ()
  }

  def runTest(fileVersion: TastyVersion, toolingVersion: String): Unit =
    unpickle(fileVersion, toolingVersion, TastyUnpickler.scala2CompilerConfig)

  def expectUnpickleError(fileVersion: TastyVersion, toolingVersion: String)(checkMessage: String => Unit): Unit = {
    try {
      runTest(fileVersion, toolingVersion)
      fail("expected an UnpickleException")
    }
    catch { case err: UnpickleException => checkMessage(err.getMessage) }
  }
}
