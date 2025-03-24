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

package scala.tools.nsc.tasty

import scala.collection.mutable
import scala.tools.tasty.{ErasedTypeRef, Signature, TastyName, TastyReader, TastyRefs}
import scala.tools.tasty.{AttributeUnpickler, Attributes}
import scala.tools.tasty.{TastyFormat, TastyHeaderUnpickler, TastyVersion, UnpicklerConfig}
import TastyFormat.NameTags._
import TastyRefs.NameRef
import TastyName._
import scala.reflect.io.AbstractFile

/**The entry point to TASTy unpickling for nsc, initialises a `TastyUniverse#Context` with the root symbols of a
 * top-level class, then parses the header and names from a TASTy file, before entering symbols from the `ASTs` section
 * with `TreeUnpickler`
 */
object TastyUnpickler {

  /** Unpickle symbol table information descending from a class and/or singleton object root
   *  from an array of bytes.
   *  @param tasty      the interface that translates TASTy operations into symbol table operations
   *  @param classRoot  the top-level class which is unpickled
   *  @param objectRoot the top-level singleton object which is unpickled
   *  @param filename   filename associated with bytearray, only used for error messages
   */
  def unpickle[Tasty <: TastyUniverse](tasty: Tasty)(bytes: Array[Byte], classRoot: tasty.Symbol, objectRoot: tasty.Symbol, filename: String): Unit = {
    import tasty._
    implicit val ctx: Context = new InitialContext(classRoot, AbstractFile.getFile(filename))

    ctx.log(s"Unpickling $filename")

    def enter(treeUnpickler: TreeUnpickler[tasty.type])(implicit ctx: Context): Unit = {
      treeUnpickler.enterTopLevel(classRoot, objectRoot)
    }

    val unpickler = new TastyUnpickler[tasty.type](new TastyReader(bytes))(tasty)
    unpickler.readHeader()
    unpickler.readNames()
    val Some(astReader) = unpickler.readSection(TastyFormat.ASTsSection): @unchecked

    val attributes = unpickler
      .readSection(TastyFormat.AttributesSection)
      .map(AttributeUnpickler.attributes)
      .getOrElse(Attributes.empty)

    val treeUnpickler = new TreeUnpickler[tasty.type](astReader, unpickler.nameAtRef)(tasty)
    val ctx0 = if (attributes.isJava) ctx.addMode(TastyModes.ReadJava) else ctx
    enter(treeUnpickler)(ctx0)
  }

  private final class Table[T] extends (NameRef => T) {
    private[this] val names = new mutable.ArrayBuffer[T]
    def add(name: T): mutable.ArrayBuffer[T] = names += name
    def apply(ref: NameRef): T = names(ref.index)
    def size: Int = names.size
  }

  trait Scala2CompilerConfig extends UnpicklerConfig {

    /** When Scala 3 is in an RC phase for a new minor version, we put here the TASTy of that Minor,
     * otherwise it should be empty.
     */
    final val toolOverrides: List[TastyVersion] = List()

    private def asScala3Compiler(version: TastyVersion): String =
      if (version.major == 28) {
        // scala 3.x.y series
        if (version.experimental > 0)
          // scenario here is someone using 3.4.0 to read 3.4.1-RC1-NIGHTLY, in this case, we should show 3.4 nightly.
          s"the same nightly or snapshot Scala 3.${version.minor - 1} compiler"
        else s"a Scala 3.${version.minor}.0 compiler or newer"
      }
      else if (version.experimental > 0) "the same Scala compiler" // unknown major version, just say same
      else "a more recent Scala compiler" // unknown major version, just say later

    /** The description of the upgraded scala compiler that can read the given TASTy version */
    final def upgradeReaderHowTo(version: TastyVersion): String =
      if (version.major == 28) {
        // scala 3.x.y series
        if (version.experimental > 0)
          // scenario here is someone using 2.13.12 to read 3.4.1-RC1-NIGHTLY, in this case
          // Scala 2.13 can not read it.
          s"either use a stable version of the library, or try from the same Scala 3.x nightly or snapshot compiler"
        else "use the latest Scala 2.13.x compiler" // happy path, they have stable TASTy, but this 2.13.x is too old.
      }
      else if (version.experimental > 0) "use the same Scala compiler" // unknown major version, just say same
      else "use a more recent Scala compiler" // unknown major version, just say later

    /** The description of the upgraded scala compiler that can produce the given TASTy version */
    final def upgradedProducerTool(version: TastyVersion): String = asScala3Compiler(version)

    final def recompileAdditionalInfo: String = """
      |  Usually this means that the library dependency containing this file should be updated.""".stripMargin

    final def upgradeAdditionalInfo(fileVersion: TastyVersion): String =
      if (fileVersion.isExperimental && toolVersion.experimental == 0) {
        """
          |  Note that Scala 2.13.x is only configured to read stable TASTy.""".stripMargin
      }
      else ""
  }

  /** A config for the TASTy reader of a scala 2 compiler */
  val scala2CompilerConfig: UnpicklerConfig = new Scala2CompilerConfig with UnpicklerConfig.DefaultTastyVersion {}
}

import TastyUnpickler._

private class TastyUnpickler[Tasty <: TastyUniverse](reader: TastyReader)(implicit tasty: Tasty) { self =>
  import tasty.{Context, assert}
  import reader._

  private[this] val nameTable = new Table[TastyName]

  def nameAtRef: NameRef => TastyName = nameTable

  private def readName(): TastyName = nameTable(readNameRef())

  private def readParamSig(): Signature.ParamSig[ErasedTypeRef] = {
    val ref = readInt()
    if (ref < 0)
      Left(ref.abs)
    else {
      Right(ErasedTypeRef(nameTable(NameRef(ref))))
    }
  }

  private def readNameContents()(implicit ctx: Context): TastyName = {
    val tag = readByte()
    val length = readNat()
    val start = currentAddr
    val end = start + length
    def debugName(name: TastyName): name.type = {
      ctx.log(s"${nameTable.size}: ${name.debug}")
      name
    }
    def readSignedRest(original: TastyName, target: TastyName): TastyName = {
      val result = ErasedTypeRef(readName())
      val paramsSig = until(end)(readParamSig())
      val sig = Signature(paramsSig, result)
      debugName(SignedName(original, sig, target))
    }
    val result = tag match {
      case UTF8 =>
        goto(end)
        debugName(SimpleName(new String(bytes.slice(start.index, start.index + length), "UTF-8")))
      case tag @ (QUALIFIED | EXPANDED | EXPANDPREFIX) =>
        val sep = tag match {
          case QUALIFIED    => PathSep
          case EXPANDED     => ExpandedSep
          case EXPANDPREFIX => ExpandPrefixSep
        }
        debugName(QualifiedName(readName(), sep, readName().asSimpleName))
      case UNIQUE =>
        val separator = readName().asSimpleName
        val num       = readNat()
        val originals = until(end)(readName())
        val original = if (originals.isEmpty) TastyName.Empty else originals.head
        debugName(UniqueName(original, separator, num))
      case DEFAULTGETTER =>
        debugName(DefaultName(readName(), readNat()))
      case TARGETSIGNED =>
        val original = readName()
        val target = readName()
        readSignedRest(original, target)
      case SIGNED =>
        val original = readName()
        readSignedRest(original, original)
      case OBJECTCLASS =>
        debugName(ObjectName(readName()))
      case BODYRETAINER =>
        debugName(SuffixName(readName(), BodyRetainerSuffix))
      case INLINEACCESSOR | SUPERACCESSOR =>
        val prefix = tag match {
          case INLINEACCESSOR => InlinePrefix
          case SUPERACCESSOR  => SuperPrefix
        }
        debugName(PrefixName(prefix, readName()))
      case _ =>
        val qual = readName()
        sys.error(s"at NameRef(${nameTable.size}): name `${qual.debug}` is qualified by unknown tag $tag")
    }
    assert(currentAddr == end, s"bad name ${result.debug} $start $currentAddr $end")
    result
  }

  def readHeader(): Unit = new TastyHeaderUnpickler(scala2CompilerConfig, reader).readHeader()

  def readNames()(implicit ctx: Context): Unit = {
    ctx.log(s"reading names:")
    doUntil(readEnd()) { nameTable.add(readNameContents()) }
  }

  def readSection(name: String): Option[TastyReader] = {
    while (!isAtEnd) {
      val secName = readName().asSimpleName.raw
      val secEnd  = readEnd()
      val curr    = currentAddr
      goto(secEnd)
      if (name == secName) return Some(new TastyReader(bytes, curr.index, secEnd.index, curr.index))
    }
    None
  }
}
