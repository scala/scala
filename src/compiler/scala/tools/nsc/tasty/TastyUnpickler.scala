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

package scala.tools.nsc.tasty

import scala.collection.mutable
import scala.tools.tasty.{ErasedTypeRef, Signature, TastyFormat, TastyHeaderUnpickler, TastyName, TastyReader, TastyRefs}
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

    val unpickler = new TastyUnpickler[tasty.type](new TastyReader(bytes))(tasty)
    unpickler.readHeader()
    unpickler.readNames()
    val Some(astReader) = unpickler.readSection(TastyFormat.ASTsSection): @unchecked
    val treeUnpickler = new TreeUnpickler[tasty.type](astReader, unpickler.nameAtRef)(tasty)
    treeUnpickler.enterTopLevel(classRoot, objectRoot)
  }

  private final class Table[T] extends (NameRef => T) {
    private[this] val names = new mutable.ArrayBuffer[T]
    def add(name: T): mutable.ArrayBuffer[T] = names += name
    def apply(ref: NameRef): T = names(ref.index)
    def size: Int = names.size
  }
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

  def readHeader(): Unit = new TastyHeaderUnpickler(reader).readHeader()

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
