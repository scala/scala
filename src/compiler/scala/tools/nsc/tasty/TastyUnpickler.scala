package scala.tools.nsc.tasty

import scala.collection.mutable
import scala.reflect.internal.SymbolTable
import TastyFormat.NameTags._
import TastyRefs.NameRef
import scala.tools.nsc.tasty.Names.TastyName
import TastyName._

object TastyUnpickler {
  class UnpickleException(msg: String) extends RuntimeException(msg)

  abstract class SectionUnpickler[R](val name: String) {
    def unpickle(reader: TastyReader, nameTable: NameRef => TastyName): R
  }

  final class Table[T] extends (NameRef => T) {
    private[this] val names = new mutable.ArrayBuffer[T]
    def add(name: T): mutable.ArrayBuffer[T] = names += name
    def apply(ref: NameRef): T = names(ref.index)
    def size: Int = names.size
  }
}

import TastyUnpickler._

class TastyUnpickler(reader: TastyReader)(implicit u: TastyUniverse) { self =>
  import u.logTasty
  import reader._

  def this(bytes: Array[Byte])(implicit u: TastyUniverse) = this(new TastyReader(bytes))

  private val sectionReader = new mutable.HashMap[String, TastyReader]

  val nameAtRef = new Table[TastyName]

  private def readName(): TastyName = nameAtRef(readNameRef())
  private def readString(): String = readName().toString

  private def readParamSig(): Signature.ParamSig[TastyName] = {
    val ref = readInt()
    if (ref < 0)
      Left(ref.abs)
    else
      Right(nameAtRef(NameRef(ref)))
  }

  private def readNameContents(): TastyName = {
    val tag = readByte()
    val length = readNat()
    val start = currentAddr
    val end = start + length
    val result = tag match {
      case UTF8 =>
        goto(end)
        val res = SimpleName(new String(bytes.slice(start.index, start.index + length), "UTF-8"))
        logTasty(s"${nameAtRef.size}: UTF8 name: $res")
        res
      case tag @ (QUALIFIED | EXPANDED | EXPANDPREFIX) =>
        val sep = tag match {
          case QUALIFIED    => TastyName.PathSep
          case EXPANDED     => TastyName.ExpandedSep
          case EXPANDPREFIX => TastyName.ExpandPrefixSep
        }
        val res = QualifiedName(readName(), sep, readName().asSimpleName)
        logTasty(s"${nameAtRef.size}: QUALIFIED | EXPANDED | EXPANDPREFIX name: $res")
        res
//        qualifiedNameKindOfTag(tag)(readName(), readName().asSimpleName)
      case UNIQUE =>
        val separator = readName().asSimpleName
        val num = readNat()
        val originals = until(end)(readName())
        val original = if (originals.isEmpty) TastyName.Empty else originals.head
        val res = UniqueName(original, separator, num)
        logTasty(s"${nameAtRef.size}: UNIQUE name: $res")
        res
//        uniqueNameKindOfSeparator(separator)(original, num)
      case DEFAULTGETTER | VARIANT =>
        val qual = readName()
        val nat = readNat()
        val res = {
          if (tag == DEFAULTGETTER) DefaultName(qual, nat)
          else VariantName(qual, contravariant = nat == 0)
        }
        logTasty(s"${nameAtRef.size}: DEFAULTGETTER | VARIANT name: $res")
        res// numberedNameKindOfTag(tag)(readName(), readNat())
      case SIGNED =>
        val original  = readName()
        val result    = readName()
        val paramsSig = until(end)(readParamSig())
        val sig       = Signature(paramsSig, result)
        val signed    = SignedName(original, sig)
        logTasty(s"${nameAtRef.size}: SIGNED name: $signed")
        signed
      case _ =>
        val qual = readName() // simpleNameKindOfTag(tag)(readName())
        val res = {
          if (tag == OBJECTCLASS) ModuleName(qual)
          else qual
        }
        logTasty(s"${nameAtRef.size}: ${TastyFormat.astTagToString(tag)} name: $res")
        res
    }
    assert(currentAddr == end, s"bad name $result $start $currentAddr $end")
    result
  }

  new TastyHeaderUnpickler(reader).readHeader()

  locally {
    doUntil(readEnd()) { nameAtRef.add(readNameContents()) }
    while (!isAtEnd) {
      val secName = readString()
      val secEnd = readEnd()
      sectionReader(secName) = new TastyReader(bytes, currentAddr.index, secEnd.index, currentAddr.index)
      goto(secEnd)
    }
  }

  def unpickle[R](sec: SectionUnpickler[R]): Option[R] =
    for (reader <- sectionReader.get(sec.name)) yield
      sec.unpickle(reader, nameAtRef)

  private[nsc] def bytes: Array[Byte] = reader.bytes
}
