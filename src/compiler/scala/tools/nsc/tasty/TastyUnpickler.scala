package scala.tools.nsc.tasty

import scala.collection.mutable
import scala.reflect.internal.SymbolTable
import TastyFormat.NameTags._
import TastyRefs.NameRef
import scala.collection.immutable.LongMap

object TastyUnpickler {
  class UnpickleException(msg: String) extends RuntimeException(msg)

  abstract class SectionUnpickler[R](val name: String) {
    def unpickle(reader: TastyReader, nameTable: TastyNameTable with TastyUniverse): R
  }

  final class Table[T] extends (NameRef => T) {
    private[TastyUnpickler] val names = new mutable.ArrayBuffer[T]
    def add(name: T): mutable.ArrayBuffer[T] = names += name
    def apply(ref: NameRef): T = names(ref.index)
    def contents: Iterable[T] = names
  }
}

import TastyUnpickler._

class TastyUnpickler(reader: TastyReader)(implicit val symbolTable: SymbolTable) extends TastyUniverse with TastyNameTable { self =>
  import symbolTable._
  import reader._

  def this(bytes: Array[Byte])(implicit symbolTable: SymbolTable) = this(new TastyReader(bytes))

  private val sectionReader = new mutable.HashMap[String, TastyReader]

  val nameAtRef: Table[TermName] = new Table

  val signedNameTable: mutable.LongMap[SigName] = mutable.LongMap.empty

  val signedNameAtRef: NameRef => Either[SigName, TermName] =
    ref =>
      if (signedNameTable.contains(ref.index))
        Left(signedNameTable(ref.index))
      else
        Right(nameAtRef(ref))

  val modulesAtRefs: mutable.HashSet[Int] = mutable.HashSet.empty

  val moduleRefs: NameRef => Boolean = ref => modulesAtRefs(ref.index)

  private def readName(): TermName = nameAtRef(readNameRef())
  private def readString(): String = readName().toString

  private def readParamSig(): ParamSig = {
    val ref = readInt()
    if (ref < 0)
      Left(ref.abs)
    else
      Right(nameAtRef(NameRef(ref)).toTypeName)
  }

  private def readNameContents(): TermName = {
    val tag = readByte()
    val length = readNat()
    val start = currentAddr
    val end = start + length
    val result = tag match {
      case UTF8 =>
        goto(end)
        val res = newTermName(bytes, start.index, length)
        logTasty(s"${nameAtRef.names.size}: UTF8 name: $res")
        res
      case tag @ (QUALIFIED | EXPANDED | EXPANDPREFIX) =>
        val sep = tag match {
          case QUALIFIED    => "."
          case EXPANDED     => "$$"
          case EXPANDPREFIX => "$"
        }
        val res = newTermName("" + readName() + sep + readName())
        logTasty(s"${nameAtRef.names.size}: QUALIFIED | EXPANDED | EXPANDPREFIX name: $res")
        res
//        qualifiedNameKindOfTag(tag)(readName(), readName().asSimpleName)
      case UNIQUE =>
        val separator = readName().toString
        val num = readNat()
        val originals = until(end)(readName())
        val original = if (originals.isEmpty) termNames.EMPTY else originals.head
        val res = newTermName("" + original + separator + num)
        logTasty(s"${nameAtRef.names.size}: UNIQUE name: $res")
        res
//        uniqueNameKindOfSeparator(separator)(original, num)
      case DEFAULTGETTER | VARIANT =>
        val result = readName()
        val nat = readNat()
        logTasty(s"${nameAtRef.names.size}: DEFAULTGETTER | VARIANT name: $result[$nat]")
        result // numberedNameKindOfTag(tag)(readName(), readNat())
      case SIGNED =>
        val original  = readName()
        val result    = readName().toTypeName
        val paramsSig = until(end)(readParamSig())
        val sig       = Sig(paramsSig, result)
        val signed    = SigName(original, sig)
        signedNameTable(nameAtRef.names.size) = signed
        logTasty(s"${nameAtRef.names.size}: SIGNED name: ${signed.show}")
        original // SignedName(original, sig)
      case _ =>
        val res = readName() // simpleNameKindOfTag(tag)(readName())
        if (tag == OBJECTCLASS) {
          modulesAtRefs += nameAtRef.names.size
        }
        logTasty(s"${nameAtRef.names.size}: ${TastyFormat.astTagToString(tag)} name: $res")
        res
    }
    assert(currentAddr == end, s"bad name $result $start $currentAddr $end")
    result
  }

  new TastyHeaderUnpickler(reader).readHeader()

  locally {
    doUntil(readEnd()) { nameAtRef.add(readNameContents()) }
    signedNameTable.repack()
    while (!isAtEnd) {
      val secName = readString()
      val secEnd = readEnd()
      sectionReader(secName) = new TastyReader(bytes, currentAddr.index, secEnd.index, currentAddr.index)
      goto(secEnd)
    }
  }

  def unpickle[R](sec: SectionUnpickler[R]): Option[R] =
    for (reader <- sectionReader.get(sec.name)) yield
      sec.unpickle(reader, this)

  private[nsc] def bytes: Array[Byte] = reader.bytes
}
