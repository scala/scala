package scala.tools.nsc
package tasty

import scala.collection.mutable
import scala.reflect.internal.SymbolTable
import TastyFormat.NameTags._
import TastyBuffer.NameRef

object TastyUnpickler {
  class UnpickleException(msg: String) extends RuntimeException(msg)

  abstract class SectionUnpickler[R](val name: String) {
    def unpickle(reader: TastyReader, nameAtRef: TASTYNameTable with TASTYUniverse): R
  }

  type TermName = SymbolTable#TermName
}

import TastyUnpickler._

class TastyUnpickler(reader: TastyReader)(implicit val symbolTable: SymbolTable) extends TASTYUniverse with TASTYNameTable { self =>
  import symbolTable._
  import reader._

  final class NameTable extends (NameRef => TermName) with TASTYUniverse with TASTYNameTable {
    val symbolTable: self.symbolTable.type = self.symbolTable
    val nameAtRef: NameRef => self.symbolTable.TermName = this
    private[TastyUnpickler] val names = new mutable.ArrayBuffer[TermName]
    def add(name: TermName): mutable.ArrayBuffer[TermName] = names += name
    def apply(ref: NameRef): TermName = names(ref.index)
    def contents: Iterable[TermName] = names
  }

  def this(bytes: Array[Byte])(implicit symbolTable: SymbolTable) = this(new TastyReader(bytes))

  private val sectionReader = new mutable.HashMap[String, TastyReader]

  val nameAtRef: NameTable = new NameTable

  private def readName(): TermName = nameAtRef(readNameRef())
  private def readString(): String = readName().toString

  private def readParamSig(): Signature.ParamSig = {
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
        logTASTY(s"${nameAtRef.names.size}: UTF8 name: $res")
        res
      case tag @ (QUALIFIED | EXPANDED | EXPANDPREFIX) =>
        val sep = tag match {
          case QUALIFIED    => "."
          case EXPANDED     => "$$"
          case EXPANDPREFIX => "$"
        }
        val res = newTermName("" + readName() + sep + readName())
        logTASTY(s"${nameAtRef.names.size}: QUALIFIED | EXPANDED | EXPANDPREFIX name: $res")
        res
//        qualifiedNameKindOfTag(tag)(readName(), readName().asSimpleName)
      case UNIQUE =>
        val separator = readName().toString
        val num = readNat()
        val originals = until(end)(readName())
        val original = if (originals.isEmpty) termNames.EMPTY else originals.head
        val res = newTermName("" + original + separator + num)
        logTASTY(s"${nameAtRef.names.size}: UNIQUE name: $res")
        res
//        uniqueNameKindOfSeparator(separator)(original, num)
      case DEFAULTGETTER | VARIANT =>
        val result = readName()
        val nat = readNat()
        logTASTY(s"${nameAtRef.names.size}: DEFAULTGETTER | VARIANT name: $result[$nat]")
        result // numberedNameKindOfTag(tag)(readName(), readNat())
      case SIGNED =>
        val original = readName()
        val result = readName().toTypeName
        val paramsSig = until(end)(readParamSig())
        val sig = Signature(paramsSig, result)
        logTASTY(s"${nameAtRef.names.size}: SIGNED name: SignedName($original, $sig)")
        original // SignedName(original, sig)
      case _ =>
        val res = readName() // simpleNameKindOfTag(tag)(readName())
        logTASTY(s"${nameAtRef.names.size}: ${TastyFormat.astTagToString(tag)} name: $res")
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
