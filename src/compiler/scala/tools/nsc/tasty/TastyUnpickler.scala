package scala.tools.nsc
package tasty

import scala.collection.mutable
import scala.reflect.internal.SymbolTable
import TastyFormat.NameTags._
import TastyBuffer.NameRef

object TastyUnpickler {
  class UnpickleException(msg: String) extends RuntimeException(msg)

  abstract class SectionUnpickler[R](val name: String) {
    def unpickle[T <: TermName](reader: TastyReader, nameAtRef: NameTable[T]): R
  }

  type TermName = SymbolTable#TermName

  trait NameTable[TermName <: TastyUnpickler.TermName] extends (NameRef => TermName) {
    private val names = new mutable.ArrayBuffer[TermName]
    def add(name: TermName): mutable.ArrayBuffer[TermName] = names += name
    def apply(ref: NameRef): TermName = names(ref.index)
    def contents: Iterable[TermName] = names
  }

}

import TastyUnpickler._

class TastyUnpickler(reader: TastyReader)(implicit val symbolTable: SymbolTable) extends TASTYUniverse { self =>
  import symbolTable._
  import reader._

  final class NameTableImpl extends TastyUnpickler.NameTable[TermName]

  def this(bytes: Array[Byte])(implicit symbolTable: SymbolTable) = this(new TastyReader(bytes))

  private val sectionReader = new mutable.HashMap[String, TastyReader]

  val nameAtRef: NameTableImpl = new NameTableImpl

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
        newTermName(bytes, start.index, length)
      case QUALIFIED | EXPANDED | EXPANDPREFIX =>
        readName()
        readName()
//        sys.error("qualifiedNameKindOfTag")
//        qualifiedNameKindOfTag(tag)(readName(), readName().asSimpleName)
      case UNIQUE =>
        val separator = readName().toString
        val num = readNat()
        val originals = until(end)(readName())
        val original = if (originals.isEmpty) termNames.EMPTY else originals.head
        sys.error("uniqueNameKindOfSeparator")
//        uniqueNameKindOfSeparator(separator)(original, num)
      case DEFAULTGETTER | VARIANT =>
        sys.error("numberedNameKindOfTag")
//        numberedNameKindOfTag(tag)(readName(), readNat())
      case SIGNED =>
        val original = readName()
        val result = readName().toTypeName
        val paramsSig = until(end)(readParamSig())
        val sig = Signature(paramsSig, result)
//        sys.error("SignedName")
        original
//        SignedName(original, sig)
      case _ =>
//        sys.error("simpleNameKindOfTag")
        readName()
//        simpleNameKindOfTag(tag)(readName())
    }
    assert(currentAddr == end, s"bad name $result $start $currentAddr $end")
    result
  }

  new TastyHeaderUnpickler(reader).readHeader()

  locally {
    until(readEnd()) { nameAtRef.add(readNameContents()) }
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
