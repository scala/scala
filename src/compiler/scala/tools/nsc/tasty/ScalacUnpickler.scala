package scala.tools.nsc.tasty

import TastyUnpickler.SectionUnpickler
import scala.reflect.io.AbstractFile
import TastyRefs.NameRef
import scala.util.control.NonFatal
import scala.reflect.internal.SymbolTable

object ScalacUnpickler {

  final class TreeSectionUnpickler[SymbolTable <: reflect.internal.SymbolTable](implicit symbolTable: SymbolTable)
  extends SectionUnpickler[TreeUnpickler { val symbolTable: SymbolTable }]("ASTs") { self =>
    def unpickle(reader: TastyReader, nameTable: TastyNameTable with TastyUniverse): TreeUnpickler { val symbolTable: SymbolTable } =
      new TreeUnpickler(reader, None, None, Seq.empty) {

        assert(nameTable.symbolTable `eq` self.symbolTable, "Unsafe creation of name ref mapper without shared underlying symbol table")

        final val symbolTable: SymbolTable =
          self.symbolTable

        final val nameAtRef: NameRef => symbolTable.TermName =
          nameTable.nameAtRef.asInstanceOf[NameRef => symbolTable.TermName]

        final val signedNameAtRef: NameRef => Either[SigName, symbolTable.TermName] =
          nameTable.signedNameAtRef.asInstanceOf[NameRef => Either[SigName, symbolTable.TermName]]

        final val moduleRefs: NameRef => Boolean =
          nameTable.moduleRefs.asInstanceOf[NameRef => Boolean]

      }
  }
}

/** A class for unpickling Tasty trees and symbols.
 *  @param bytes         the bytearray containing the Tasty file from which we unpickle
 */
abstract class ScalacUnpickler[S <: SymbolTable with Singleton](val symbolTable: S, bytes: Array[Byte]/*, mode: UnpickleMode = UnpickleMode.TopLevel*/) extends TastyUniverse { self =>
  import symbolTable._
  import ScalacUnpickler._

  val unpickler: TastyUnpickler = new TastyUnpickler(bytes)

  private val treeUnpickler = unpickler.unpickle(new TreeSectionUnpickler[self.symbolTable.type]).get

  /** Unpickle symbol table information descending from a class and/or module root
   *  from an array of bytes.
   *  @param classRoot  the top-level class which is unpickled
   *  @param moduleRoot the top-level module which is unpickled
   *  @param filename   filename associated with bytearray, only used for error messages
   */
  def unpickle(classRoot: ClassSymbol, moduleRoot: ModuleSymbol, filename: String): Unit = {
    import treeUnpickler.Contexts._
    try {
      implicit val ctx: Context = {
        new InitialContext(classRoot, mirrorThatLoaded(classRoot), AbstractFile.getFile(filename))
      }
      treeUnpickler.enter(classRoot, moduleRoot)
    } catch {
      case NonFatal(ex) =>
        ex.printStackTrace()
        throw new RuntimeException(s"error reading TASTy from $filename: ${ex.getMessage()}")
    }
  }

}
