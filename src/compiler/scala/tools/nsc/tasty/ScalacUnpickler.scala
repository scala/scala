package scala.tools.nsc.tasty

import TastyUnpickler.SectionUnpickler
import scala.reflect.io.AbstractFile
import TastyRefs.NameRef
import scala.util.control.NonFatal
import scala.reflect.internal.SymbolTable
import scala.tools.nsc.tasty.Names.TastyName

object ScalacUnpickler {

  final class TreeSectionUnpickler[SymbolTable <: reflect.internal.SymbolTable](implicit symbolTable: SymbolTable)
  extends SectionUnpickler[TreeUnpickler { val symbolTable: SymbolTable }]("ASTs") { self =>
    def unpickle(reader: TastyReader, nameAtRef: NameRef => TastyName ): TreeUnpickler { val symbolTable: SymbolTable } =
      new TreeUnpickler(reader, nameAtRef, None, None, Seq.empty) {
        final val symbolTable: SymbolTable = self.symbolTable
      }
  }
}

/** A class for unpickling Tasty trees and symbols.
 *  @param bytes         the bytearray containing the Tasty file from which we unpickle
 */
abstract class ScalacUnpickler[S <: SymbolTable with Singleton](val symbolTable: S, bytes: Array[Byte]/*, mode: UnpickleMode = UnpickleMode.TopLevel*/) extends TastyUniverse { self =>
  // import symbolTable._
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
    import treeUnpickler.Contexts.InitialContext
    try {
      implicit val ctx: treeUnpickler.Context = {
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
