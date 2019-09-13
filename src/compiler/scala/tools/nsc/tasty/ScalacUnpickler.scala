package scala.tools.nsc
package tasty

//import TreeUnpickler.UnpickleMode
import TastyUnpickler.SectionUnpickler
import scala.reflect.io.AbstractFile
import scala.tools.nsc.tasty.TastyBuffer.NameRef
import scala.util.control.NonFatal

object ScalacUnpickler {

  /** Exception thrown if classfile is corrupted */
  class BadSignature(msg: String) extends RuntimeException(msg)

  final class TreeSectionUnpickler[SymbolTable <: reflect.internal.SymbolTable](posUnpickler: Option[PositionUnpickler], commentUnpickler: Option[CommentUnpickler] = None)(implicit symbolTable: SymbolTable)
  extends SectionUnpickler[TreeUnpickler { val symbolTable: SymbolTable }](TreePickler.sectionName) { self =>
    def unpickle(reader: TastyReader, nameTable: TASTYNameTable with TASTYUniverse): TreeUnpickler { val symbolTable: SymbolTable } =
      new TreeUnpickler(reader, posUnpickler, commentUnpickler, Seq.empty) {
        final val tastyFlags: TASTYFlags = TASTYFlags.Live
        final val symbolTable: SymbolTable = self.symbolTable
        final val nameAtRef: NameRef => symbolTable.TermName = {
          assert(nameTable.symbolTable eq self.symbolTable, "Unsafe creation of name ref mapper without shared underlying symbol table")
          nameTable.nameAtRef.asInstanceOf[NameRef => symbolTable.TermName]
        }
      }
  }

  final class PositionsSectionUnpickler extends SectionUnpickler[PositionUnpickler]("Positions") {
    def unpickle(reader: TastyReader, nameAtRef: TASTYNameTable with TASTYUniverse): PositionUnpickler =
      new PositionUnpickler(reader, nameAtRef.nameAtRef)
  }

  final class CommentsSectionUnpickler extends SectionUnpickler[CommentUnpickler]("Comments") {
    def unpickle(reader: TastyReader, nameAtRef: TASTYNameTable with TASTYUniverse): CommentUnpickler =
      new CommentUnpickler(reader)
  }
}

/** A class for unpickling Tasty trees and symbols.
 *  @param bytes         the bytearray containing the Tasty file from which we unpickle
 */
abstract class ScalacUnpickler(bytes: Array[Byte]/*, mode: UnpickleMode = UnpickleMode.TopLevel*/) extends TASTYUniverse { self =>
  import symbolTable._
  import ScalacUnpickler._

  val unpickler: TastyUnpickler = new TastyUnpickler(bytes)

  private val posUnpicklerOpt = unpickler.unpickle(new PositionsSectionUnpickler)
  private val commentUnpicklerOpt = unpickler.unpickle(new CommentsSectionUnpickler)
  private val treeUnpickler = unpickler.unpickle(treeSectionUnpickler(posUnpicklerOpt, commentUnpicklerOpt)).get

  /** Unpickle symbol table information descending from a class and/or module root
   *  from an array of bytes.
   *  @param classRoot  the top-level class which is unpickled
   *  @param moduleRoot the top-level module which is unpickled
   *  @param filename   filename associated with bytearray, only used for error messages
   */
  def unpickle(classRoot: ClassSymbol, moduleRoot: ModuleSymbol, filename: String): Unit = {
    import treeUnpickler.Context
    try {
      implicit val ctx: Context = {
        val loadingMirror = mirrorThatLoaded(classRoot)
        new Context.InitialContext(classRoot, loadingMirror, AbstractFile.getFile(filename))
      }
      reporter.echo(NoPosition, s"Unpickling ${ctx.source}. Root owner is ${ctx.owner} with decls ${ctx.owner.info.decls}")
      treeUnpickler.enter(moduleRoot, classRoot)
    } catch {
      case NonFatal(ex) =>
        ex.printStackTrace()
        throw new RuntimeException(s"error reading TASTy from $filename: ${ex.getMessage()}")
    }
  }

  protected def treeSectionUnpickler(posUnpicklerOpt: Option[PositionUnpickler], commentUnpicklerOpt: Option[CommentUnpickler]): TreeSectionUnpickler[self.symbolTable.type] = {
    new TreeSectionUnpickler(posUnpicklerOpt, commentUnpicklerOpt)
  }

//  protected def computeRootTrees(implicit ctx: Context): List[Tree] = treeUnpickler.unpickle(mode)

//  private[this] var ids: Array[String] = null

//  override def mightContain(id: String)(implicit ctx: Context): Boolean = {
//    if (ids == null)
//      ids =
//        unpickler.nameAtRef.contents.toArray.collect {
//          case name: SimpleName => name.toString
//        }.sorted
//    ids.binarySearch(id) >= 0
//  }
}
