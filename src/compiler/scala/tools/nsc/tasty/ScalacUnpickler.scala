package scala.tools.nsc.tasty

import TastyUnpickler.SectionUnpickler
import TastyRefs.NameRef
import Names.TastyName

import scala.reflect.io.AbstractFile
import scala.util.control.NonFatal

object ScalacUnpickler {

  final class TreeSectionUnpickler[Tasty <: TastyUniverse](implicit tasty: Tasty)
  extends SectionUnpickler[TreeUnpickler[Tasty]]("ASTs") { self =>
    def unpickle(reader: TastyReader, nameAtRef: NameRef => TastyName ): TreeUnpickler[Tasty] =
      new TreeUnpickler(reader, nameAtRef, None, None, Seq.empty)
  }
}

/** A class for unpickling Tasty trees and symbols.
 *  @param bytes         the bytearray containing the Tasty file from which we unpickle
 */
class ScalacUnpickler[Tasty <: TastyUniverse](bytes: Array[Byte]/*, mode: UnpickleMode = UnpickleMode.TopLevel*/)(implicit val tasty: Tasty) { self =>
  import tasty._
  import ScalacUnpickler._

  val unpickler: TastyUnpickler = new TastyUnpickler(bytes)

  private val treeUnpickler = unpickler.unpickle[TreeUnpickler[tasty.type]](new TreeSectionUnpickler()(tasty)).get

  /** Unpickle symbol table information descending from a class and/or module root
   *  from an array of bytes.
   *  @param classRoot  the top-level class which is unpickled
   *  @param moduleRoot the top-level module which is unpickled
   *  @param filename   filename associated with bytearray, only used for error messages
   */
  def unpickle(classRoot: ClassSymbol, moduleRoot: ModuleSymbol, filename: String): Unit = {
    import Contexts.InitialContext
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
