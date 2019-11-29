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

  object Unpickler {
    def tasty[Tasty <: TastyUniverse](implicit tasty: Tasty) = tasty
  }

  final implicit class Unpickler[Tasty <: TastyUniverse](private val tasty: Tasty) extends AnyVal {
    import tasty._

    /** Unpickle symbol table information descending from a class and/or module root
     *  from an array of bytes.
     *  @param classRoot  the top-level class which is unpickled
     *  @param moduleRoot the top-level module which is unpickled
     *  @param filename   filename associated with bytearray, only used for error messages
     */
    def unpickle(bytes: Array[Byte]/*, mode: UnpickleMode = UnpickleMode.TopLevel*/, classRoot: ClassSymbol, moduleRoot: ModuleSymbol, filename: String): Unit = {
      import Contexts._
      implicit val thisTasty: tasty.type = tasty
      try {
        val unpickler: TastyUnpickler = new TastyUnpickler(bytes)

        val treeUnpickler = unpickler.unpickle[TreeUnpickler[tasty.type]](new TreeSectionUnpickler()(tasty)).get

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
}
