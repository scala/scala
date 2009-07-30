package scala.tools.nsc
package dependencies;
import util.SourceFile;
import io.AbstractFile
import symtab.Flags

import collection._

abstract class References extends SubComponent with Files {
  import global._

  val phaseName = "references analysis";

  def newPhase(prev: Phase) = new ReferenceAnalysisPhase(prev)

  /** Top level definitions per source file. */
  val definitions: mutable.Map[AbstractFile, List[Symbol]] =
    new mutable.HashMap[AbstractFile, List[Symbol]] {
      override def default(f : AbstractFile) = Nil
  }

  /** External references used by source file. */
  var references: immutable.Map[AbstractFile, immutable.Set[String]] =
    new immutable.HashMap[AbstractFile, immutable.Set[String]] {
      override def default(f : AbstractFile) = immutable.Set()
    }

  class ReferenceAnalysisPhase(prev: Phase) extends StdPhase(prev) {
    def apply(unit: global.CompilationUnit) {
      val file = unit.source.file
      references += file -> immutable.Set.empty[String]

      val buf = new mutable.ListBuffer[Symbol]

      (new Traverser {
        override def traverse(tree: Tree) {
          if ((tree.symbol ne null)
              && (tree.symbol != NoSymbol)
              && (!tree.symbol.isPackage)
              && (!tree.symbol.hasFlag(Flags.JAVA))
              && ((tree.symbol.sourceFile eq null)
                  || (tree.symbol.sourceFile.path != file.path))) {
            references = references.updated(file, references(file) + tree.symbol.fullNameString)
          }
          tree match {
            case cdef: ClassDef if !cdef.symbol.isModuleClass && !cdef.symbol.hasFlag(Flags.PACKAGE) =>
              buf += cdef.symbol
              super.traverse(tree)

            case _ =>
              super.traverse(tree)
          }
        }
      }).apply(unit.body)

      definitions(unit.source.file) = buf.toList
    }
  }
}

