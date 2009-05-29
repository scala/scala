package scala.tools.nsc.dependencies;
import util.SourceFile;

import collection._

abstract class References extends SubComponent with Files {
  import global._

  val phaseName = "references analysis";

  def newPhase(prev: Phase) = new ReferenceAnalysisPhase(prev)

  /** Top level definitions per source file. */
  val definitions: mutable.Map[String, List[Symbol]] = new mutable.HashMap

  class ReferenceAnalysisPhase(prev: Phase) extends StdPhase(prev) {
    def apply(unit: global.CompilationUnit) {
      val buf = new mutable.ListBuffer[Symbol]
      (new Traverser {
        override def traverse(tree: Tree) {
          tree match {
            case cdef: ClassDef if !cdef.symbol.isModuleClass =>
              buf += cdef.symbol.cloneSymbol
            case _ =>
              super.traverse(tree)
          }
        }
      }).apply(unit.body)

      definitions(unit.source.file.path) = buf.toList
    }
  }
}

