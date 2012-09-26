package scala.reflect.macros
package runtime

import scala.tools.nsc.util.ScalaClassLoader

trait Infrastructure {
  self: Context =>

  val currentRun: Run = universe.currentRun

  val currentClassPath: List[java.net.URL] = universe.classPath.asURLs

  type Run = universe.Run

  object Run extends RunExtractor {
    def unapply(run: Run): Option[(CompilationUnit, List[CompilationUnit])] = Some((run.currentUnit, run.units.toList))
  }

  type CompilationUnit = universe.CompilationUnit

  object CompilationUnit extends CompilationUnitExtractor {
    def unapply(compilationUnit: CompilationUnit): Option[(java.io.File, Array[Char], Tree)] = Some((compilationUnit.source.file.file, compilationUnit.source.content, compilationUnit.body))
  }
}
