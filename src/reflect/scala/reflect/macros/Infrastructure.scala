package scala.reflect
package macros

trait Infrastructure {
  self: Context =>

  /** Exposes current compilation run.
   */
  val currentRun: Run

  /** Exposes current classpath.
   */
  val currentClassPath: List[java.net.URL]

  /** As seen by macro API, compilation run is an opaque type that can be deconstructed into:
   *    1) Current compilation unit
   *    2) List of all compilation units that comprise the run
   */
  type Run

  val Run: RunExtractor

  abstract class RunExtractor {
    def unapply(run: Run): Option[(CompilationUnit, List[CompilationUnit])]
  }

  /** As seen by macro API, compilation unit is an opaque type that can be deconstructed into:
   *    1) File that corresponds to the unit (if not applicable, null)
   *    2) Content of the file (if not applicable, empty array)
   *    3) Body, i.e. the AST that represents the compilation unit
   */
  type CompilationUnit

  val CompilationUnit: CompilationUnitExtractor

  abstract class CompilationUnitExtractor {
    def unapply(compilationUnit: CompilationUnit): Option[(java.io.File, Array[Char], Tree)]
  }
}
