package scala.reflect.makro
package runtime

trait Infrastructure {
  self: Context =>

  val forJVM: Boolean = mirror.forJVM

  val forMSIL: Boolean = mirror.forMSIL

  val forInteractive: Boolean = mirror.forInteractive

  val forScaladoc: Boolean = mirror.forScaladoc

  val currentRun: Run = mirror.currentRun

  type Run = mirror.Run

  object Run extends RunExtractor {
    def unapply(run: Run): Option[(CompilationUnit, List[CompilationUnit])] = Some(run.currentUnit, run.units.toList)
  }

  type CompilationUnit = mirror.CompilationUnit

  object CompilationUnit extends CompilationUnitExtractor {
    def unapply(compilationUnit: CompilationUnit): Option[(java.io.File, Array[Char], Tree)] = Some(compilationUnit.source.file.file, compilationUnit.source.content, compilationUnit.body)
  }

  val currentMacro: Symbol = expandee.symbol

  val globalCache: collection.mutable.Map[Any, Any] = mirror.analyzer.globalMacroCache

  val cache: collection.mutable.Map[Any, Any] = mirror.analyzer.perRunMacroCache.getOrElseUpdate(currentMacro, collection.mutable.Map[Any, Any]())
}