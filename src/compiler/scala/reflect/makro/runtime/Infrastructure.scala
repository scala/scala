package scala.reflect.makro
package runtime

import scala.tools.nsc.util.ScalaClassLoader

trait Infrastructure {
  self: Context =>

  val forJVM: Boolean = universe.forJVM

  val forMSIL: Boolean = universe.forMSIL

  val forInteractive: Boolean = universe.forInteractive

  val forScaladoc: Boolean = universe.forScaladoc

  val currentRun: Run = universe.currentRun

  val libraryClassPath: List[java.net.URL] = universe.classPath.asURLs

  lazy val libraryClassLoader: ClassLoader = {
    val classpath = libraryClassPath
    var loader: ClassLoader = ScalaClassLoader.fromURLs(classpath, self.getClass.getClassLoader)

    // [Eugene] a heuristic to detect REPL
    if (universe.settings.exposeEmptyPackage.value) {
      import scala.tools.nsc.interpreter._
      val virtualDirectory = universe.settings.outputDirs.getSingleOutput.get
      loader = new AbstractFileClassLoader(virtualDirectory, loader) {}
    }

    loader
  }

  type Run = universe.Run

  object Run extends RunExtractor {
    def unapply(run: Run): Option[(CompilationUnit, List[CompilationUnit])] = Some(run.currentUnit, run.units.toList)
  }

  type CompilationUnit = universe.CompilationUnit

  object CompilationUnit extends CompilationUnitExtractor {
    def unapply(compilationUnit: CompilationUnit): Option[(java.io.File, Array[Char], Tree)] = Some(compilationUnit.source.file.file, compilationUnit.source.content, compilationUnit.body)
  }

  val currentMacro: Symbol = expandee.symbol

  val globalCache: collection.mutable.Map[Any, Any] = universe.analyzer.globalMacroCache

  val cache: collection.mutable.Map[Any, Any] = universe.analyzer.perRunMacroCache.getOrElseUpdate(currentMacro, collection.mutable.Map[Any, Any]())
}