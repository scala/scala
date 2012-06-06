package scala.reflect
package makro

trait Infrastructure {
  self: Context =>

  /** Determines whether the compiler expanding a macro targets JVM.
   */
  val forJVM: Boolean

  /** Determines whether the compiler expanding a macro targets CLR.
   */
  val forMSIL: Boolean

  /** Determines whether the compiler expanding a macro is a presentation compiler.
   */
  val forInteractive: Boolean

  /** Determines whether the compiler expanding a macro is a Scaladoc compiler.
   */
  val forScaladoc: Boolean

  /** Exposes current compilation run.
   */
  val currentRun: Run

  /** Exposes library classpath.
   */
  val libraryClassPath: List[java.net.URL]

  /** Exposes a classloader that corresponds to the library classpath.
   *
   *  With this classloader you can perform on-the-fly evaluation of macro arguments.
   *  For example, consider this code snippet:
   *
   *    def staticEval[T](x: T) = macro staticEval[T]
   *
   *    def staticEval[T: c.TypeTag](c: Context)(x: c.Expr[T]) = {
   *      import scala.reflect.runtime.{universe => ru}
   *      val mirror = ru.runtimeMirror(c.libraryClassLoader)
   *      import scala.tools.reflect.ToolBox
   *      val toolBox = mirror.mkToolBox()
   *      val importer = ru.mkImporter(c.universe).asInstanceOf[ru.Importer { val from: c.universe.type }]
   *      val tree = c.resetAllAttrs(x.tree.duplicate)
   *      val imported = importer.importTree(tree)
   *      val valueOfX = toolBox.runExpr(imported).asInstanceOf[T]
   *      ...
   *    }
   *
   *  // [Eugene++] using this guy will tremendously slow down the compilation
   *  // https://twitter.com/xeno_by/status/201248317831774208
   *  // todo. we need to address this somehow
   */
  def libraryClassLoader: ClassLoader

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

  /** Returns a macro definition which triggered this macro expansion.
   */
  val currentMacro: Symbol

  // todo. redo caches as discussed on Reflecting Meeting 2012/03/29
  // https://docs.google.com/document/d/1oUZGQpdt2qwioTlJcSt8ZFQwVLTvpxn8xa67P8OGVpU/edit

  /** A cache shared by all invocations of all macros across all compilation runs.
   *
   *  Needs to be used with extreme care, since memory leaks here will swiftly crash the presentation compiler.
   *  For example, Scala IDE typically launches a compiler run on every edit action so there might be hundreds of runs per minute.
   */
  val globalCache: collection.mutable.Map[Any, Any]

  /** A cache shared by all invocations of the same macro within a single compilation run.
   *
   *  This cache is cleared automatically after a compilation run is completed or abandoned.
   *  It is also specific to a particular macro definition.
   *
   *  To share data between different macros and/or different compilation runs, use ``globalCache''.
   */
  val cache: collection.mutable.Map[Any, Any]
}
