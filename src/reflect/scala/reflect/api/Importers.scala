package scala.reflect
package api

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 * This trait provides support for importers, a facility to migrate reflection artifacts between universes.
 * ''Note: this trait should typically be used only rarely.''
 *
 *  Reflection artifacts, such as [[scala.reflect.api.Symbols Symbols]] and [[scala.reflect.api.Types Types]],
 *  are contained in [[scala.reflect.api.Universes Universe]]s. Typically all processing happens
 *  within a single `Universe` (e.g. a compile-time macro `Universe` or a runtime reflection `Universe`), but sometimes
 *  there is a need to migrate artifacts from one `Universe` to another. For example, runtime compilation works by
 *  importing runtime reflection trees into a runtime compiler universe, compiling the importees and exporting the
 *  result back.
 *
 *  Reflection artifacts are firmly grounded in their `Universe`s, which is reflected by the fact that types of artifacts
 *  from different universes are not compatible. By using `Importer`s, however, they be imported from one universe
 *  into another. For example, to import `foo.bar.Baz` from the source `Universe` to the target `Universe`,
 *  an importer will first check whether the entire owner chain exists in the target `Universe`.
 *  If it does, then nothing else will be done. Otherwise, the importer will recreate the entire owner chain
 *  and will import the corresponding type signatures into the target `Universe`.
 *
 *  Since importers match `Symbol` tables of the source and the target `Universe`s using plain string names,
 *  it is programmer's responsibility to make sure that imports don't distort semantics, e.g., that
 *  `foo.bar.Baz` in the source `Universe` means the same that `foo.bar.Baz` does in the target `Universe`.
 *
 *  === Example ===
 *
 *  Here's how one might implement a macro that performs compile-time evaluation of its argument
 *  by using a runtime compiler to compile and evaluate a tree that belongs to a compile-time compiler:
 *
 *  {{{
 *  def staticEval[T](x: T) = macro staticEval[T]
 *
 *  def staticEval[T](c: scala.reflect.macros.Context)(x: c.Expr[T]) = {
 *    // creates a runtime reflection universe to host runtime compilation
 *    import scala.reflect.runtime.{universe => ru}
 *    val mirror = ru.runtimeMirror(c.libraryClassLoader)
 *    import scala.tools.reflect.ToolBox
 *    val toolBox = mirror.mkToolBox()
 *
 *    // runtime reflection universe and compile-time macro universe are different
 *    // therefore an importer is needed to bridge them
 *    // currently mkImporter requires a cast to correctly assign the path-dependent types
 *    val importer0 = ru.mkImporter(c.universe)
 *    val importer = importer0.asInstanceOf[ru.Importer { val from: c.universe.type }]
 *
 *    // the created importer is used to turn a compiler tree into a runtime compiler tree
 *    // both compilers use the same classpath, so semantics remains intact
 *    val imported = importer.importTree(tree)
 *
 *    // after the tree is imported, it can be evaluated as usual
 *    val tree = toolBox.resetAllAttrs(imported.duplicate)
 *    val valueOfX = toolBox.eval(imported).asInstanceOf[T]
 *    ...
 *  }
 *  }}}
 *
 * @group ReflectionAPI
 */
trait Importers { self: Universe =>

  /** Creates an importer that moves reflection artifacts between universes.
   *  @group Importers
   */
  def mkImporter(from0: Universe): Importer { val from: from0.type }

  /** The API of importers.
   *  The main source of information about importers is the [[scala.reflect.api.Importers]] page.
   *  @group Importers
   */
  trait Importer {
    /** The source universe of reflection artifacts that will be processed.
     *  The target universe is universe that created this importer with `mkImporter`.
     */
    val from: Universe

    /** An importer that works in reverse direction, namely:
     *  imports reflection artifacts from the current universe to the universe specified in `from`.
     */
    val reverse: from.Importer { val from: self.type }

    /** In the current universe, locates or creates a symbol that corresponds to the provided symbol in the source universe.
     *  If necessary imports the owner chain, companions, type signature, annotations and attachments.
     */
    def importSymbol(sym: from.Symbol): Symbol

    /** In the current universe, locates or creates a type that corresponds to the provided type in the source universe.
     *  If necessary imports the underlying symbols, annotations, scopes and trees.
     */
    def importType(tpe: from.Type): Type

    /** In the current universe, creates a tree that corresponds to the provided tree in the source universe.
     *  If necessary imports the underlying symbols, types and attachments.
     */
    def importTree(tree: from.Tree): Tree

    /** In the current universe, creates a position that corresponds to the provided position in the source universe.
     */
    def importPosition(pos: from.Position): Position
  }
}