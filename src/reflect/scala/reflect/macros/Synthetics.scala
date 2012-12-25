package scala.reflect
package macros

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  A slice of [[scala.reflect.macros.Context the Scala macros context]] that
 *  exposes functions to introduce synthetic definitions.
 *
 *  @define TOPLEVEL_TREE Top-level tree is a tree that represents a non-inner class or object in one of the currently compiled source files.
 *  Note that top-level isn't equivalent to [[scala.reflect.api.Symbols#SymbolApi.isStatic]],
 *  because static also embraces definitions nested in static objects
 *
 *  @define INTRODUCE_TOP_LEVEL Allowed definitions include classes (represented by `ClassDef` trees), traits (represented
 *  by `ClassDef` trees having the `TRAIT` flag set in `mods`) and objects (represented by `ModuleDef` trees).
 *
 *  The definitions are put into the package with a prototype provided in `packagePrototype`.
 *  Supported prototypes are (see [[PackageSpec]] for more details):
 *    * Strings and names representing a fully-qualified name of the package
 *    * Trees that can work as package ids
 *    * Package or package class symbols
 *
 *  Typical value for a package prototype is a fully-qualified name in a string.
 *  For example, to generate a class available at `foo.bar.Test`, call this method as follows:
 *
 *    introduceTopLevel("foo.bar", ClassDef(<mods>, TypeName("Test"), <tparams>, <template>))
 *
 *  It is possible to add definitions to the empty package by using `nme.EMPTY_PACKAGE_NAME.toString`, but
 *  that's not recommended, since such definitions cannot be seen from outside the empty package.
 *
 *  Only the multi-parameter overload of this method can be used to introduce companions.
 *  If companions are introduced by two different calls, then they will be put into different virtual files, and `scalac`
 *  will show an error about companions being defined in different files. By the way, this also means that there's currently no way
 *  to define a companion for an existing class or module
 */
trait Synthetics {
  self: Context =>

  import universe._

  /** Looks up a top-level definition tree with a given fully-qualified name
   *  (term name for modules, type name for classes). $TOPLEVEL_TREE.
   *  If such a tree does not exist, returns `EmptyTree`.
   */
  def topLevelDef(name: Name): Tree

  /** Returns a reference to a top-level definition tree with a given fully-qualified name
   *  (term name for modules, type name for classes). $TOPLEVEL_TREE.
   *  If such a tree does not exist, returns `EmptyTree`.
   */
  def topLevelRef(name: Name): Tree

  /** Adds a top-level definition to the compiler's symbol table. $INTRODUCE_TOP_LEVEL.
   *
   *  Returns a fully-qualified reference to the introduced definition.
   */
  def introduceTopLevel[T: PackageSpec](packagePrototype: T, definition: ImplDef): RefTree

  /** Adds a list of top-level definitions to the compiler's symbol table. $INTRODUCE_TOP_LEVEL.
   *
   *  Returns a list of fully-qualified references to the introduced definitions.
   */
  def introduceTopLevel[T: PackageSpec](packagePrototype: T, definitions: ImplDef*): List[RefTree]

  /** A factory which can create a package def from a prototype and a list of declarations.
   */
  trait PackageSpec[T] { def mkPackageDef(prototype: T, stats: List[Tree]): PackageDef }

  /** Hosts supported package specs.
   */
  object PackageSpec {
    /** Package def can be created from a fully-qualified name and a list of definitions.
     *  The name is converted into an Ident or a chain of Selects.
     */
    implicit val stringIsPackageSpec = new PackageSpec[String] {
      def mkPackageDef(prototype: String, stats: List[Tree]): PackageDef = self.mkPackageDef(prototype, stats)
    }

    /** Package def can be created from a fully-qualified term name and a list of definitions.
     *  The name is converted into an Ident or a chain of Selects.
     */
    implicit val termNameIsPackageSpec = new PackageSpec[TermName] {
      def mkPackageDef(prototype: TermName, stats: List[Tree]): PackageDef = self.mkPackageDef(prototype, stats)
    }

    /** Package def can be created from a package id tree and a list of definitions.
     *  If the tree is not a valid package id, i.e. is not a term-name ident or a chain of term-name selects,
     *  then the produced PackageDef will fail compilation at some point in the future.
     */
    implicit val refTreeIsPackageSpec = new PackageSpec[RefTree] {
      def mkPackageDef(prototype: RefTree, stats: List[Tree]): PackageDef = self.mkPackageDef(prototype, stats)
    }

    /** Package def can be created from a package/package class symbol and a list of definitions.
     *  If the provided symbol is not a package symbol or a package class symbol, package construction will throw an exception.
     */
    implicit val SymbolIsPackageSpec = new PackageSpec[Symbol] {
      def mkPackageDef(prototype: Symbol, stats: List[Tree]): PackageDef = self.mkPackageDef(prototype, stats)
    }
  }

  protected def mkPackageDef(name: String, stats: List[Tree]): PackageDef
  protected def mkPackageDef(name: TermName, stats: List[Tree]): PackageDef
  protected def mkPackageDef(tree: RefTree, stats: List[Tree]): PackageDef
  protected def mkPackageDef(sym: Symbol, stats: List[Tree]): PackageDef
}
