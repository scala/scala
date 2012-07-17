package scala.reflect
package base

// [Eugene++ to Martin] why was this a member of `scala.reflect`, but not `scala.reflect.base`?

abstract class MirrorOf[U <: base.Universe with Singleton] {
  /** .. */
  val universe: U

  /** .. */
  def RootClass: U#ClassSymbol
  def RootPackage: U#ModuleSymbol
  def EmptyPackageClass: U#ClassSymbol
  def EmptyPackage: U#ModuleSymbol

  /** The symbol corresponding to the globally accessible class with the
   *  given fully qualified name `fullName`.
   *
   *  If the name points to a type alias, it's recursively dealiased and its target is returned.
   *  If you need a symbol that corresponds to the type alias itself, load it directly from the package class:
   *
   *    scala> cm.staticClass("scala.List")
   *    res0: reflect.runtime.universe.ClassSymbol = class List
   *
   *    scala> res0.fullName
   *    res1: String = scala.collection.immutable.List
   *
   *    scala> cm.staticModule("scala")
   *    res2: reflect.runtime.universe.ModuleSymbol = package scala
   *
   *    scala> res2.moduleClass.typeSignature member newTypeName("List")
   *    res3: reflect.runtime.universe.Symbol = type List
   *
   *    scala> res3.fullName
   *    res4: String = scala.List
   */
  def staticClass(fullName: String): U#ClassSymbol

  /** The symbol corresponding to the globally accessible object with the
   *  given fully qualified name `fullName`.
   */
  def staticModule(fullName: String): U#ModuleSymbol
}
