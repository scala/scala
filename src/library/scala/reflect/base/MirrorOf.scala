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
   */
  def staticClass(fullName: String): U#ClassSymbol

  /** The symbol corresponding to the globally accessible object with the
   *  given fully qualified name `fullName`.
   */
  def staticModule(fullName: String): U#ModuleSymbol
}
