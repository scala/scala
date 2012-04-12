package scala.reflect
package api

trait ClassLoaders { self: Universe =>

  /** The symbol corresponding to the globally accessible class with the
   *  given fully qualified name `fullName`.
   */
  def staticClass(fullName: String): Symbol

  /** The symbol corresponding to the globally accessible object with the
   *  given fully qualified name `fullName`.
   */
  def staticModule(fullName: String): Symbol

}