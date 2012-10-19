package scala.reflect
package macros

/** A slice of [[scala.reflect.macros.Context the Scala macros context]] that
 *  provides functions that generate unique names.
 */
trait Names {
  self: Context =>

  /** Creates a unique string. */
  def fresh(): String

  /** Creates a unique string having a given prefix. */
  def fresh(name: String): String

  /** Creates a unique name having a given name as a prefix and
   *  having the same flavor (term name or type name) as the given name.
   */
  def fresh[NameType <: Name](name: NameType): NameType
}
