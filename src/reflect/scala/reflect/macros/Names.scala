package scala
package reflect
package macros

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  A slice of [[scala.reflect.macros.Context the Scala macros context]] that
 *  provides functions that generate unique names.
 */
trait Names {
  self: Context =>

  /** Creates a unique string. */
  @deprecated("Use freshName instead", "2.11.0")
  def fresh(): String

  /** Creates a unique string having a given prefix. */
  @deprecated("Use freshName instead", "2.11.0")
  def fresh(name: String): String

  /** Creates a unique name having a given name as a prefix and
   *  having the same flavor (term name or type name) as the given name.
   */
  @deprecated("Use freshName instead", "2.11.0")
  def fresh[NameType <: Name](name: NameType): NameType

  /** Creates a unique string. */
  def freshName(): String

  /** Creates a unique string having a given prefix. */
  def freshName(name: String): String

  /** Creates a unique name having a given name as a prefix and
   *  having the same flavor (term name or type name) as the given name.
   */
  def freshName[NameType <: Name](name: NameType): NameType
}
