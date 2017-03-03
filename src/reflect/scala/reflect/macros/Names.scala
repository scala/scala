package scala
package reflect
package macros

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  A slice of [[scala.reflect.macros.blackbox.Context the Scala macros context]] that
 *  provides functions that generate fresh names.
 *
 *  In the current implementation, fresh names are more or less unique in the sense that
 *  within the same compilation run they are guaranteed not to clash with:
 *    1) Results of past and future invocations of functions of `freshName` family
 *    2) User-defined or macro-generated names that don't contain dollar symbols
 *    3) Macro-generated names that are created by concatenating names from the first, second and third categories
 *
 *  Uniqueness of fresh names across compilation runs is not guaranteed, but that's something
 *  that we would like to improve upon in future releases. See [[https://issues.scala-lang.org/browse/SI-6879]] for more information.
 *
 *  @define freshNameNoParams
 *  Creates a string that represents a more or less unique name.
 *  Consult [[scala.reflect.macros.Names]] for more information on uniqueness of such names.
 *
 *  @define freshNameStringParam
 *  Creates a string that represents a more or less unique name having a given prefix.
 *  Consult [[scala.reflect.macros.Names]] for more information on uniqueness of such names.
 *
 *  @define freshNameNameParam
 *  Creates a more or less unique name having a given name as a prefix and
 *  having the same flavor (term name or type name) as the given name.
 *  Consult [[scala.reflect.macros.Names]] for more information on uniqueness of such names.
 */
trait Names {
  self: blackbox.Context =>

  /** $freshNameNoParams */
  @deprecated("use freshName instead", "2.11.0")
  def fresh(): String

  /** $freshNameStringParam */
  @deprecated("use freshName instead", "2.11.0")
  def fresh(name: String): String

  /** $freshNameNameParam */
  @deprecated("use freshName instead", "2.11.0")
  def fresh[NameType <: Name](name: NameType): NameType

  /** $freshNameNoParams */
  def freshName(): String

  /** $freshNameStringParam */
  def freshName(name: String): String

  /** $freshNameNameParam */
  def freshName[NameType <: Name](name: NameType): NameType
}
