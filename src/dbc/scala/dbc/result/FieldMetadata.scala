/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc
package result


/** The class <code>FieldMetadata</cocde> provides informations attached to
 *  a field about its content and its relationship to the originating database.
 */
@deprecated(DbcIsDeprecated, "2.9.0") abstract class FieldMetadata {

  /** The name of the field. */
  def name: String

  /** The index of the field in the tuple. */
  def index: Int

  /** The expected type of the field. This information is used for automatic
   *  transformation of the field value into a usable type.
   */
  def datatype: DataType

  /** The name of the catalog in the database from which the field originates */
  def catalog: String

  /** The name of the schema in the database from which the field originates */
  def schema: String

  /** The name of the table in the database from which the field originates */
  def table: String

}
