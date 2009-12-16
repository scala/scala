/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.dbc;


/** An ISO-9075:2003 (SQL) data type. Mappings between SQL types and
 *  database specific types should be provided by the database driver.
 */
abstract class DataType {

  /** Tests whether this datatype is equivalent to another. Usually, two
   *  types are defined as equivalent if they are equal. Two types can be
   *  equivalent without being equal if values of those types will be
   *  encoded in the same native Scala type.
   */
  def isEquivalent(datatype: DataType): Boolean;

  /** Tests whether this datatype is equivalent or a subtype of another
   *  datatype. Type <code>A</code> is said to be subtype of type
   *  <code>B</code> if any value of type <code>A</code> can be
   *  represented as a value of type <code>B</code>.
   */
  def isSubtypeOf(datatype: DataType): Boolean;

  /** The native Scala type in which values of this SQL type will be
   *  encoded.
   */
  type NativeType <: Any;

  /** The native Scala type in which values of this SQL type will be
   *  encoded. This must point to the same type as <code>NativeType</code>.
   */
  def nativeTypeId: DataType.Id;

  /** Whether the value can take the null value, None when this property is
   *  unknown.
   */
  def nullable: Option[Boolean] = None;

  /** The SQL name of the type */
  def sqlString: String = "UNDEFINED DATA TYPE"

}

object DataType {

  type Id = Int;

  val OBJECT     : Id = 10;
  val BOOLEAN    : Id = 20;
  val BYTE       : Id = 30;
  val SHORT      : Id = 31;
  val INT        : Id = 32;
  val LONG       : Id = 33;
  val BIG_INTEGER: Id = 34;
  val BIG_DECIMAL: Id = 35;
  val FLOAT      : Id = 40;
  val DOUBLE     : Id = 41;
  val STRING     : Id = 50;

}
