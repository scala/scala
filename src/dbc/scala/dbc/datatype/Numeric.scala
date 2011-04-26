/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc
package datatype;


/** A type category for all SQL types that store numbers. */
@deprecated(DbcIsDeprecated, "2.9.0") abstract class Numeric[Type](_nativeTypeId: DataType.Id) extends DataType {

  type NativeType = Type;
  val nativeTypeId = _nativeTypeId;

  /** The radix in which the precision (and scale when appliable) is defined.
   *  ISO-9075 only allows 2 and 10 for this value.
   */
  def precisionRadix: Int;

  /** The number of significant digits for that number. */
  def precision: Int;

  /** Whether the number is signed or not. */
  def signed: scala.Boolean;

}
