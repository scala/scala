/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc
package datatype;


/** A type category for all SQL types that store constant-precision
  * numbers.
  */
abstract class ExactNumeric[Type](
  override val nativeTypeId: DataType.Id
) extends datatype.Numeric[Type](nativeTypeId) {

  def isEquivalent(datatype: DataType) = datatype match {
    case dt: ExactNumeric[_] =>
      (nativeTypeId == dt.nativeTypeId &&
       precisionRadix == dt.precisionRadix &&
       precision == dt.precision &&
       scale == dt.scale &&
       signed == dt.signed)
    case _ =>
      false
  }

  def isSubtypeOf(datatype: DataType) = datatype match {
    case dt: ExactNumeric[_] =>
      (nativeTypeId == dt.nativeTypeId &&
       precisionRadix == dt.precisionRadix &&
       precision <= dt.precision &&
       scale <= dt.scale &&
       signed == dt.signed)
    case _ =>
      false
  }

  /** The number of digits used after the decimal point. */
  def scale: Int;

  /** A SQL-99 compliant string representation of the type.
   * <h3>Compatibility notice</h3> This method assumes that an integer
   * uses 32 bits, a small 16 and a big 64. This is not defined in the
   * standard but is usually the case.
   */
  override def sqlString: java.lang.String = Tuple3(precisionRadix,precision,scale) match {
    case Tuple3(2,16,0) => "SMALLINT"
    case Tuple3(2,32,0) => "INTEGER"
    case Tuple3(2,64,0) => "BIGINT"
    case Tuple3(2,java.lang.Integer.MAX_VALUE,0) => "BIGINT"
    case Tuple3(2,p,s) =>
      throw exception.UnsupportedFeature("SQL-99 does not support an exact numeric type with a binary defined precision other than 16, 32 and 64 bits");
    case Tuple3(10,p,0) => "NUMERIC (" + p.toString() + ")"
    case Tuple3(10,p,s) => "NUMERIC (" + p.toString() + ", " + s.toString() + ")"
    case Tuple3(pr,_,_) =>
      throw exception.UnsupportedFeature("SQL-99 does not support the precision of an exact numeric type to be defined in a radix other than 2 or 10");
  }

}
