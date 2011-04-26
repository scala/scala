/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc
package datatype;


/** A type category for all SQL types that store varying-precision
 * numbers.
 */
@deprecated(DbcIsDeprecated, "2.9.0") abstract class ApproximateNumeric[Type] (
  override val nativeTypeId: DataType.Id
) extends datatype.Numeric[Type](nativeTypeId) {

  def isEquivalent(datatype: DataType) = datatype match {
    case dt: ApproximateNumeric[_] =>
      (nativeTypeId == dt.nativeTypeId &&
       precisionRadix == dt.precisionRadix &&
       precision == dt.precision &&
       signed == dt.signed)
    case _ =>
      false
  }

  def isSubtypeOf (datatype:DataType) = datatype match {
    case dt:ApproximateNumeric[_] =>
      (nativeTypeId == dt.nativeTypeId &&
       precisionRadix == dt.precisionRadix &&
       precision <= dt.precision &&
       signed == dt.signed)
    case _ =>
      false
  }

  /** A SQL-99 compliant string representation of the type.
   *  <h3>Compatibility notice</h3> This method assumes that a real
   *  uses 32 bits and a double 64. This is not defined in the
   *  standard but is usually the case.
   */
  override def sqlString: java.lang.String = Tuple2(precisionRadix,precision) match {
    case Tuple2(2,64) => "REAL"
    case Tuple2(2,128) => "DOUBLE PRECISION"
    case Tuple2(2,p) =>
      throw exception.UnsupportedFeature("SQL-99 does not support an approximate numeric type with a binary defined precision other than 16, 32 and 64 bits");
    case Tuple2(10,p) => "FLOAT (" + p.toString() + ")"
    case Tuple2(pr,_) =>
      throw exception.UnsupportedFeature("SQL-99 does not support the precision of an approximate numeric type to be defined in a radix other than 2 or 10");
  }

}
