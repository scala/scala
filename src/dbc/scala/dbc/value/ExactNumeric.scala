/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id:ExactNumeric.scala 6853 2006-03-20 16:58:47 +0100 (Mon, 20 Mar 2006) dubochet $


package scala.dbc
package value;


import java.math.BigInteger;
import java.math.BigDecimal;

abstract class ExactNumeric [Type] extends Value {

  val dataType: datatype.ExactNumeric[Type];

  def sqlString = nativeValue.toString();

}

object ExactNumeric {

  implicit def exactNumericToByte (obj:value.ExactNumeric[Byte]): Byte = obj.nativeValue;
  implicit def exactNumericToShort (obj:value.ExactNumeric[Short]): Short = obj.nativeValue;
  implicit def exactNumericToInt (obj:value.ExactNumeric[Int]): Int = obj.nativeValue;
  implicit def exactNumericToLong (obj:value.ExactNumeric[Long]): Long = obj.nativeValue;
  implicit def exactNumericToBigInteger (obj:value.ExactNumeric[BigInteger]): BigInteger = obj.nativeValue;
  implicit def exactNumericToBigDecimal (obj:value.ExactNumeric[BigDecimal]): BigDecimal = obj.nativeValue;

}
