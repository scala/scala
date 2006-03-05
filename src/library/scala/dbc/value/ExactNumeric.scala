/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.dbc.value;


import java.math.BigInteger;
import java.math.BigDecimal;

abstract class ExactNumeric [Type] extends Value {

  val dataType: datatype.ExactNumeric[Type];

  def sqlString = nativeValue.toString();

}

object ExactNumeric {

	def view (obj:value.ExactNumeric[Byte]): Byte = obj.nativeValue;
  def view (obj:value.ExactNumeric[Short]): Short = obj.nativeValue;
  def view (obj:value.ExactNumeric[Int]): Int = obj.nativeValue;
  def view (obj:value.ExactNumeric[Long]): Long = obj.nativeValue;
  def view (obj:value.ExactNumeric[BigInteger]): BigInteger = obj.nativeValue;
  def view (obj:value.ExactNumeric[BigDecimal]): BigDecimal = obj.nativeValue;

}
