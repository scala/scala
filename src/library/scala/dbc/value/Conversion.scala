/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.dbc.value;


import java.math._;

object Conversion {

	class Illegal (msg:String) extends Exception(msg);

	implicit def view (value:Value): Byte = {
		if (value.dataType.nativeTypeId == DataType.BYTE) {
			val v = value.asInstanceOf[dbc.value.ExactNumeric[Byte]];
			v.nativeValue
		} else {
			throw new Illegal("Cannot convert value to byte: "+value)
		}
	}

	implicit def view (value:Value): Short = {
		if (value.dataType.nativeTypeId == DataType.BYTE) {
			val v = value.asInstanceOf[dbc.value.ExactNumeric[Byte]];
			v.nativeValue.coerce
		} else if (value.dataType.nativeTypeId == DataType.SHORT) {
			val v = value.asInstanceOf[dbc.value.ExactNumeric[Short]];
			v.nativeValue
		} else {
			throw new Illegal("Cannot convert value to short: "+value)
		}
	}

	implicit def view (value:Value): Int = {
		if (value.dataType.nativeTypeId == DataType.BYTE) {
			val v = value.asInstanceOf[dbc.value.ExactNumeric[Byte]];
			v.nativeValue.coerce
		} else if (value.dataType.nativeTypeId == DataType.SHORT) {
			val v = value.asInstanceOf[dbc.value.ExactNumeric[Short]];
			v.nativeValue.coerce
		} else if (value.dataType.nativeTypeId == DataType.INT) {
			val v = value.asInstanceOf[dbc.value.ExactNumeric[Int]];
			v.nativeValue
		} else {
			throw new Illegal("Cannot convert value to int: "+value)
		}
	}

	implicit def view (value:Value): Long = {
		if (value.dataType.nativeTypeId == DataType.BYTE) {
			val v = value.asInstanceOf[dbc.value.ExactNumeric[Byte]];
			v.nativeValue.coerce
		} else if (value.dataType.nativeTypeId == DataType.SHORT) {
			val v = value.asInstanceOf[dbc.value.ExactNumeric[Short]];
			v.nativeValue.coerce
		} else if (value.dataType.nativeTypeId == DataType.INT) {
			val v = value.asInstanceOf[dbc.value.ExactNumeric[Int]];
			v.nativeValue.coerce
		} else if (value.dataType.nativeTypeId == DataType.LONG) {
			val v = value.asInstanceOf[dbc.value.ExactNumeric[Long]];
			v.nativeValue
		} else {
			throw new Illegal("Cannot convert value to long: "+value)
		}
	}

	implicit def view (value:Value): BigInteger = {
		if (value.dataType.nativeTypeId == DataType.BYTE) {
			val v = value.asInstanceOf[dbc.value.ExactNumeric[Byte]];
			new BigInteger(v.nativeValue.toString(),10)
		} else if (value.dataType.nativeTypeId == DataType.SHORT) {
			val v = value.asInstanceOf[dbc.value.ExactNumeric[Short]];
			new BigInteger(v.nativeValue.toString(),10)
		} else if (value.dataType.nativeTypeId == DataType.INT) {
			val v = value.asInstanceOf[dbc.value.ExactNumeric[Int]];
			new BigInteger(v.nativeValue.toString(),10)
		} else if (value.dataType.nativeTypeId == DataType.LONG) {
			val v = value.asInstanceOf[dbc.value.ExactNumeric[Long]];
			new BigInteger(v.nativeValue.toString(),10)
		} else if (value.dataType.nativeTypeId == DataType.BIG_INTEGER) {
			val v = value.asInstanceOf[dbc.value.ExactNumeric[BigInteger]];
			v.nativeValue
		} else {
			throw new Illegal("Cannot convert value to big integer: "+value)
		}
	}

	implicit def view (value:Value): BigDecimal = {
		if (value.dataType.nativeTypeId == DataType.BYTE) {
			val v = value.asInstanceOf[dbc.value.ExactNumeric[Byte]];
			new BigDecimal(v.nativeValue.toString())
		} else if (value.dataType.nativeTypeId == DataType.SHORT) {
			val v = value.asInstanceOf[dbc.value.ExactNumeric[Short]];
			new BigDecimal(v.nativeValue.toString())
		} else if (value.dataType.nativeTypeId == DataType.INT) {
			val v = value.asInstanceOf[dbc.value.ExactNumeric[Int]];
			new BigDecimal(v.nativeValue.toString())
		} else if (value.dataType.nativeTypeId == DataType.LONG) {
			val v = value.asInstanceOf[dbc.value.ExactNumeric[Long]];
			new BigDecimal(v.nativeValue.toString())
		} else if (value.dataType.nativeTypeId == DataType.BIG_INTEGER) {
			val v = value.asInstanceOf[dbc.value.ExactNumeric[BigInteger]];
			new BigDecimal(v.nativeValue)
		} else if (value.dataType.nativeTypeId == DataType.BIG_DECIMAL) {
			val v = value.asInstanceOf[dbc.value.ExactNumeric[BigDecimal]];
			v.nativeValue
		} else {
			throw new Illegal("Cannot convert value to big decimal: "+value)
		}
	}

	implicit def view (value:Value): Float = {
		if (value.dataType.nativeTypeId == DataType.FLOAT) {
			val v = value.asInstanceOf[dbc.value.ApproximateNumeric[Float]];
			v.nativeValue
		} else {
			throw new Illegal("Cannot convert value to float: "+value)
		}
	}

	implicit def view (value:Value): Double = {
		if (value.dataType.nativeTypeId == DataType.FLOAT) {
			val v = value.asInstanceOf[dbc.value.ApproximateNumeric[Float]];
			v.nativeValue.coerce
		} else if (value.dataType.nativeTypeId == DataType.DOUBLE) {
			val v = value.asInstanceOf[dbc.value.ApproximateNumeric[Double]];
			v.nativeValue
		} else {
			throw new Illegal("Cannot convert value to double: "+value)
		}
	}

	implicit def view (value:Value): scala.Boolean = {
		if (value.dataType.nativeTypeId == DataType.BOOLEAN) {
			val v = value.asInstanceOf[dbc.value.Boolean];
			v.nativeValue
		} else {
			throw new Illegal("Cannot convert value to boolean: "+value)
		}
	}

	implicit def view (value:Value): String = value match {
		case v:dbc.value.Character => v.nativeValue;
		case v:dbc.value.CharacterLargeObject => v.nativeValue;
		case v:dbc.value.CharacterVarying => v.nativeValue;
		case _ => throw new Illegal("Cannot convert value to string")
	}

}
