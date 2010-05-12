/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc
package value;


import java.math.BigInteger;
import java.math.BigDecimal;

object Factory {

  def create (result: java.sql.ResultSet, index: Int, expectedDataType: DataType): Value = {
    expectedDataType.nativeTypeId match {
      case DataType.OBJECT =>
        new value.Unknown {
          val dataType = expectedDataType.asInstanceOf[datatype.Unknown];
          val nativeValue: AnyRef = result.getObject(index);
        }
      case DataType.STRING => {
        expectedDataType match {
          case t:datatype.Character =>
              new value.Character {
              val dataType = t;
              val nativeValue: String = result.getString(index);
            }
          case t:datatype.CharacterVarying =>
            new value.CharacterVarying {
              val dataType = t;
              val nativeValue: String = result.getString(index);
            }
          case t:datatype.CharacterLargeObject =>
            new value.CharacterLargeObject {
              val dataType = t;
              val nativeValue: String = result.getString(index);
            }
        }
      }
      case DataType.BOOLEAN =>
        new value.Boolean {
          val dataType = expectedDataType.asInstanceOf[datatype.Boolean];
          val nativeValue: scala.Boolean = result.getBoolean(index);
        }
      case DataType.FLOAT  =>
        new value.ApproximateNumeric[Float] {
          val dataType = expectedDataType.asInstanceOf[datatype.ApproximateNumeric[Float]];
          val nativeValue: Float = result.getFloat(index);
        }
      case DataType.DOUBLE =>
        new value.ApproximateNumeric[Double] {
          val dataType = expectedDataType.asInstanceOf[datatype.ApproximateNumeric[Double]];
           val nativeValue: Double = result.getDouble(index);
        }
      case DataType.BYTE =>
        new value.ExactNumeric[Byte] {
          val dataType = expectedDataType.asInstanceOf[datatype.ExactNumeric[Byte]];
          val nativeValue: Byte = result.getByte(index);
        }
      case DataType.SHORT =>
        new value.ExactNumeric[Short] {
          val dataType = expectedDataType.asInstanceOf[datatype.ExactNumeric[Short]];
          val nativeValue: Short = result.getShort(index);
        }
      case DataType.INT =>
        new value.ExactNumeric[Int] {
          val dataType = expectedDataType.asInstanceOf[datatype.ExactNumeric[Int]];
          val nativeValue: Int = result.getInt(index);
        }
      case DataType.LONG =>
        new value.ExactNumeric[Long] {
          val dataType = expectedDataType.asInstanceOf[datatype.ExactNumeric[Long]];
          val nativeValue:Long = result.getLong(index);
        }
      case DataType.BIG_INTEGER =>
        new value.ExactNumeric[BigInteger] {
          val dataType = expectedDataType.asInstanceOf[datatype.ExactNumeric[BigInteger]];
          val nativeValue: BigInteger = result.getBigDecimal(index).unscaledValue();
        }
      case DataType.BIG_DECIMAL =>
        new value.ExactNumeric[BigDecimal] {
          val dataType = expectedDataType.asInstanceOf[datatype.ExactNumeric[BigDecimal]];
          val nativeValue: BigDecimal = result.getBigDecimal(index);
        }

    }
  }

}
