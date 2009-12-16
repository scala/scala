/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.dbc
package syntax;


import java.math.BigDecimal;
import java.math.BigInteger;

object DataTypeUtil {

  final val java_lang_Integer_SIZE = 32;
  final val java_lang_Long_SIZE    = 64;

  def boolean = new datatype.Boolean;
  def tinyint = new datatype.ExactNumeric[Byte](dbc.DataType.BYTE) {
    val precisionRadix = 2;
    val precision = 8;
    val signed = true;
    val scale = 0;
  }
  def smallint = new datatype.ExactNumeric[Short](dbc.DataType.SHORT) {
    val precisionRadix = 2;
    val precision = 16;
    val signed = true;
    val scale = 0;
  }
  def integer = new datatype.ExactNumeric[Int](dbc.DataType.INT) {
    val precisionRadix = 2;
    val precision = 32;
    val signed = true;
    val scale = 0;
  }
  def bigint = new datatype.ExactNumeric[Long](dbc.DataType.LONG) {
    val precisionRadix = 2;
    val precision = 64;
    val signed = true;
    val scale = 0;
  }
  def numeric (_precision:Int): DataType = numeric(_precision,0);
  def numeric (_precision:Int, _scale:Int): DataType =
    Pair(datatype.Factory.bytePrecision(_precision,true,true),_scale == 0) match {
      case Pair(bp,true) if (bp <= java_lang_Integer_SIZE) =>
        new datatype.ExactNumeric[Int](DataType.INT) {
          val precisionRadix = 10;
          val precision = _precision;
          val signed = true;
          val scale = 0;
        }
      case Pair(bp,true) if (bp <= java_lang_Long_SIZE) =>
        new datatype.ExactNumeric[Long](DataType.LONG) {
          val precisionRadix = 10;
          val precision = _precision;
          val signed = true;
          val scale = 0;
        }
      case Pair(_,true) =>
        new datatype.ExactNumeric[BigInteger](DataType.BIG_INTEGER) {
          val precisionRadix = 10;
          val precision = _precision;
          val signed = true;
          val scale = 0;
        }
      case Pair(_,false) =>
        new datatype.ExactNumeric[BigDecimal](DataType.BIG_DECIMAL) {
          val precisionRadix = 10;
          val precision = _precision;
          val signed = true;
          val scale = _scale;
        }
    }
  def real = new datatype.ApproximateNumeric[Float](DataType.FLOAT) {
    val precisionRadix = 2;
    val precision = 64;
    val signed = true;
  }
  def doublePrecision = new datatype.ApproximateNumeric[Double](DataType.DOUBLE) {
    val precisionRadix = 2;
    val precision = 128;
    val signed = true;
  }
  def character (_length: Int) = new datatype.Character {
    val length = _length;
  }
  def characterVarying (_length: Int) = new datatype.CharacterVarying {
    def length = _length;
  }
  def characterLargeObject = new datatype.CharacterLargeObject;

}
