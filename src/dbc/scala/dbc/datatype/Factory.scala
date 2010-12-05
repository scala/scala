/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc
package datatype;


import java.sql.Types._;
import java.math.BigInteger;
import java.math.BigDecimal;

object Factory {

  final val java_lang_Integer_SIZE = 32;
  final val java_lang_Long_SIZE    = 64;

  /** Returns a nullable property formatted as a boolean option */
  def isNullable (metadata:java.sql.ResultSetMetaData, index:Int): Option[scala.Boolean] =
    metadata.isNullable(index) match {
      case java.sql.ResultSetMetaData.columnNoNulls => Some(false);
      case java.sql.ResultSetMetaData.columnNullable => Some(true);
      case java.sql.ResultSetMetaData.columnNullableUnknown => None;
    }

  /** Returns the binary precision for an integer field. This should only be
   * used to find precision for integer numbers. It assumes that
   * bytes cannot be used partially (result % 8 = 0). */
  def bytePrecision (precision:Int, signed:scala.Boolean, safe:scala.Boolean): Int = {
    val decimalPrecision = precision + (if (safe) 1 else 0);
    Pair(signed,decimalPrecision) match {
      case Pair(_,0) => java.lang.Integer.MAX_VALUE // That's a bit of a hack.
      case Pair(_,dp) if (dp <= 3) => 8
      case Pair(_,dp) if (dp <= 5) => 16
      case Pair(true,dp) if (dp <= 7) => 24
      case Pair(false,dp) if (dp <= 8) => 24
      case Pair(_,dp) if (dp <= 10) => 32
      case Pair(true,dp) if (dp <= 12) => 40
      case Pair(false,dp) if (dp <= 13) => 40
      case Pair(_,dp) if (dp <= 15) => 48
      case Pair(_,dp) if (dp <= 17) => 56
      case Pair(true,dp) if (dp <= 19) => 64
      case Pair(false,dp) if (dp <= 20) => 64
      case Pair(_,dp) if (dp <= 22) => 72
      case Pair(true,dp) if (dp <= 24) => 80
      case Pair(false,dp) if (dp <= 25) => 80
      case Pair(_,dp) if (dp <= 27) => 88
      case Pair(_,dp) if (dp <= 29) => 96
      case Pair(_,dp) if (dp <= 32) => 104
      case Pair(_,dp) if (dp <= 34) => 112
      case Pair(true,dp) if (dp <= 36) => 120
      case Pair(false,dp) if (dp <= 37) => 120
      case Pair(_,dp) if (dp <= 39) => 128
      case _ => java.lang.Integer.MAX_VALUE
    }
  }

  def create (metadata:java.sql.ResultSetMetaData, index:Int): DataType = {
    metadata.getColumnType(index) match {
      /* Boolean data types. */
      case BOOLEAN => new datatype.Boolean {
        override val nullable = isNullable(metadata,index);
      }
      case BIT => new datatype.Boolean {
        override val nullable = isNullable(metadata,index);
      }
      /* Fixed precision numeric data types. */
      case DECIMAL => {
        Pair(bytePrecision(metadata.getPrecision(index),metadata.isSigned(index),true),metadata.getScale(index) == 0) match {
          case Pair(bp,true) if (bp <= java_lang_Integer_SIZE) =>
            new datatype.ExactNumeric[Int](DataType.INT) {
              override val nullable = isNullable(metadata,index);
              val precisionRadix = 10;
              val precision = metadata.getPrecision(index);
              val signed = metadata.isSigned(index);
              val scale = metadata.getScale(index);
            }
          case Pair(bp,true) if (bp <= java_lang_Long_SIZE) =>
            new datatype.ExactNumeric[Long](DataType.LONG) {
              override val nullable = isNullable(metadata,index);
              val precisionRadix = 10;
              val precision = metadata.getPrecision(index);
              val signed = metadata.isSigned(index);
              val scale = metadata.getScale(index);
            }
          case Pair(_,true) =>
            new datatype.ExactNumeric[BigInteger](DataType.BIG_INTEGER) {
              override val nullable = isNullable(metadata,index);
              val precisionRadix = 10;
              val precision = metadata.getPrecision(index);
              val signed = metadata.isSigned(index);
              val scale = metadata.getScale(index);
            }
          case Pair(_,false) =>
            new datatype.ExactNumeric[BigDecimal](DataType.BIG_DECIMAL) {
              override val nullable = isNullable(metadata,index);
              val precisionRadix = 10;
              val precision = metadata.getPrecision(index);
              val signed = metadata.isSigned(index);
              val scale = metadata.getScale(index);
            }
        }
      }
      case NUMERIC => {
        Pair(bytePrecision(metadata.getPrecision(index),metadata.isSigned(index),true),metadata.getScale(index) == 0) match {
          case Pair(bp,true) if (bp <= java_lang_Integer_SIZE) =>
            new datatype.ExactNumeric[Int](DataType.INT) {
              override val nullable = isNullable(metadata,index);
              val precisionRadix = 10;
              val precision = metadata.getPrecision(index);
              val signed = metadata.isSigned(index);
              val scale = metadata.getScale(index);
            }
          case Pair(bp,true) if (bp <= java_lang_Long_SIZE) =>
            new datatype.ExactNumeric[Long](DataType.LONG) {
              override val nullable = isNullable(metadata,index);
              val precisionRadix = 10;
              val precision = metadata.getPrecision(index);
              val signed = metadata.isSigned(index);
              val scale = metadata.getScale(index);
            }
          case Pair(_,true) =>
            new datatype.ExactNumeric[BigInteger](DataType.BIG_INTEGER) {
              override val nullable = isNullable(metadata,index);
              val precisionRadix = 10;
              val precision = metadata.getPrecision(index);
              val signed = metadata.isSigned(index);
              val scale = metadata.getScale(index);
            }
          case Pair(_,false) =>
            new datatype.ExactNumeric[BigDecimal](DataType.BIG_DECIMAL) {
              override val nullable = isNullable(metadata,index);
              val precisionRadix = 10;
              val precision = metadata.getPrecision(index);
              val signed = metadata.isSigned(index);
              val scale = metadata.getScale(index);
            }
        }
      }
      /* Fixed precision integer data types. */
      case BIGINT =>
        new datatype.ExactNumeric[Long](DataType.LONG) {
          override val nullable = isNullable(metadata,index);
          val precisionRadix = 2;
          val precision = 64;
          val signed = metadata.isSigned(index);
          val scale = 0;
        }
      case INTEGER =>
        new datatype.ExactNumeric[Int](DataType.INT) {
          override val nullable = isNullable(metadata,index);
          val precisionRadix = 2;
          val precision = 32;
          val signed = metadata.isSigned(index);
          val scale = 0;
        }
      case SMALLINT =>
        new datatype.ExactNumeric[Short](DataType.SHORT) {
          override val nullable = isNullable(metadata,index);
          val precisionRadix = 2;
          val precision = 16;
          val signed = metadata.isSigned(index);
          val scale = 0;
        }
      case TINYINT =>
        new datatype.ExactNumeric[Byte](DataType.BYTE) {
          override val nullable = isNullable(metadata,index);
          val precisionRadix = 2;
          val precision = 8;
          val signed = metadata.isSigned(index);
          val scale = 0;
        }
      /* Floating point numeric data types. */
      case REAL =>
        new datatype.ApproximateNumeric[Float](DataType.FLOAT) {
          override val nullable = isNullable(metadata,index);
          val precisionRadix = 2;
          val precision = 64;
          val signed = metadata.isSigned(index);
        }
      case DOUBLE =>
        new datatype.ApproximateNumeric[Double](DataType.DOUBLE) {
          override val nullable = isNullable(metadata,index);
          val precisionRadix = 2;
          val precision = 128;
          val signed = metadata.isSigned(index);
        }
      case FLOAT =>
        new datatype.ApproximateNumeric[Double](DataType.DOUBLE) {
          override val nullable = isNullable(metadata,index);
          val precisionRadix = 2;
          val precision = 128;
          val signed = metadata.isSigned(index);
        }
      /* Character string data types. */
      case CHAR => new datatype.Character {
        override val nullable = isNullable(metadata,index);
        val length = metadata.getColumnDisplaySize(index);
      }
      case CLOB => new datatype.CharacterLargeObject {
        override val nullable = isNullable(metadata,index);
      }
      case LONGVARCHAR => {
        if (metadata.getColumnDisplaySize(index) >= 0)
          new datatype.CharacterVarying {
            override val nullable = isNullable(metadata,index);
            def length = metadata.getColumnDisplaySize(index);
          }
        else // A PostgreSQL Hack
          new datatype.CharacterLargeObject {
            override val nullable = isNullable(metadata,index);
          }
      }
      case VARCHAR => {
        if (metadata.getColumnDisplaySize(index) >= 0)
          new datatype.CharacterVarying {
            override val nullable = isNullable(metadata,index);
            def length = metadata.getColumnDisplaySize(index);
          }
        else // A PostgreSQL Hack
          new datatype.CharacterLargeObject {
            override val nullable = isNullable(metadata,index);
          }
      }
      /* Undefined cases. */
      case OTHER => new datatype.Unknown {
        override val nullable = isNullable(metadata, index);
      }
      /* Unsupported data types. */
      case REF | ARRAY | STRUCT =>
        system.error ("I don't support composite data types yet.");
      case DATALINK | DISTINCT | JAVA_OBJECT | NULL =>
        system.error ("I won't support strange data types.");
      /* Unsupported binary string data types. */
      case BINARY | BLOB | LONGVARBINARY | VARBINARY =>
        system.error ("I don't support binary string data types yet.");
      /* Unsupported date and time data types. */
      case DATE | TIME | TIMESTAMP =>
        system.error ("I don't support date and time data types yet.");
      /* Default case */
      case x => system.error ("I don't know about this ("+metadata.getColumnTypeName(index)+") JDBC type.")
    }
  }
}
