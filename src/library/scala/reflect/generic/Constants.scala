/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package generic

import java.lang.Integer.toOctalString
import PickleFormat._

trait Constants { self: Universe =>

  import definitions._

  final val NoTag      = LITERAL - LITERAL
  final val UnitTag    = LITERALunit - LITERAL
  final val BooleanTag = LITERALboolean - LITERAL
  final val ByteTag    = LITERALbyte - LITERAL
  final val ShortTag   = LITERALshort - LITERAL
  final val CharTag    = LITERALchar - LITERAL
  final val IntTag     = LITERALint - LITERAL
  final val LongTag    = LITERALlong - LITERAL
  final val FloatTag   = LITERALfloat - LITERAL
  final val DoubleTag  = LITERALdouble - LITERAL
  final val StringTag  = LITERALstring - LITERAL
  final val NullTag    = LITERALnull - LITERAL
  final val ClassTag   = LITERALclass - LITERAL
  // For supporting java enumerations inside java annotations (see ClassfileParser)
  final val EnumTag    = LITERALenum - LITERAL

  case class Constant(value: Any) {

    val tag: Int =
      if (value.isInstanceOf[Unit]) UnitTag
      else if (value.isInstanceOf[Boolean]) BooleanTag
      else if (value.isInstanceOf[Byte]) ByteTag
      else if (value.isInstanceOf[Short]) ShortTag
      else if (value.isInstanceOf[Char]) CharTag
      else if (value.isInstanceOf[Int]) IntTag
      else if (value.isInstanceOf[Long]) LongTag
      else if (value.isInstanceOf[Float]) FloatTag
      else if (value.isInstanceOf[Double]) DoubleTag
      else if (value.isInstanceOf[String]) StringTag
      else if (value.isInstanceOf[AbsType]) ClassTag
      else if (value.isInstanceOf[AbsSymbol]) EnumTag
      else if (value == null) NullTag
      else throw new Error("bad constant value: " + value)

    def isNumeric: Boolean = ByteTag <= tag && tag <= DoubleTag

    def tpe: Type = tag match {
      case UnitTag    => UnitClass.tpe
      case BooleanTag => BooleanClass.tpe
      case ByteTag    => ByteClass.tpe
      case ShortTag   => ShortClass.tpe
      case CharTag    => CharClass.tpe
      case IntTag     => IntClass.tpe
      case LongTag    => LongClass.tpe
      case FloatTag   => FloatClass.tpe
      case DoubleTag  => DoubleClass.tpe
      case StringTag  => StringClass.tpe
      case NullTag    => NullClass.tpe
      case ClassTag   => ClassType(value.asInstanceOf[Type])
      case EnumTag    =>
        // given (in java): "class A { enum E { VAL1 } }"
        //  - symbolValue: the symbol of the actual enumeration value (VAL1)
        //  - .owner: the ModuleClasSymbol of the enumeration (object E)
        //  - .linkedClassOfClass: the ClassSymbol of the enumeration (class E)
        symbolValue.owner.linkedClassOfClass.tpe
    }

    /** We need the equals method to take account of tags as well as values.
     *
     *  @param other ...
     *  @return      ...
     */
    override def equals(other: Any): Boolean = other match {
      case that: Constant =>
        this.tag == that.tag &&
        (this.value == that.value || this.isNaN && that.isNaN)
      case _ => false
    }

    def isNaN = value match {
      case f: Float => f.isNaN
      case d: Double => d.isNaN
      case _ => false
    }

    def booleanValue: Boolean =
      if (tag == BooleanTag) value.asInstanceOf[Boolean]
      else throw new Error("value " + value + " is not a boolean");

    def byteValue: Byte = tag match {
      case ByteTag   => value.asInstanceOf[Byte]
      case ShortTag  => value.asInstanceOf[Short].toByte
      case CharTag   => value.asInstanceOf[Char].toByte
      case IntTag    => value.asInstanceOf[Int].toByte
      case LongTag   => value.asInstanceOf[Long].toByte
      case FloatTag  => value.asInstanceOf[Float].toByte
      case DoubleTag => value.asInstanceOf[Double].toByte
      case _         => throw new Error("value " + value + " is not a Byte")
    }

    def shortValue: Short = tag match {
      case ByteTag   => value.asInstanceOf[Byte].toShort
      case ShortTag  => value.asInstanceOf[Short]
      case CharTag   => value.asInstanceOf[Char].toShort
      case IntTag    => value.asInstanceOf[Int].toShort
      case LongTag   => value.asInstanceOf[Long].toShort
      case FloatTag  => value.asInstanceOf[Float].toShort
      case DoubleTag => value.asInstanceOf[Double].toShort
      case _         => throw new Error("value " + value + " is not a Short")
    }

    def charValue: Char = tag match {
      case ByteTag   => value.asInstanceOf[Byte].toChar
      case ShortTag  => value.asInstanceOf[Short].toChar
      case CharTag   => value.asInstanceOf[Char]
      case IntTag    => value.asInstanceOf[Int].toChar
      case LongTag   => value.asInstanceOf[Long].toChar
      case FloatTag  => value.asInstanceOf[Float].toChar
      case DoubleTag => value.asInstanceOf[Double].toChar
      case _         => throw new Error("value " + value + " is not a Char")
    }

    def intValue: Int = tag match {
      case ByteTag   => value.asInstanceOf[Byte].toInt
      case ShortTag  => value.asInstanceOf[Short].toInt
      case CharTag   => value.asInstanceOf[Char].toInt
      case IntTag    => value.asInstanceOf[Int]
      case LongTag   => value.asInstanceOf[Long].toInt
      case FloatTag  => value.asInstanceOf[Float].toInt
      case DoubleTag => value.asInstanceOf[Double].toInt
      case _         => throw new Error("value " + value + " is not an Int")
    }

    def longValue: Long = tag match {
      case ByteTag   => value.asInstanceOf[Byte].toLong
      case ShortTag  => value.asInstanceOf[Short].toLong
      case CharTag   => value.asInstanceOf[Char].toLong
      case IntTag    => value.asInstanceOf[Int].toLong
      case LongTag   => value.asInstanceOf[Long]
      case FloatTag  => value.asInstanceOf[Float].toLong
      case DoubleTag => value.asInstanceOf[Double].toLong
      case _         => throw new Error("value " + value + " is not a Long")
    }

    def floatValue: Float = tag match {
      case ByteTag   => value.asInstanceOf[Byte].toFloat
      case ShortTag  => value.asInstanceOf[Short].toFloat
      case CharTag   => value.asInstanceOf[Char].toFloat
      case IntTag    => value.asInstanceOf[Int].toFloat
      case LongTag   => value.asInstanceOf[Long].toFloat
      case FloatTag  => value.asInstanceOf[Float]
      case DoubleTag => value.asInstanceOf[Double].toFloat
      case _         => throw new Error("value " + value + " is not a Float")
    }

    def doubleValue: Double = tag match {
      case ByteTag   => value.asInstanceOf[Byte].toDouble
      case ShortTag  => value.asInstanceOf[Short].toDouble
      case CharTag   => value.asInstanceOf[Char].toDouble
      case IntTag    => value.asInstanceOf[Int].toDouble
      case LongTag   => value.asInstanceOf[Long].toDouble
      case FloatTag  => value.asInstanceOf[Float].toDouble
      case DoubleTag => value.asInstanceOf[Double]
      case _         => throw new Error("value " + value + " is not a Double")
    }

    /** Convert constant value to conform to given type.
     *
     *  @param pt ...
     *  @return   ...
     */
    def convertTo(pt: Type): Constant = {
      val target = pt.typeSymbol
      if (target == tpe.typeSymbol)
        this
      else if (target == ByteClass && ByteTag <= tag && tag <= IntTag &&
          -128 <= intValue && intValue <= 127)
        Constant(byteValue)
      else if (target == ShortClass && ByteTag <= tag && tag <= IntTag &&
               -32768 <= intValue && intValue <= 32767)
        Constant(shortValue)
      else if (target == CharClass && ByteTag <= tag && tag <= IntTag  &&
               0 <= intValue && intValue <= 65635)
        Constant(charValue)
      else if (target == IntClass && ByteTag <= tag && tag <= IntTag)
        Constant(intValue)
      else if (target == LongClass && ByteTag <= tag && tag <= LongTag)
        Constant(longValue)
      else if (target == FloatClass && ByteTag <= tag && tag <= FloatTag)
        Constant(floatValue)
      else if (target == DoubleClass && ByteTag <= tag && tag <= DoubleTag)
        Constant(doubleValue)
      else {
        null
      }
    }

    def stringValue: String =
      if (value == null) "null"
      else if (tag == ClassTag) signature(typeValue)
      else value.toString()

    def escapedStringValue: String = {
      def escape(text: String): String = {
        val buf = new StringBuilder
        for (c <- text.iterator)
          if (c.isControl)
            buf.append("\\0" + toOctalString(c.asInstanceOf[Int]))
          else
            buf.append(c)
        buf.toString
      }
      tag match {
        case NullTag   => "null"
        case StringTag => "\"" + escape(stringValue) + "\""
        case ClassTag  => "classOf[" + signature(typeValue) + "]"
        case CharTag   => escape("\'" + charValue + "\'")
        case LongTag   => longValue.toString() + "L"
        case _         => value.toString()
      }
    }

    def typeValue: Type = value.asInstanceOf[Type]

    def symbolValue: Symbol = value.asInstanceOf[Symbol]

    override def hashCode: Int =
      if (value == null) 0 else value.## * 41 + 17
  }
}
