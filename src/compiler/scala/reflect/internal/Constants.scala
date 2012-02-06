/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

import java.lang.Integer.toOctalString
import annotation.switch

trait Constants extends api.Constants {
  self: SymbolTable =>

  import definitions._

  final val NoTag      = 0
  final val UnitTag    = 1
  final val BooleanTag = 2
  final val ByteTag    = 3
  final val ShortTag   = 4
  final val CharTag    = 5
  final val IntTag     = 6
  final val LongTag    = 7
  final val FloatTag   = 8
  final val DoubleTag  = 9
  final val StringTag  = 10
  final val NullTag    = 11
  final val ClassTag   = 12
  // For supporting java enumerations inside java annotations (see ClassfileParser)
  final val EnumTag    = 13

  case class Constant(value: Any) extends AbsConstant {
    val tag: Int = value match {
      case null         => NullTag
      case x: Unit      => UnitTag
      case x: Boolean   => BooleanTag
      case x: Byte      => ByteTag
      case x: Short     => ShortTag
      case x: Int       => IntTag
      case x: Long      => LongTag
      case x: Float     => FloatTag
      case x: Double    => DoubleTag
      case x: String    => StringTag
      case x: Char      => CharTag
      case x: Type      => ClassTag
      case x: Symbol    => EnumTag
      case _            => throw new Error("bad constant value: " + value + " of class " + value.getClass)
    }

    def isByteRange: Boolean  = isIntRange && Byte.MinValue <= intValue && intValue <= Byte.MaxValue
    def isShortRange: Boolean = isIntRange && Short.MinValue <= intValue && intValue <= Short.MaxValue
    def isCharRange: Boolean  = isIntRange && Char.MinValue <= intValue && intValue <= Char.MaxValue
    def isIntRange: Boolean   = ByteTag <= tag && tag <= IntTag
    def isLongRange: Boolean  = ByteTag <= tag && tag <= LongTag
    def isFloatRange: Boolean = ByteTag <= tag && tag <= FloatTag
    def isNumeric: Boolean    = ByteTag <= tag && tag <= DoubleTag
    def isNonUnitAnyVal       = BooleanTag <= tag && tag <= DoubleTag
    def isAnyVal              = UnitTag <= tag && tag <= DoubleTag

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
     */
    override def equals(other: Any): Boolean = other match {
      case that: Constant =>
        this.tag == that.tag &&
        (this.value == that.value || this.isNaN && that.isNaN)
      case _ => false
    }

    def isNaN = value match {
      case f: Float  => f.isNaN
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
     */
    def convertTo(pt: Type): Constant = {
      val target = pt.typeSymbol
      if (target == tpe.typeSymbol)
        this
      else if (target == ByteClass && isByteRange)
        Constant(byteValue)
      else if (target == ShortClass && isShortRange)
        Constant(shortValue)
      else if (target == CharClass && isCharRange)
        Constant(charValue)
      else if (target == IntClass && isIntRange)
        Constant(intValue)
      else if (target == LongClass && isLongRange)
        Constant(longValue)
      else if (target == FloatClass && isFloatRange)
        Constant(floatValue)
      else if (target == DoubleClass && isNumeric)
        Constant(doubleValue)
      else
        null
    }

    def stringValue: String =
      if (value == null) "null"
      else if (tag == ClassTag) signature(typeValue)
      else value.toString()

    @switch def escapedChar(ch: Char): String = ch match {
      case '\b' => "\\b"
      case '\t' => "\\t"
      case '\n' => "\\n"
      case '\f' => "\\f"
      case '\r' => "\\r"
      case '"'  => "\\\""
      case '\'' => "\\\'"
      case '\\' => "\\\\"
      case _    => if (ch.isControl) "\\0" + toOctalString(ch) else String.valueOf(ch)
    }

    def escapedStringValue: String = {
      def escape(text: String): String = text flatMap escapedChar
      tag match {
        case NullTag   => "null"
        case StringTag => "\"" + escape(stringValue) + "\""
        case ClassTag  => "classOf[" + signature(typeValue) + "]"
        case CharTag   => "'" + escapedChar(charValue) + "'"
        case LongTag   => longValue.toString() + "L"
        case _         => String.valueOf(value)
      }
    }
    def typeValue: Type     = value.asInstanceOf[Type]
    def symbolValue: Symbol = value.asInstanceOf[Symbol]

    override def hashCode: Int = value.## * 41 + 17
  }

  object Constant extends ConstantExtractor
}
