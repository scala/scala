/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect
package internal

import scala.annotation.{nowarn, switch}

trait Constants extends api.Constants {
  self: SymbolTable =>

  import definitions._

  final val NoTag        = 0
  final val UnitTag      = 1
  final val BooleanTag   = 2
  final val ByteTag      = 3
  final val ShortTag     = 4
  final val CharTag      = 5
  final val IntTag       = 6
  final val LongTag      = 7
  final val FloatTag     = 8
  final val DoubleTag    = 9
  final val StringTag    = 10
  final val NullTag      = 11
  final val ClazzTag     = 12
  // For supporting java enumerations inside java annotations (see ClassfileParser)
  final val EnumTag      = 13

  case class Constant(value: Any) extends ConstantApi {
    import java.lang.Double.doubleToRawLongBits
    import java.lang.Float.floatToRawIntBits

    val tag: Int = value match {
      case null       => NullTag
      case x: Unit    => UnitTag
      case x: Boolean => BooleanTag
      case x: Byte    => ByteTag
      case x: Short   => ShortTag
      case x: Int     => IntTag
      case x: Long    => LongTag
      case x: Float   => FloatTag
      case x: Double  => DoubleTag
      case x: String  => StringTag
      case x: Char    => CharTag
      case x: Type    => ClazzTag
      case x: Symbol  => EnumTag
      case _          => throw new Error("bad constant value: " + value + " of class " + value.getClass)
    }

    def isByteRange: Boolean  = isIntRange && Byte.MinValue <= intValue && intValue <= Byte.MaxValue
    def isShortRange: Boolean = isIntRange && Short.MinValue <= intValue && intValue <= Short.MaxValue
    def isCharRange: Boolean  = isIntRange && Char.MinValue <= intValue && intValue <= Char.MaxValue
    def isIntRange: Boolean   = ByteTag <= tag && tag <= IntTag
    def isLongRange: Boolean  = ByteTag <= tag && tag <= LongTag
    def isFloatRepresentable: Boolean = ByteTag <= tag && tag <= FloatTag && (tag != IntTag || intValue == intValue.toFloat.toInt) && (tag != LongTag || longValue == longValue.toFloat.toLong)
    def isDoubleRepresentable: Boolean = ByteTag <= tag && tag <= DoubleTag && (tag != LongTag || longValue == longValue.toDouble.toLong)
    def isNumeric: Boolean    = ByteTag <= tag && tag <= DoubleTag
    def isNonUnitAnyVal       = BooleanTag <= tag && tag <= DoubleTag
    def isSuitableLiteralType = BooleanTag <= tag && tag <= NullTag
    def isAnyVal              = UnitTag <= tag && tag <= DoubleTag

    def tpe: Type = tag match {
      case UnitTag    => UnitTpe
      case BooleanTag => BooleanTpe
      case ByteTag    => ByteTpe
      case ShortTag   => ShortTpe
      case CharTag    => CharTpe
      case IntTag     => IntTpe
      case LongTag    => LongTpe
      case FloatTag   => FloatTpe
      case DoubleTag  => DoubleTpe
      case StringTag  => StringTpe
      case NullTag    => NullTpe
      case ClazzTag   => ClassType(typeValue)
      case EnumTag    => EnumType(symbolValue)
    }

    /** We need the equals method to take account of tags as well as values.
     */
    // !!! In what circumstance could `equalHashValue == that.equalHashValue && tag != that.tag` be true?
    override def equals(other: Any): Boolean = other match {
      case that: Constant =>
        this.tag == that.tag && {
          //
          // Consider two `NaN`s to be identical, despite non-equality
          // Consider -0d to be distinct from 0d, despite equality
          //
          // We use the raw versions (i.e. `floatToRawIntBits` rather than `floatToIntBits`)
          // to avoid treating different encodings of `NaN` as the same constant.
          // You probably can't express different `NaN` varieties as compile time
          // constants in regular Scala code, but it is conceivable that you could
          // conjure them with a macro.
          //
          this.tag match {
            case NullTag =>
              true
            case FloatTag =>
              floatToRawIntBits(value.asInstanceOf[Float]) == floatToRawIntBits(that.value.asInstanceOf[Float])
            case DoubleTag =>
              doubleToRawLongBits(value.asInstanceOf[Double]) == doubleToRawLongBits(that.value.asInstanceOf[Double])
            case _ =>
              // we do not want cooperative equality for determining if constants are equal
              this.value.equals(that.value): @nowarn("cat=other-non-cooperative-equals")
          }
        }
      case _ => false
    }

    def isNaN = value match {
      case f: Float  => java.lang.Float.isNaN(f)
      case d: Double => java.lang.Double.isNaN(d)
      case _ => false
    }

    def booleanValue: Boolean =
      if (tag == BooleanTag) value.asInstanceOf[Boolean]
      else throw new Error("value " + value + " is not a boolean")

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
      else if (target == FloatClass && isFloatRepresentable)
        Constant(floatValue)
      else if (target == DoubleClass && isDoubleRepresentable)
        Constant(doubleValue)
      else
        null
    }

    def stringValue: String =
      if (value == null) "null"
      else if (tag == ClazzTag) signature(typeValue)
      else value.toString()

    def escapedStringValue: String = {
      import java.lang.StringBuilder
      def requiresFormat(c: Char): Boolean =
        (c: @switch) match {
          case '\b' | '\t' | '\n' | '\f' | '\r' | '"' | '\'' | '\\' => true
          case c => c.isControl
        }
      def escapedChar(b: StringBuilder, c: Char): Unit = {
        def quadNibble(b: StringBuilder, x: Int, i: Int): Unit =
          if (i < 4) {
            quadNibble(b, x >> 4, i + 1)
            val n = x & 0xF
            val c = if (n < 10) '0' + n else 'A' + (n - 10)
            b.append(c.toChar)
          }
        val replace = (c: @switch) match {
          case '\b' => "\\b"
          case '\t' => "\\t"
          case '\n' => "\\n"
          case '\f' => "\\f"
          case '\r' => "\\r"
          case '"'  => "\\\""
          case '\'' => "\\\'"
          case '\\' => "\\\\"
          case c =>
            if (c.isControl) {
              b.append("\\u")
              quadNibble(b, c.toInt, 0)
            }
            else b.append(c)
            return
        }
        b.append(replace)
      }
      def escape(text: String) = {
        def mustBuild: Boolean = {
          var i = 0
          while (i < text.length) {
            if (requiresFormat(text.charAt(i))) return true
            i += 1
          }
          false
        }
        if (mustBuild) {
          val b = new StringBuilder(text.length + 16).append('"')
          var i = 0
          while (i < text.length) {
            escapedChar(b, text.charAt(i))
            i += 1
          }
          b.append('"').toString
        }
        else "\"" + text + "\""
      }
      tag match {
        case NullTag   => "null"
        case StringTag => escape(stringValue)
        case ClazzTag  =>
          def show(tpe: Type) = "classOf[" + signature(tpe) + "]"
          typeValue match {
            case ErasedValueType(clazz, underlying) =>
              // A note on tpe_* usage here:
              //
              // We've intentionally erased the type arguments to the value class so that different
              // instantiations of a particular value class that erase to the same underlying type
              // don't result in spurious bridges (e.g. run/t6385.scala). I don't think that matters;
              // printing trees of `classOf[ValueClass[String]]` shows `classOf[ValueClass]` at phase
              // erasure both before and after the use of `tpe_*` here.
              show(clazz.tpe_*)
            case _ => show(typeValue)
          }
        case CharTag =>
          val c = charValue
          if (requiresFormat(c)) {
            val b = new StringBuilder().append('\'')
            escapedChar(b, c)
            b.append('\'').toString
          }
          else "'" + c + "'"
        case LongTag => longValue.toString() + "L"
        case EnumTag => symbolValue.name.toString()
        case _       => String.valueOf(value)
      }
    }
    def typeValue: Type     = value.asInstanceOf[Type]
    def symbolValue: Symbol = value.asInstanceOf[Symbol]

    override def hashCode: Int = {
      import scala.util.hashing.MurmurHash3._
      val seed = 17
      var h = seed
      h = mix(h, tag.##) // include tag in the hash, otherwise 0, 0d, 0L, 0f collide.
      val valueHash = tag match {
        case NullTag => 0
        // We could just use value.hashCode here, at the cost of a collision between different NaNs
        case FloatTag => java.lang.Integer.hashCode(floatToRawIntBits(value.asInstanceOf[Float]))
        case DoubleTag => java.lang.Long.hashCode(doubleToRawLongBits(value.asInstanceOf[Double]))
        case _ => value.hashCode()
      }
      h = mix(h, valueHash)
      finalizeHash(h, length = 2)
    }
  }

  object Constant extends ConstantExtractor

  implicit val ConstantTag: ClassTag[Constant] = ClassTag[Constant](classOf[Constant])
}
