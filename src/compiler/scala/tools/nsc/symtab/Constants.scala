/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.symtab


import java.lang.Integer.toOctalString
import compat.StringBuilder

import classfile.PickleFormat._

trait Constants requires SymbolTable {

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
  final val EnumTag    = ClassTag + 1
  final val ArrayTag   = EnumTag + 1

  def isNumeric(tag: int) = ByteTag <= tag && tag <= DoubleTag

  case class Constant(value: Any) {

    val tag: int =
      if (value.isInstanceOf[unit]) UnitTag
      else if (value.isInstanceOf[boolean]) BooleanTag
      else if (value.isInstanceOf[byte]) ByteTag
      else if (value.isInstanceOf[short]) ShortTag
      else if (value.isInstanceOf[char]) CharTag
      else if (value.isInstanceOf[int]) IntTag
      else if (value.isInstanceOf[long]) LongTag
      else if (value.isInstanceOf[float]) FloatTag
      else if (value.isInstanceOf[double]) DoubleTag
      else if (value.isInstanceOf[String]) StringTag
      else if (value.isInstanceOf[Type]) ClassTag
      else if (value.isInstanceOf[Symbol]) EnumTag
      else if (value.isInstanceOf[Array[Constant]]) ArrayTag
      else if (value == null) NullTag
      else throw new Error("bad constant value: " + value)

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
      case NullTag    => AllRefClass.tpe
      case ClassTag   => ClassClass.tpe
      case EnumTag    => symbolValue.owner.linkedClassOfClass.tpe
    }

    /** We need the equals method to take account of tags as well as values.
     *
     *  @param other ...
     *  @return      ...
     */
    override def equals(other: Any): boolean = other match {
      case that: Constant => this.value == that.value && this.tag == that.tag
      case _ => false
    }

    def booleanValue: boolean =
      if (tag == BooleanTag) value.asInstanceOf[boolean]
      else throw new Error("value " + value + " is not a boolean");

    def byteValue: byte = tag match {
      case ByteTag   => value.asInstanceOf[byte]
      case ShortTag  => value.asInstanceOf[short].asInstanceOf[byte]
      case CharTag   => value.asInstanceOf[char].asInstanceOf[byte]
      case IntTag    => value.asInstanceOf[int].asInstanceOf[byte]
      case LongTag   => value.asInstanceOf[long].asInstanceOf[byte]
      case FloatTag  => value.asInstanceOf[float].asInstanceOf[byte]
      case DoubleTag => value.asInstanceOf[double].asInstanceOf[byte]
      case _         => throw new Error("value " + value + " is not a byte")
    }

    def shortValue: short = tag match {
      case ByteTag   => value.asInstanceOf[byte].asInstanceOf[short]
      case ShortTag  => value.asInstanceOf[short]
      case CharTag   => value.asInstanceOf[char].asInstanceOf[short]
      case IntTag    => value.asInstanceOf[int].asInstanceOf[short]
      case LongTag   => value.asInstanceOf[long].asInstanceOf[short]
      case FloatTag  => value.asInstanceOf[float].asInstanceOf[short]
      case DoubleTag => value.asInstanceOf[double].asInstanceOf[short]
      case _         => throw new Error("value " + value + " is not a short")
    }

    def charValue: char = tag match {
      case ByteTag   => value.asInstanceOf[byte].asInstanceOf[char]
      case ShortTag  => value.asInstanceOf[short].asInstanceOf[char]
      case CharTag   => value.asInstanceOf[char]
      case IntTag    => value.asInstanceOf[int].asInstanceOf[char]
      case LongTag   => value.asInstanceOf[long].asInstanceOf[char]
      case FloatTag  => value.asInstanceOf[float].asInstanceOf[char]
      case DoubleTag => value.asInstanceOf[double].asInstanceOf[char]
      case _         => throw new Error("value " + value + " is not a char")
    }

    def intValue: int = tag match {
      case ByteTag   => value.asInstanceOf[byte].asInstanceOf[int]
      case ShortTag  => value.asInstanceOf[short].asInstanceOf[int]
      case CharTag   => value.asInstanceOf[char].asInstanceOf[int]
      case IntTag    => value.asInstanceOf[int]
      case LongTag   => value.asInstanceOf[long].asInstanceOf[int]
      case FloatTag  => value.asInstanceOf[float].asInstanceOf[int]
      case DoubleTag => value.asInstanceOf[double].asInstanceOf[int]
      case _         => throw new Error("value " + value + " is not an int")
    }

    def longValue: long = tag match {
      case ByteTag   => value.asInstanceOf[byte].asInstanceOf[long]
      case ShortTag  => value.asInstanceOf[short].asInstanceOf[long]
      case CharTag   => value.asInstanceOf[char].asInstanceOf[long]
      case IntTag    => value.asInstanceOf[int].asInstanceOf[long]
      case LongTag   => value.asInstanceOf[long]
      case FloatTag  => value.asInstanceOf[float].asInstanceOf[long]
      case DoubleTag => value.asInstanceOf[double].asInstanceOf[long]
      case _         => throw new Error("value " + value + " is not a long")
    }

    def floatValue: float = tag match {
      case ByteTag   => value.asInstanceOf[byte].asInstanceOf[float]
      case ShortTag  => value.asInstanceOf[short].asInstanceOf[float]
      case CharTag   => value.asInstanceOf[char].asInstanceOf[float]
      case IntTag    => value.asInstanceOf[int].asInstanceOf[float]
      case LongTag   => value.asInstanceOf[long].asInstanceOf[float]
      case FloatTag  => value.asInstanceOf[float]
      case DoubleTag => value.asInstanceOf[double].asInstanceOf[float]
      case _         => throw new Error("value " + value + " is not a float")
    }

    def doubleValue: double = tag match {
      case ByteTag   => value.asInstanceOf[byte].asInstanceOf[double]
      case ShortTag  => value.asInstanceOf[short].asInstanceOf[double]
      case CharTag   => value.asInstanceOf[char].asInstanceOf[double]
      case IntTag    => value.asInstanceOf[int].asInstanceOf[double]
      case LongTag   => value.asInstanceOf[long].asInstanceOf[double]
      case FloatTag  => value.asInstanceOf[float].asInstanceOf[double]
      case DoubleTag => value.asInstanceOf[double]
      case _         => throw new Error("value " + value + " is not a double")
    }

    /** Convert constant value to conform to given type.
     *
     *  @param pt ...
     *  @return   ...
     */
    def convertTo(pt: Type): Constant = {
      val target = pt.symbol
      if (target == tpe.symbol)
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
        for (val c <- Iterator.fromString(text))
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

    def arrayValue: Array[Constant] =
      throw new Error("value " + value + " is not an array")

    override def hashCode(): int =
      if (value == null) 0 else value.hashCode() * 41 + 17
  }

  class ArrayConstant(override val arrayValue: Array[Constant],
                   override val tpe: Type)
  extends Constant(arrayValue) {
    override def toString() = arrayValue.mkString("Constant(", "," , ")")
  }

}
