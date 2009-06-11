/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.symtab


import java.lang.Integer.toOctalString

import classfile.PickleFormat._

trait Constants {
  self: SymbolTable =>

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

  def isNumeric(tag: Int) = ByteTag <= tag && tag <= DoubleTag

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
      else if (value.isInstanceOf[Type]) ClassTag
      else if (value.isInstanceOf[Symbol]) EnumTag
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
      case NullTag    => NullClass.tpe
      case ClassTag   => Predef_classOfType(value.asInstanceOf[Type])
      case EnumTag    => symbolValue.owner.linkedClassOfClass.tpe
    }

    /** We need the equals method to take account of tags as well as values.
     *
     *  @param other ...
     *  @return      ...
     */
    override def equals(other: Any): Boolean = other match {
      case that: Constant => this.value == that.value && this.tag == that.tag
      case _ => false
    }

    def booleanValue: Boolean =
      if (tag == BooleanTag) value.asInstanceOf[Boolean]
      else throw new Error("value " + value + " is not a boolean");

    def byteValue: Byte = tag match {
      case ByteTag   => value.asInstanceOf[Byte]
      case ShortTag  => value.asInstanceOf[Short].asInstanceOf[Byte]
      case CharTag   => value.asInstanceOf[Char].asInstanceOf[Byte]
      case IntTag    => value.asInstanceOf[Int].asInstanceOf[Byte]
      case LongTag   => value.asInstanceOf[Long].asInstanceOf[Byte]
      case FloatTag  => value.asInstanceOf[Float].asInstanceOf[Byte]
      case DoubleTag => value.asInstanceOf[Double].asInstanceOf[Byte]
      case _         => throw new Error("value " + value + " is not a Byte")
    }

    def shortValue: Short = tag match {
      case ByteTag   => value.asInstanceOf[Byte].asInstanceOf[Short]
      case ShortTag  => value.asInstanceOf[Short]
      case CharTag   => value.asInstanceOf[Char].asInstanceOf[Short]
      case IntTag    => value.asInstanceOf[Int].asInstanceOf[Short]
      case LongTag   => value.asInstanceOf[Long].asInstanceOf[Short]
      case FloatTag  => value.asInstanceOf[Float].asInstanceOf[Short]
      case DoubleTag => value.asInstanceOf[Double].asInstanceOf[Short]
      case _         => throw new Error("value " + value + " is not a Short")
    }

    def charValue: Char = tag match {
      case ByteTag   => value.asInstanceOf[Byte].asInstanceOf[Char]
      case ShortTag  => value.asInstanceOf[Short].asInstanceOf[Char]
      case CharTag   => value.asInstanceOf[Char]
      case IntTag    => value.asInstanceOf[Int].asInstanceOf[Char]
      case LongTag   => value.asInstanceOf[Long].asInstanceOf[Char]
      case FloatTag  => value.asInstanceOf[Float].asInstanceOf[Char]
      case DoubleTag => value.asInstanceOf[Double].asInstanceOf[Char]
      case _         => throw new Error("value " + value + " is not a Char")
    }

    def intValue: Int = tag match {
      case ByteTag   => value.asInstanceOf[Byte].asInstanceOf[Int]
      case ShortTag  => value.asInstanceOf[Short].asInstanceOf[Int]
      case CharTag   => value.asInstanceOf[Char].asInstanceOf[Int]
      case IntTag    => value.asInstanceOf[Int]
      case LongTag   => value.asInstanceOf[Long].asInstanceOf[Int]
      case FloatTag  => value.asInstanceOf[Float].asInstanceOf[Int]
      case DoubleTag => value.asInstanceOf[Double].asInstanceOf[Int]
      case _         => throw new Error("value " + value + " is not an Int")
    }

    def longValue: Long = tag match {
      case ByteTag   => value.asInstanceOf[Byte].asInstanceOf[Long]
      case ShortTag  => value.asInstanceOf[Short].asInstanceOf[Long]
      case CharTag   => value.asInstanceOf[Char].asInstanceOf[Long]
      case IntTag    => value.asInstanceOf[Int].asInstanceOf[Long]
      case LongTag   => value.asInstanceOf[Long]
      case FloatTag  => value.asInstanceOf[Float].asInstanceOf[Long]
      case DoubleTag => value.asInstanceOf[Double].asInstanceOf[Long]
      case _         => throw new Error("value " + value + " is not a Long")
    }

    def floatValue: Float = tag match {
      case ByteTag   => value.asInstanceOf[Byte].asInstanceOf[Float]
      case ShortTag  => value.asInstanceOf[Short].asInstanceOf[Float]
      case CharTag   => value.asInstanceOf[Char].asInstanceOf[Float]
      case IntTag    => value.asInstanceOf[Int].asInstanceOf[Float]
      case LongTag   => value.asInstanceOf[Long].asInstanceOf[Float]
      case FloatTag  => value.asInstanceOf[Float]
      case DoubleTag => value.asInstanceOf[Double].asInstanceOf[Float]
      case _         => throw new Error("value " + value + " is not a Float")
    }

    def doubleValue: Double = tag match {
      case ByteTag   => value.asInstanceOf[Byte].asInstanceOf[Double]
      case ShortTag  => value.asInstanceOf[Short].asInstanceOf[Double]
      case CharTag   => value.asInstanceOf[Char].asInstanceOf[Double]
      case IntTag    => value.asInstanceOf[Int].asInstanceOf[Double]
      case LongTag   => value.asInstanceOf[Long].asInstanceOf[Double]
      case FloatTag  => value.asInstanceOf[Float].asInstanceOf[Double]
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

    override def hashCode(): Int =
      if (value == null) 0 else value.hashCode() * 41 + 17
  }
}
