/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc.symtab;

import classfile.PickleFormat._;

[_trait_] abstract class Constants: SymbolTable {

  import definitions._;

  final val UnitTag    = LITERALunit - LITERAL;
  final val BooleanTag = LITERALboolean - LITERAL;
  final val ByteTag    = LITERALbyte - LITERAL;
  final val ShortTag   = LITERALshort - LITERAL;
  final val CharTag    = LITERALchar - LITERAL;
  final val IntTag     = LITERALint - LITERAL;
  final val LongTag    = LITERALlong - LITERAL;
  final val FloatTag   = LITERALfloat - LITERAL;
  final val DoubleTag  = LITERALdouble - LITERAL;
  final val StringTag  = LITERALstring - LITERAL;
  final val NullTag    = LITERALnull - LITERAL;
  final val ZeroTag    = LITERALzero - LITERAL;

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
      else if (value == null) NullTag
      else throw new Error("bad constant value: " + value);

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
    }

    /** We need the equals method to take account of tags as well as values */
    override def equals(other: Any): boolean = other match {
      case that: Constant => this.value == that.value && this.tag == that.tag
      case _ => false
    }

    def booleanValue: boolean =
      if (tag == BooleanTag) value.asInstanceOf$erased[boolean]
      else throw new Error("value " + value + " is not a boolean");

    def byteValue: byte = tag match {
      case ByteTag   => value.asInstanceOf$erased[byte]
      case ShortTag  => value.asInstanceOf$erased[short].asInstanceOf[byte]
      case CharTag   => value.asInstanceOf$erased[char].asInstanceOf[byte]
      case IntTag    => value.asInstanceOf$erased[int].asInstanceOf[byte]
      case LongTag   => value.asInstanceOf$erased[long].asInstanceOf[byte]
      case FloatTag  => value.asInstanceOf$erased[float].asInstanceOf[byte]
      case DoubleTag => value.asInstanceOf$erased[double].asInstanceOf[byte]
      case _         => throw new Error("value " + value + " is not a byte")
    }

    def shortValue: short = tag match {
      case ByteTag   => value.asInstanceOf$erased[byte].asInstanceOf[short]
      case ShortTag  => value.asInstanceOf$erased[short]
      case CharTag   => value.asInstanceOf$erased[char].asInstanceOf[short]
      case IntTag    => value.asInstanceOf$erased[int].asInstanceOf[short]
      case LongTag   => value.asInstanceOf$erased[long].asInstanceOf[short]
      case FloatTag  => value.asInstanceOf$erased[float].asInstanceOf[short]
      case DoubleTag => value.asInstanceOf$erased[double].asInstanceOf[short]
      case _         => throw new Error("value " + value + " is not a short")
    }

    def charValue: char = tag match {
      case ByteTag   => value.asInstanceOf$erased[byte].asInstanceOf[char]
      case ShortTag  => value.asInstanceOf$erased[short].asInstanceOf[char]
      case CharTag   => value.asInstanceOf$erased[char]
      case IntTag    => value.asInstanceOf$erased[int].asInstanceOf[char]
      case LongTag   => value.asInstanceOf$erased[long].asInstanceOf[char]
      case FloatTag  => value.asInstanceOf$erased[float].asInstanceOf[char]
      case DoubleTag => value.asInstanceOf$erased[double].asInstanceOf[char]
      case _         => throw new Error("value " + value + " is not a char")
    }

    def intValue: int = tag match {
      case ByteTag   => value.asInstanceOf$erased[byte].asInstanceOf[int]
      case ShortTag  => value.asInstanceOf$erased[short].asInstanceOf[int]
      case CharTag   => value.asInstanceOf$erased[char].asInstanceOf[int]
      case IntTag    => value.asInstanceOf$erased[int]
      case LongTag   => value.asInstanceOf$erased[long].asInstanceOf[int]
      case FloatTag  => value.asInstanceOf$erased[float].asInstanceOf[int]
      case DoubleTag => value.asInstanceOf$erased[double].asInstanceOf[int]
      case _         => throw new Error("value " + value + " is not an int")
    }

    def longValue: long = tag match {
      case ByteTag   => value.asInstanceOf$erased[byte].asInstanceOf[long]
      case ShortTag  => value.asInstanceOf$erased[short].asInstanceOf[long]
      case CharTag   => value.asInstanceOf$erased[char].asInstanceOf[long]
      case IntTag    => value.asInstanceOf$erased[int].asInstanceOf[long]
      case LongTag   => value.asInstanceOf$erased[long]
      case FloatTag  => value.asInstanceOf$erased[float].asInstanceOf[long]
      case DoubleTag => value.asInstanceOf$erased[double].asInstanceOf[long]
      case _         => throw new Error("value " + value + " is not a long")
    }

    def floatValue: float = tag match {
      case ByteTag   => value.asInstanceOf$erased[byte].asInstanceOf[float]
      case ShortTag  => value.asInstanceOf$erased[short].asInstanceOf[float]
      case CharTag   => value.asInstanceOf$erased[char].asInstanceOf[float]
      case IntTag    => value.asInstanceOf$erased[int].asInstanceOf[float]
      case LongTag   => value.asInstanceOf$erased[long].asInstanceOf[float]
      case FloatTag  => value.asInstanceOf$erased[float]
      case DoubleTag => value.asInstanceOf$erased[double].asInstanceOf[float]
      case _         => throw new Error("value " + value + " is not a float")
    }
/*
    def doubleValue: double = {
      System.out.println("doubleValue " + tag + " " + value);
      tag match {
        case ByteTag   => System.out.println("Byte"); value.asInstanceOf$erased[byte].asInstanceOf[double]
        case ShortTag  => System.out.println("Short"); value.asInstanceOf$erased[short].asInstanceOf[double]
        case CharTag   => System.out.println("Char"); value.asInstanceOf$erased[char].asInstanceOf[double]
        case IntTag    => System.out.println("Int"); value.asInstanceOf$erased[int].asInstanceOf[double]
        case LongTag   => System.out.println("Long"); value.asInstanceOf$erased[long].asInstanceOf[double]
        case FloatTag  => System.out.println("Float"); value.asInstanceOf$erased[float].asInstanceOf[double]
        case DoubleTag => System.out.println("Double"); value.asInstanceOf$erased[double]
        case _         => System.out.println("error"); throw new Error("value " + value + " is not a double")
      }
    }
*/
    def doubleValue: double = tag match {
      case ByteTag   => value.asInstanceOf$erased[byte].asInstanceOf[double]
      case ShortTag  => value.asInstanceOf$erased[short].asInstanceOf[double]
      case CharTag   => value.asInstanceOf$erased[char].asInstanceOf[double]
      case IntTag    => value.asInstanceOf$erased[int].asInstanceOf[double]
      case LongTag   => value.asInstanceOf$erased[long].asInstanceOf[double]
      case FloatTag  => value.asInstanceOf$erased[float].asInstanceOf[double]
      case DoubleTag => value.asInstanceOf$erased[double]
      case _         => throw new Error("value " + value + " is not a double")
    }

    /** Convert constant value to conform to given type */
    def convertTo(pt: Type): Constant = {
      val target = pt.symbol;
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
      else null
    }

    def stringValue: String =
      if (value == null) "null" else value.toString();

    override def hashCode(): int =
      if (value == null) 0 else value.hashCode() * 41 + 17;
  }

}
