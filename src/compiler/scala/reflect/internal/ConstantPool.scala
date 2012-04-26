/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.reflect.internal

import ClassfileConstants._

object ConstantPool {
  final case class JvmVersion(minorVersion: Int, majorVersion: Int)

  type UShort = Char

  final val CONSTANT_Utf8               = 1
  final val CONSTANT_Integer            = 3
  final val CONSTANT_Float              = 4
  final val CONSTANT_Long               = 5
  final val CONSTANT_Double             = 6
  final val CONSTANT_Class              = 7
  final val CONSTANT_String             = 8
  final val CONSTANT_Fieldref           = 9
  final val CONSTANT_Methodref          = 10
  final val CONSTANT_InterfaceMethodref = 11
  final val CONSTANT_NameAndType        = 12

  /*
    4.2.2 Unqualified Names
    Names of methods, fields and local variables are stored as unqualified
    names. Unqualified names must not contain the characters '.', ';', '['
    or '/'. Method names are further constrained so that, with the exception
    of the special method names <init> and <clinit> (§3.9), they must not
    contain the characters '<' or '>'.

    4.3 Descriptors and Signatures
    A descriptor is a string representing the type of a field or method.
    Descriptors are represented in the class file format using modified
    UTF-8 strings (§4.4.7) and thus may be drawn, where not further
    constrained, from the entire Unicode character set. A signature is a
    string representing the generic type of a field or method, or generic
    type information for a class declaration.
  */
  abstract class Name_Info(tag: Byte) extends PoolEntry(tag) {
    def name_index: UShort
  }
  abstract class Ref_Info(tag: Byte) extends PoolEntry(tag) {
    def class_index: UShort
    def name_and_type_index: UShort
  }
  class Class_info(val name_index: UShort) extends Name_Info(CONSTANT_Class) { }
  class Double_info(val value: Double) extends PoolEntry(CONSTANT_Double) {
    override def width = 2
  }
  class Fieldref_info(val class_index: UShort, val name_and_type_index: UShort) extends Ref_Info(CONSTANT_Fieldref)
  class Float_info(val value: Float) extends PoolEntry(CONSTANT_Float)
  class Integer_info(val value: Int) extends PoolEntry(CONSTANT_Integer)
  class InterfaceMethodref_info(val class_index: UShort, val name_and_type_index: UShort) extends Ref_Info(CONSTANT_InterfaceMethodref)
  class Long_info(val value: Long) extends PoolEntry(CONSTANT_Long) {
    override def width = 2
  }
  class Methodref_info(val class_index: UShort, val name_and_type_index: UShort) extends Ref_Info(CONSTANT_Methodref)
  class NameAndType_info(val name_index: UShort, val descriptor_index: UShort) extends Name_Info(CONSTANT_NameAndType) {
    override def toString = "NameAndType  #%s:#%s;".format(name_index, descriptor_index)
  }
  class String_info(val string_index: UShort) extends PoolEntry(CONSTANT_String) { }
  class Utf8_info(override val stringValue: String) extends PoolEntry(CONSTANT_Utf8) {
    override def toString = ("Asciz    " + stringValue).trim
  }

  abstract class PoolEntry(tag: Byte) {
    def width = 1
    def stringValue: String = sys.error("Not a String-valued constant pool entry: " + this)
    override def toString = (
      getClass.getName.split("[.$]").last + "/" + tag
    )
  }
  object NoEntry extends PoolEntry(-1) { }
}
