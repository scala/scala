/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.symtab.classfile

object ClassfileConstants {

  final val JAVA_MAGIC = 0xCAFEBABE
  final val JAVA_MAJOR_VERSION = 45
  final val JAVA_MINOR_VERSION = 3

  final val JAVA_ACC_PUBLIC       = 0x0001
  final val JAVA_ACC_PRIVATE      = 0x0002
  final val JAVA_ACC_PROTECTED    = 0x0004
  final val JAVA_ACC_STATIC       = 0x0008
  final val JAVA_ACC_FINAL        = 0x0010
  final val JAVA_ACC_SUPER        = 0x0020
  final val JAVA_ACC_SYNCHRONIZED = 0x0020
  final val JAVA_ACC_VOLATILE     = 0x0040
  final val JAVA_ACC_BRIDGE       = 0x0040
  final val JAVA_ACC_TRANSIENT    = 0x0080
  final val JAVA_ACC_NATIVE       = 0x0100
  final val JAVA_ACC_INTERFACE    = 0x0200
  final val JAVA_ACC_ABSTRACT     = 0x0400
  final val JAVA_ACC_STRICT       = 0x0800
  final val JAVA_ACC_SYNTHETIC    = 0x1000
  final val JAVA_ACC_ANNOTATION   = 0x2000
  final val JAVA_ACC_ENUM         = 0x4000

  // tags describing the type of a literal in the constant pool
  final val CONSTANT_UTF8          =  1
  final val CONSTANT_UNICODE       =  2
  final val CONSTANT_INTEGER       =  3
  final val CONSTANT_FLOAT         =  4
  final val CONSTANT_LONG          =  5
  final val CONSTANT_DOUBLE        =  6
  final val CONSTANT_CLASS         =  7
  final val CONSTANT_STRING        =  8
  final val CONSTANT_FIELDREF      =  9
  final val CONSTANT_METHODREF     = 10
  final val CONSTANT_INTFMETHODREF = 11
  final val CONSTANT_NAMEANDTYPE   = 12

  // tags desribing the type of a literal in attribute values
  final val BYTE_TAG   = 'B'
  final val CHAR_TAG   = 'C'
  final val DOUBLE_TAG = 'D'
  final val FLOAT_TAG  = 'F'
  final val INT_TAG    = 'I'
  final val LONG_TAG   = 'J'
  final val SHORT_TAG  = 'S'
  final val BOOL_TAG   = 'Z'
  final val STRING_TAG = 's'
  final val ENUM_TAG   = 'e'
  final val CLASS_TAG  = 'c'
  final val ARRAY_TAG  = '['
  final val VOID_TAG   = 'V'
  final val ANNOTATION_TAG = '@'

}
