/*     ___ ____ ___   __   ___   ___
**    / _// __// _ | / /  / _ | / _ \    Scala classfile decoder
**  __\ \/ /__/ __ |/ /__/ __ |/ ___/    (c) 2003-2009, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_/_/        http://scala-lang.org/
**
*/

// $Id$

package scala.tools.scalap


object Classfiles {
  final val JAVA_MAGIC = 0xCAFEBABE
  final val JAVA_MAJOR_VERSION = 45
  final val JAVA_MINOR_VERSION = 3

  final val CONSTANT_UTF8 = 1
  final val CONSTANT_UNICODE = 2
  final val CONSTANT_INTEGER = 3
  final val CONSTANT_FLOAT = 4
  final val CONSTANT_LONG = 5
  final val CONSTANT_DOUBLE = 6
  final val CONSTANT_CLASS = 7
  final val CONSTANT_STRING = 8
  final val CONSTANT_FIELDREF = 9
  final val CONSTANT_METHODREF = 10
  final val CONSTANT_INTFMETHODREF = 11
  final val CONSTANT_NAMEANDTYPE = 12

  final val BAD_ATTR = 0x00000
  final val SOURCEFILE_ATTR = 0x00001
  final val SYNTHETIC_ATTR = 0x00002
  final val DEPRECATED_ATTR = 0x00004
  final val CODE_ATTR = 0x00008
  final val EXCEPTIONS_ATTR = 0x00010
  final val CONSTANT_VALUE_ATTR = 0x00020
  final val LINE_NUM_TABLE_ATTR = 0x00040
  final val LOCAL_VAR_TABLE_ATTR = 0x00080
  final val INNERCLASSES_ATTR = 0x08000
  final val META_ATTR = 0x10000
  final val SCALA_ATTR = 0x20000

  final val SOURCEFILE_N = "SourceFile"
  final val SYNTHETIC_N = "Synthetic"
  final val DEPRECATED_N = "Deprecated"
  final val CODE_N = "Code"
  final val EXCEPTIONS_N = "Exceptions"
  final val CONSTANT_VALUE_N = "ConstantValue"
  final val LINE_NUM_TABLE_N = "LineNumberTable"
  final val LOCAL_VAR_TABLE_N = "LocalVariableTable"
  final val INNERCLASSES_N = "InnerClasses"
  final val META_N = "JacoMeta"
  final val SCALA_N = "ScalaSignature"
  final val CONSTR_N = "<init>"
}

