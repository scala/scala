/*     ___ ____ ___   __   ___   ___
**    / _// __// _ | / /  / _ | / _ \    Scala classfile decoder
**  __\ \/ /__/ __ |/ /__/ __ |/ ___/    (c) 2003-2013, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_/_/        http://scala-lang.org/
**
*/


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

  final val constantTagToString = Map(
    CONSTANT_UTF8 -> "UTF8",
    CONSTANT_UNICODE -> "Unicode",
    CONSTANT_INTEGER -> "Int",
    CONSTANT_FLOAT -> "Float",
    CONSTANT_LONG -> "Long",
    CONSTANT_DOUBLE -> "Double",
    CONSTANT_CLASS -> "class",
    CONSTANT_STRING -> "Asciz",
    CONSTANT_FIELDREF -> "Field",
    CONSTANT_METHODREF -> "Method",
    CONSTANT_INTFMETHODREF -> "InterfaceMethod",
    CONSTANT_NAMEANDTYPE -> "NameAndType"
  )
}

