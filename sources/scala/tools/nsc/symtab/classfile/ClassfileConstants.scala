/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scala.tools.nsc.symtab.classfile;

object ClassfileConstants {

  val JAVA_MAGIC = 0xCAFEBABE;
  val JAVA_MAJOR_VERSION = 45;
  val JAVA_MINOR_VERSION = 3;

  val JAVA_ACC_PUBLIC       = 0x0001;
  val JAVA_ACC_PRIVATE      = 0x0002;
  val JAVA_ACC_PROTECTED    = 0x0004;
  val JAVA_ACC_STATIC       = 0x0008;
  val JAVA_ACC_FINAL        = 0x0010;
  val JAVA_ACC_SUPER        = 0x0020;
  val JAVA_ACC_SYNCHRONIZED = 0x0020;
  val JAVA_ACC_VOLATILE     = 0x0040;
  val JAVA_ACC_BRIDGE       = 0x0040;
  val JAVA_ACC_TRANSIENT    = 0x0080;
  val JAVA_ACC_NATIVE       = 0x0100;
  val JAVA_ACC_INTERFACE    = 0x0200;
  val JAVA_ACC_ABSTRACT     = 0x0400;
  val JAVA_ACC_STRICT       = 0x0800;
  val JAVA_ACC_SYNTHETIC    = 0x1000;

  val CONSTANT_UTF8 = 1;
  val CONSTANT_UNICODE = 2;
  val CONSTANT_INTEGER = 3;
  val CONSTANT_FLOAT = 4;
  val CONSTANT_LONG = 5;
  val CONSTANT_DOUBLE = 6;
  val CONSTANT_CLASS = 7;
  val CONSTANT_STRING = 8;
  val CONSTANT_FIELDREF = 9;
  val CONSTANT_METHODREF = 10;
  val CONSTANT_INTFMETHODREF = 11;
  val CONSTANT_NAMEANDTYPE = 12;
}
