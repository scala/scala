/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.symtab.classfile;

import scalac.util.Name;

public interface ClassfileConstants {

    int JAVA_MAGIC = 0xCAFEBABE;
    int JAVA_MAJOR_VERSION = 45;
    int JAVA_MINOR_VERSION = 3;

    int CONSTANT_UTF8 = 1;
    int CONSTANT_UNICODE = 2;
    int CONSTANT_INTEGER = 3;
    int CONSTANT_FLOAT = 4;
    int CONSTANT_LONG = 5;
    int CONSTANT_DOUBLE = 6;
    int CONSTANT_CLASS = 7;
    int CONSTANT_STRING = 8;
    int CONSTANT_FIELDREF = 9;
    int CONSTANT_METHODREF = 10;
    int CONSTANT_INTFMETHODREF = 11;
    int CONSTANT_NAMEANDTYPE = 12;

    int BAD_ATTR = 0x00000;
    int SOURCEFILE_ATTR = 0x00001;
    int SYNTHETIC_ATTR = 0x00002;
    int DEPRECATED_ATTR = 0x00004;
    int CODE_ATTR = 0x00008;
    int EXCEPTIONS_ATTR = 0x00010;
    int CONSTANT_VALUE_ATTR = 0x00020;
    int LINE_NUM_TABLE_ATTR = 0x00040;
    int LOCAL_VAR_TABLE_ATTR = 0x00080;
    int INNERCLASSES_ATTR = 0x08000;
    int META_ATTR = 0x10000;
    int SCALA_ATTR = 0x20000;
    int JACO_ATTR = 0x40000;

    Name SOURCEFILE_N = Name.fromString("SourceFile");
    Name SYNTHETIC_N = Name.fromString("Synthetic");
    Name DEPRECATED_N = Name.fromString("Deprecated");
    Name CODE_N = Name.fromString("Code");
    Name EXCEPTIONS_N = Name.fromString("Exceptions");
    Name CONSTANT_VALUE_N = Name.fromString("ConstantValue");
    Name LINE_NUM_TABLE_N = Name.fromString("LineNumberTable");
    Name LOCAL_VAR_TABLE_N = Name.fromString("LocalVariableTable");
    Name INNERCLASSES_N = Name.fromString("InnerClasses");
    Name META_N = Name.fromString("JacoMeta");
    Name SCALA_N = Name.fromString("ScalaSignature");
    Name JACO_N = Name.fromString("JacoInterface");
    Name CONSTR_N = Name.fromString("<init>");
}
