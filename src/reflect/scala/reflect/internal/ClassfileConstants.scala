/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package reflect
package internal

import scala.annotation.switch

object ClassfileConstants {
  final val JAVA_MAGIC = 0xCAFEBABE
  final val JAVA_MAJOR_VERSION = 45
  final val JAVA_MINOR_VERSION = 3

  /** (see http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.1)
   *
   *  If the `ACC_INTERFACE` flag is set, the `ACC_ABSTRACT` flag must also
   *  be set (ch. 2.13.1).
   *
   *  A class file cannot have both its `ACC_FINAL` and `ACC_ABSTRACT` flags
   *  set (ch. 2.8.2).
   *
   *  A field may have at most one of its `ACC_PRIVATE`, `ACC_PROTECTED`,
   *  `ACC_PUBLIC` flags set (ch. 2.7.4).
   *
   *  A field may not have both its `ACC_FINAL` and `ACC_VOLATILE` flags set
   *  (ch. 2.9.1).
   *
   *  If a method has its `ACC_ABSTRACT` flag set it must not have any of its
   *  `ACC_FINAL`, `ACC_NATIVE`, `ACC_PRIVATE`, `ACC_STATIC`, `ACC_STRICT`,
   *  or `ACC_SYNCHRONIZED` flags set (ch. 2.13.3.2).
   *
   *  All interface methods must have their `ACC_ABSTRACT` and
   *  `ACC_PUBLIC` flags set.
   *
   *  Note for future reference: see this thread on ACC_SUPER and
   *  how its enforcement differs on the android vm.
   *    https://groups.google.com/forum/?hl=en#!topic/jvm-languages/jVhzvq8-ZIk
   *
   */                                        // Class   Field   Method
  final val JAVA_ACC_PUBLIC       = 0x0001   //   X       X        X
  final val JAVA_ACC_PRIVATE      = 0x0002   //           X        X
  final val JAVA_ACC_PROTECTED    = 0x0004   //           X        X
  final val JAVA_ACC_STATIC       = 0x0008   //           X        X
  final val JAVA_ACC_FINAL        = 0x0010   //   X       X        X
  final val JAVA_ACC_SUPER        = 0x0020   //   X
  final val JAVA_ACC_SYNCHRONIZED = 0x0020   //                    X
  final val JAVA_ACC_VOLATILE     = 0x0040   //           X
  final val JAVA_ACC_BRIDGE       = 0x0040   //                    X
  final val JAVA_ACC_TRANSIENT    = 0x0080   //           X
  final val JAVA_ACC_VARARGS      = 0x0080   //                    X
  final val JAVA_ACC_NATIVE       = 0x0100   //                    X
  final val JAVA_ACC_INTERFACE    = 0x0200   //   X
  final val JAVA_ACC_ABSTRACT     = 0x0400   //   X                X
  final val JAVA_ACC_STRICT       = 0x0800   //                    X
  final val JAVA_ACC_SYNTHETIC    = 0x1000   //   X       X        X
  final val JAVA_ACC_ANNOTATION   = 0x2000   //   X
  final val JAVA_ACC_ENUM         = 0x4000   //   X       X

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
  final val CONSTANT_METHODHANDLE  = 15
  final val CONSTANT_METHODTYPE    = 16
  final val CONSTANT_INVOKEDYNAMIC = 18

  // tags describing the type of a literal in attribute values
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
  final val TVAR_TAG   = 'T'
  final val OBJECT_TAG = 'L'
  final val ANNOTATION_TAG = '@'
  final val SCALA_NOTHING = "scala.runtime.Nothing$"
  final val SCALA_NULL = "scala.runtime.Null$"


  // tags describing the type of newarray
  final val T_BOOLEAN = 4
  final val T_CHAR    = 5
  final val T_FLOAT   = 6
  final val T_DOUBLE  = 7
  final val T_BYTE    = 8
  final val T_SHORT   = 9
  final val T_INT     = 10
  final val T_LONG    = 11

  // JVM mnemonics
  final val nop         = 0x00
  final val aconst_null = 0x01
  final val iconst_m1   = 0x02

  final val iconst_0    = 0x03
  final val iconst_1    = 0x04
  final val iconst_2    = 0x05
  final val iconst_3    = 0x06
  final val iconst_4    = 0x07
  final val iconst_5    = 0x08

  final val lconst_0    = 0x09
  final val lconst_1    = 0x0a
  final val fconst_0    = 0x0b
  final val fconst_1    = 0x0c
  final val fconst_2    = 0x0d
  final val dconst_0    = 0x0e
  final val dconst_1    = 0x0f

  final val bipush      = 0x10
  final val sipush      = 0x11
  final val ldc         = 0x12
  final val ldc_w       = 0x13
  final val ldc2_w      = 0x14

  final val iload       = 0x15
  final val lload       = 0x16
  final val fload       = 0x17
  final val dload       = 0x18
  final val aload       = 0x19

  final val iload_0     = 0x1a
  final val iload_1     = 0x1b
  final val iload_2     = 0x1c
  final val iload_3     = 0x1d
  final val lload_0     = 0x1e
  final val lload_1     = 0x1f
  final val lload_2     = 0x20
  final val lload_3     = 0x21
  final val fload_0     = 0x22
  final val fload_1     = 0x23
  final val fload_2     = 0x24
  final val fload_3     = 0x25
  final val dload_0     = 0x26
  final val dload_1     = 0x27
  final val dload_2     = 0x28
  final val dload_3     = 0x29
  final val aload_0     = 0x2a
  final val aload_1     = 0x2b
  final val aload_2     = 0x2c
  final val aload_3     = 0x2d
  final val iaload      = 0x2e
  final val laload      = 0x2f
  final val faload      = 0x30
  final val daload      = 0x31
  final val aaload      = 0x32
  final val baload      = 0x33
  final val caload      = 0x34
  final val saload      = 0x35

  final val istore      = 0x36
  final val lstore      = 0x37
  final val fstore      = 0x38
  final val dstore      = 0x39
  final val astore      = 0x3a
  final val istore_0    = 0x3b
  final val istore_1    = 0x3c
  final val istore_2    = 0x3d
  final val istore_3    = 0x3e
  final val lstore_0    = 0x3f
  final val lstore_1    = 0x40
  final val lstore_2    = 0x41
  final val lstore_3    = 0x42
  final val fstore_0    = 0x43
  final val fstore_1    = 0x44
  final val fstore_2    = 0x45
  final val fstore_3    = 0x46
  final val dstore_0    = 0x47
  final val dstore_1    = 0x48
  final val dstore_2    = 0x49
  final val dstore_3    = 0x4a
  final val astore_0    = 0x4b
  final val astore_1    = 0x4c
  final val astore_2    = 0x4d
  final val astore_3    = 0x4e
  final val iastore     = 0x4f
  final val lastore     = 0x50
  final val fastore     = 0x51
  final val dastore     = 0x52
  final val aastore     = 0x53
  final val bastore     = 0x54
  final val castore     = 0x55
  final val sastore     = 0x56

  final val pop         = 0x57
  final val pop2        = 0x58
  final val dup         = 0x59
  final val dup_x1      = 0x5a
  final val dup_x2      = 0x5b
  final val dup2        = 0x5c
  final val dup2_x1     = 0x5d
  final val dup2_x2     = 0x5e
  final val swap        = 0x5f

  final val iadd        = 0x60
  final val ladd        = 0x61
  final val fadd        = 0x62
  final val dadd        = 0x63
  final val isub        = 0x64
  final val lsub        = 0x65
  final val fsub        = 0x66
  final val dsub        = 0x67
  final val imul        = 0x68
  final val lmul        = 0x69
  final val fmul        = 0x6a
  final val dmul        = 0x6b
  final val idiv        = 0x6c
  final val ldiv        = 0x6d
  final val fdiv        = 0x6e
  final val ddiv        = 0x6f
  final val irem        = 0x70
  final val lrem        = 0x71
  final val frem        = 0x72
  final val drem        = 0x73

  final val ineg        = 0x74
  final val lneg        = 0x75
  final val fneg        = 0x76
  final val dneg        = 0x77

  final val ishl        = 0x78
  final val lshl        = 0x79
  final val ishr        = 0x7a
  final val lshr        = 0x7b
  final val iushr       = 0x7c
  final val lushr       = 0x7d
  final val iand        = 0x7e
  final val land        = 0x7f
  final val ior         = 0x80
  final val lor         = 0x81
  final val ixor        = 0x82
  final val lxor        = 0x83
  final val iinc        = 0x84

  final val i2l         = 0x85
  final val i2f         = 0x86
  final val i2d         = 0x87
  final val l2i         = 0x88
  final val l2f         = 0x89
  final val l2d         = 0x8a
  final val f2i         = 0x8b
  final val f2l         = 0x8c
  final val f2d         = 0x8d
  final val d2i         = 0x8e
  final val d2l         = 0x8f
  final val d2f         = 0x90
  final val i2b         = 0x91
  final val i2c         = 0x92
  final val i2s         = 0x93

  final val lcmp        = 0x94
  final val fcmpl       = 0x95
  final val fcmpg       = 0x96
  final val dcmpl       = 0x97
  final val dcmpg       = 0x98

  final val ifeq        = 0x99
  final val ifne        = 0x9a
  final val iflt        = 0x9b
  final val ifge        = 0x9c
  final val ifgt        = 0x9d
  final val ifle        = 0x9e
  final val if_icmpeq   = 0x9f
  final val if_icmpne   = 0xa0
  final val if_icmplt   = 0xa1
  final val if_icmpge   = 0xa2
  final val if_icmpgt   = 0xa3
  final val if_icmple   = 0xa4
  final val if_acmpeq   = 0xa5
  final val if_acmpne   = 0xa6
  final val goto        = 0xa7
  final val jsr         = 0xa8
  final val ret         = 0xa9
  final val tableswitch = 0xaa
  final val lookupswitch = 0xab
  final val ireturn     = 0xac
  final val lreturn     = 0xad
  final val freturn     = 0xae
  final val dreturn     = 0xaf
  final val areturn     = 0xb0
  final val return_     = 0xb1

  final val getstatic   = 0xb2
  final val putstatic   = 0xb3
  final val getfield    = 0xb4
  final val putfield    = 0xb5

  final val invokevirtual   = 0xb6
  final val invokespecial   = 0xb7
  final val invokestatic    = 0xb8
  final val invokeinterface = 0xb9
  final val invokedynamic   = 0xba

  final val new_          = 0xbb
  final val newarray      = 0xbc
  final val anewarray     = 0xbd
  final val arraylength   = 0xbe
  final val athrow        = 0xbf
  final val checkcast     = 0xc0
  final val instanceof    = 0xc1
  final val monitorenter  = 0xc2
  final val monitorexit   = 0xc3
  final val wide          = 0xc4
  final val multianewarray = 0xc5
  final val ifnull        = 0xc6
  final val ifnonnull     = 0xc7
  final val goto_w        = 0xc8
  final val jsr_w         = 0xc9

  // reserved opcodes
  final val breakpoint    = 0xca
  final val impdep1       = 0xfe
  final val impdep2       = 0xff

  abstract class FlagTranslation {
    import Flags._

    private def isAnnotation(flags: Int): Boolean = (flags & JAVA_ACC_ANNOTATION) != 0
    private def translateFlag(jflag: Int, isAnnotation: Boolean, isClass: Boolean): Long = (jflag: @switch) match {
      case JAVA_ACC_PRIVATE    => PRIVATE
      case JAVA_ACC_PROTECTED  => PROTECTED
      case JAVA_ACC_FINAL      => FINAL
      case JAVA_ACC_SYNTHETIC  => SYNTHETIC | ARTIFACT  // maybe should be just artifact?
      case JAVA_ACC_STATIC     => STATIC
      case JAVA_ACC_ABSTRACT   => if (isAnnotation) 0L else if (isClass) ABSTRACT else DEFERRED
      case JAVA_ACC_INTERFACE  => if (isAnnotation) 0L else TRAIT | INTERFACE | ABSTRACT
      case JAVA_ACC_ENUM       => JAVA_ENUM
      case JAVA_ACC_ANNOTATION => JAVA_ANNOTATION
      case _                   => 0L
    }
    private def translateFlags(jflags: Int, baseFlags: Long, isClass: Boolean): Long = {
      val isAnnot = isAnnotation(jflags)
      def translateFlag0(jflags: Int): Long = translateFlag(jflags, isAnnot, isClass)
      var res: Long = JAVA | baseFlags
      /* fast, elegant, maintainable, pick any two... */
      res |= translateFlag0(jflags & JAVA_ACC_PRIVATE)
      res |= translateFlag0(jflags & JAVA_ACC_PROTECTED)
      res |= translateFlag0(jflags & JAVA_ACC_FINAL)
      res |= translateFlag0(jflags & JAVA_ACC_SYNTHETIC)
      res |= translateFlag0(jflags & JAVA_ACC_STATIC)
      res |= translateFlag0(jflags & JAVA_ACC_ABSTRACT)
      res |= translateFlag0(jflags & JAVA_ACC_INTERFACE)
      res |= translateFlag0(jflags & JAVA_ACC_ENUM)
      res |= translateFlag0(jflags & JAVA_ACC_ANNOTATION)
      res
    }

    def classFlags(jflags: Int): Long = {
      translateFlags(jflags, 0, isClass = true)
    }
    def fieldFlags(jflags: Int): Long = {
      translateFlags(jflags, if ((jflags & JAVA_ACC_FINAL) == 0) MUTABLE else 0 , isClass = false)
    }
    def methodFlags(jflags: Int): Long = {
      translateFlags(jflags, if ((jflags & JAVA_ACC_BRIDGE) != 0) BRIDGE | ARTIFACT else 0, isClass = false)
    }
  }
  object FlagTranslation extends FlagTranslation { }

  def toScalaMethodFlags(flags: Int): Long = FlagTranslation methodFlags flags
  def toScalaClassFlags(flags: Int): Long  = FlagTranslation classFlags flags
  def toScalaFieldFlags(flags: Int): Long  = FlagTranslation fieldFlags flags
}
