/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Iulian Dragos
 */

package scala.tools.nsc
package backend.jvm

import scala.collection.{ mutable, immutable }
import ch.epfl.lamp.fjbg._

trait GenJVMUtil {
  self: GenJVM =>

  import global._
  import icodes._
  import icodes.opcodes._
  import definitions._

  /** Map from type kinds to the Java reference types. It is used for
   *  loading class constants. @see Predef.classOf.
   */
  val classLiteral = immutable.Map[TypeKind, JObjectType](
    UNIT   -> new JObjectType("java.lang.Void"),
    BOOL   -> new JObjectType("java.lang.Boolean"),
    BYTE   -> new JObjectType("java.lang.Byte"),
    SHORT  -> new JObjectType("java.lang.Short"),
    CHAR   -> new JObjectType("java.lang.Character"),
    INT    -> new JObjectType("java.lang.Integer"),
    LONG   -> new JObjectType("java.lang.Long"),
    FLOAT  -> new JObjectType("java.lang.Float"),
    DOUBLE -> new JObjectType("java.lang.Double")
  )

  // Don't put this in per run caches.
  private val javaNameCache = new mutable.WeakHashMap[Symbol, Name]() ++= List(
    NothingClass        -> binarynme.RuntimeNothing,
    RuntimeNothingClass -> binarynme.RuntimeNothing,
    NullClass           -> binarynme.RuntimeNull,
    RuntimeNullClass    -> binarynme.RuntimeNull
  )

  /** This trait may be used by tools who need access to
   *  utility methods like javaName and javaType. (for instance,
   *  the Eclipse plugin uses it).
   */
  trait BytecodeUtil {

    val conds = immutable.Map[TestOp, Int](
      EQ -> JExtendedCode.COND_EQ,
      NE -> JExtendedCode.COND_NE,
      LT -> JExtendedCode.COND_LT,
      GT -> JExtendedCode.COND_GT,
      LE -> JExtendedCode.COND_LE,
      GE -> JExtendedCode.COND_GE
    )

    /** Specialized array conversion to prevent calling
     *  java.lang.reflect.Array.newInstance via TraversableOnce.toArray
     */

    def mkArray(xs: Traversable[JType]): Array[JType] = { val a = new Array[JType](xs.size); xs.copyToArray(a); a }
    def mkArray(xs: Traversable[String]): Array[String] = { val a = new Array[String](xs.size); xs.copyToArray(a); a }

    /** Return the a name of this symbol that can be used on the Java
     *  platform.  It removes spaces from names.
     *
     *  Special handling:
     *    scala.Nothing erases to scala.runtime.Nothing$
     *       scala.Null erases to scala.runtime.Null$
     *
     *  This is needed because they are not real classes, and they mean
     *  'abrupt termination upon evaluation of that expression' or null respectively.
     *  This handling is done already in GenICode, but here we need to remove
     *  references from method signatures to these types, because such classes can
     *  not exist in the classpath: the type checker will be very confused.
     */
    def javaName(sym: Symbol): String =
      javaNameCache.getOrElseUpdate(sym, {
        if (sym.isClass || (sym.isModule && !sym.isMethod))
          sym.javaBinaryName
        else
          sym.javaSimpleName
      }).toString

    def javaType(t: TypeKind): JType = (t: @unchecked) match {
      case UNIT            => JType.VOID
      case BOOL            => JType.BOOLEAN
      case BYTE            => JType.BYTE
      case SHORT           => JType.SHORT
      case CHAR            => JType.CHAR
      case INT             => JType.INT
      case LONG            => JType.LONG
      case FLOAT           => JType.FLOAT
      case DOUBLE          => JType.DOUBLE
      case REFERENCE(cls)  => new JObjectType(javaName(cls))
      case ARRAY(elem)     => new JArrayType(javaType(elem))
    }

    def javaType(t: Type): JType = javaType(toTypeKind(t))

    def javaType(s: Symbol): JType =
      if (s.isMethod)
        new JMethodType(
          if (s.isClassConstructor) JType.VOID else javaType(s.tpe.resultType),
          mkArray(s.tpe.paramTypes map javaType)
        )
      else
        javaType(s.tpe)

    protected def genConstant(jcode: JExtendedCode, const: Constant) {
      const.tag match {
        case UnitTag    => ()
        case BooleanTag => jcode emitPUSH const.booleanValue
        case ByteTag    => jcode emitPUSH const.byteValue
        case ShortTag   => jcode emitPUSH const.shortValue
        case CharTag    => jcode emitPUSH const.charValue
        case IntTag     => jcode emitPUSH const.intValue
        case LongTag    => jcode emitPUSH const.longValue
        case FloatTag   => jcode emitPUSH const.floatValue
        case DoubleTag  => jcode emitPUSH const.doubleValue
        case StringTag  => jcode emitPUSH const.stringValue
        case NullTag    => jcode.emitACONST_NULL()
        case ClazzTag   =>
          val kind = toTypeKind(const.typeValue)
          val toPush =
            if (kind.isValueType) classLiteral(kind)
            else javaType(kind).asInstanceOf[JReferenceType]

          jcode emitPUSH toPush

        case EnumTag   =>
          val sym = const.symbolValue
          jcode.emitGETSTATIC(javaName(sym.owner),
                              javaName(sym),
                              javaType(sym.tpe.underlying))
        case _         =>
          abort("Unknown constant value: " + const)
      }
    }
  }
}
