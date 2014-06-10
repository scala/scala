package scala.tools.nsc
package backend.jvm

import scala.collection.immutable
import scala.annotation.switch
import scala.tools.asm
import asm.Opcodes
import scala.collection.mutable.ListBuffer

/**
 * BTypes is a backend component that defines the class BType, a number of basic instances and
 * some utilities.
 *
 * A BType is essentially an slice of the array `chrs` denoting the name of the type, and a field
 * denoting the kind (object, array, method, or one of the primitive types).
 *
 * BTypes depends on Global just because it re-uses hash-consing of Name. It would be cleaner to
 * create an interface for BTypeName and extend it in scala.reflect.internal.Names#Name, that
 * would simplify testing BTypes (no Global needed).
 */
abstract class BTypes[G <: Global](val __global_dont_use: G) {
  def chrs: Array[Char]

  /**
   * Interface for names stored in `chrs`
   */
  type BTypeName <: __global_dont_use.Name

  /**
   * Create a new name in `chrs`. Names are assumed to be hash-consed. Equality on BType will use
   * reference equality to compare the names.
   */
  def createNewName(s: String): BTypeName

  /*sealed*/ trait BType { // Not sealed for now due to SI-8546
    final override def toString: String = this match {
      case UNIT   => "V"
      case BOOL   => "Z"
      case CHAR   => "C"
      case BYTE   => "B"
      case SHORT  => "S"
      case INT    => "I"
      case FLOAT  => "F"
      case LONG   => "J"
      case DOUBLE => "D"
      case c @ ClassBType(_, _)   => "L" + c.internalName + ";"
      case ArrayBType(component)  => "[" + component
      case MethodBType(args, res) => "(" + args.mkString + ")" + res
    }

    /**
     * @return The Java descriptor of this type. Examples:
     *  - int: I
     *  - java.lang.String: Ljava/lang/String;
     *  - int[]: [I
     *  - Object m(String s, double d): (Ljava/lang/String;D)Ljava/lang/Object;
     */
    final def descriptor = toString

    /**
     * @return 0 for void, 2 for long and double, 1 otherwise
     */
    final def size: Int = this match {
      case UNIT => 0
      case LONG | DOUBLE => 2
      case _ => 1
    }

    final def isPrimitive: Boolean = this.isInstanceOf[PrimitiveBType]
    final def isRef: Boolean       = this.isInstanceOf[RefBType]
    final def isArray: Boolean     = this.isInstanceOf[ArrayBType]
    final def isClass: Boolean     = this.isInstanceOf[ClassBType]
    final def isMethod: Boolean    = this.isInstanceOf[MethodBType]

    final def isNonVoidPrimitiveType = isPrimitive && this != UNIT
    // TODO @lry should also include !isMethod in isNonSpecial? in this case it would be equivalent to isClass, so we could get rid of it.
    final def isNonSpecial           = !isPrimitive && !isArray && !isPhantomType
    final def isNullType             = this == RT_NULL || this == CT_NULL
    final def isNothingType          = this == RT_NOTHING || this == CT_NOTHING
    final def isPhantomType          = isNullType || isNothingType

    final def isBoxed = this match {
      case BOXED_UNIT  | BOXED_BOOLEAN | BOXED_CHAR   |
           BOXED_BYTE  | BOXED_SHORT   | BOXED_INT    |
           BOXED_FLOAT | BOXED_LONG    | BOXED_DOUBLE => true
      case _ => false
    }

    final def isIntSizedType = this == BOOL || this == CHAR || this == BYTE ||
                               this == SHORT || this == INT
    final def isIntegralType = this == INT || this == BYTE || this == LONG ||
                               this == CHAR || this == SHORT
    final def isRealType     = this == FLOAT || this == DOUBLE
    final def isNumericType  = isIntegralType || isRealType
    final def isWideType     = size == 2

    /**
     * See documentation of [[typedOpcode]].
     * The numbers are taken from asm.Type.VOID_TYPE ff., the values are those shifted by << 8.
     */
    private def loadStoreOpcodeOffset: Int = this match {
      case UNIT | INT  => 0
      case BOOL | BYTE => 5
      case CHAR        => 6
      case SHORT       => 7
      case FLOAT       => 2
      case LONG        => 1
      case DOUBLE      => 3
      case _           => 4
    }

    /**
     * See documentation of [[typedOpcode]].
     * The numbers are taken from asm.Type.VOID_TYPE ff., the values are those shifted by << 16.
     */
    private def typedOpcodeOffset: Int = this match {
      case UNIT                               => 5
      case BOOL | CHAR | BYTE | SHORT | INT   => 0
      case FLOAT                              => 2
      case LONG                               => 1
      case DOUBLE                             => 3
      case _                                  => 4
    }

    /**
     * Some JVM opcodes have typed variants. This method returns the correct opcode according to
     * the type.
     *
     * @param opcode A JVM instruction opcode. This opcode must be one of ILOAD, ISTORE, IALOAD,
     *               IASTORE, IADD, ISUB, IMUL, IDIV, IREM, INEG, ISHL, ISHR, IUSHR, IAND, IOR
     *               IXOR and IRETURN.
     * @return The opcode adapted to this java type. For example, if this type is `float` and
     *         `opcode` is `IRETURN`, this method returns `FRETURN`.
     */
    final def typedOpcode(opcode: Int): Int = {
      if (opcode == Opcodes.IALOAD || opcode == Opcodes.IASTORE)
        opcode + loadStoreOpcodeOffset
      else
        opcode + typedOpcodeOffset
    }

    /**
     * The asm.Type corresponding to this BType.
     *
     * Note about asm.Type.getObjectType (*): For class types, the method expects the internal
     * name, i.e. without the surrounding 'L' and ';'. For array types on the other hand, the
     * method expects a full descriptor, for example "[Ljava/lang/String;".
     *
     * See method asm.Type.getType that creates a asm.Type from a type descriptor
     *  - for an OBJECT type, the 'L' and ';' are not part of the range of the created Type
     *  - for an ARRAY type, the full descriptor is part of the range
     */
    def toASMType: asm.Type = this match {
      case UNIT   => asm.Type.VOID_TYPE
      case BOOL   => asm.Type.BOOLEAN_TYPE
      case CHAR   => asm.Type.CHAR_TYPE
      case BYTE   => asm.Type.BYTE_TYPE
      case SHORT  => asm.Type.SHORT_TYPE
      case INT    => asm.Type.INT_TYPE
      case FLOAT  => asm.Type.FLOAT_TYPE
      case LONG   => asm.Type.LONG_TYPE
      case DOUBLE => asm.Type.DOUBLE_TYPE
      case c @ ClassBType(_, _)  => asm.Type.getObjectType(c.internalName) // (*)
      case a @ ArrayBType(_)     => asm.Type.getObjectType(a.descriptor)
      case m @ MethodBType(_, _) => asm.Type.getMethodType(m.descriptor)
    }

    def asRefBType  : RefBType   = this.asInstanceOf[RefBType]
    def asArrayBType: ArrayBType = this.asInstanceOf[ArrayBType]
    def asClassBType: ClassBType = this.asInstanceOf[ClassBType]
  }

  object BType {
    /**
     * @param chars The character array containing the descriptor
     * @param start The position where the descriptor starts
     * @return The BType and the index of the first character after the consumed descriptor
     */
    private[BTypes] def fromNonMethodDescriptor(chars: Array[Char], start: Int): (BType, Int) = {
      chars(start) match {
        case 'L' =>
          var i = start
          while (chars(i) != ';') { i += 1 }
          // Example: chars = "IILpkg/Cls;I"
          //                     ^       ^
          //                  start=2   i=10
          // `start + 1` to exclude the 'L', `i - start - 1` excludes the ';'
          (new ClassBType(new String(chars, start + 1, i - start - 1)), i + 1)
        case '[' =>
          val (res, next) = fromNonMethodDescriptor(chars, start + 1)
          (ArrayBType(res), next)
        case 'V' => (UNIT,   start + 1)
        case 'Z' => (BOOL,   start + 1)
        case 'C' => (CHAR,   start + 1)
        case 'B' => (BYTE,   start + 1)
        case 'S' => (SHORT,  start + 1)
        case 'I' => (INT,    start + 1)
        case 'F' => (FLOAT,  start + 1)
        case 'J' => (LONG,   start + 1)
        case 'D' => (DOUBLE, start + 1)
      }
    }
  }

  sealed trait PrimitiveBType extends BType

  case object UNIT   extends PrimitiveBType
  case object BOOL   extends PrimitiveBType
  case object CHAR   extends PrimitiveBType
  case object BYTE   extends PrimitiveBType
  case object SHORT  extends PrimitiveBType
  case object INT    extends PrimitiveBType
  case object FLOAT  extends PrimitiveBType
  case object LONG   extends PrimitiveBType
  case object DOUBLE extends PrimitiveBType

  sealed trait RefBType extends BType {
    /**
     * The class or array type of this reference type. Used for ANEWARRAY, MULTIANEWARRAY,
     * INSTANCEOF and CHECKCAST instructions. Also used for emitting invokevirtual calls to
     * (a: Array[T]).clone() for any T, see genApply.
     *
     * In contrast to the descriptor, this string does not contain the surrounding 'L' and ';' for
     * class types, for example "java/lang/String".
     * However, for array types, the full descriptor is used, for example "[Ljava/lang/String;".
     *
     * This can be verified for example using javap or ASMifier.
     */
    def classOrArrayType: String = this match {
      case c: ClassBType => c.internalName
      case a: ArrayBType => a.descriptor
    }
  }

  /**
   * Class or Interface type.
   *
   * The information for creating a ClassBType (superClass, interfaces, etc) is obtained
   *   - either from a ClassSymbol, for classes being compiled or referenced from source (see
   *     BCodeTypes)
   *   - or, during inlining, from ASM ClassNodes that are parsed from class files.
   *
   * The class name is represented as a slice of the `chrs` array. This representation is efficient
   * because the JVM class name is obtained through `classSymbol.javaBinaryName`. This already adds
   * the necessary string to the `chrs` array, so it makes sense to reuse the same name table in the
   * backend.
   *
   * Not a case class because that would expose the constructor that takes (offset, length)
   * parameters (I didn't find a way to make it private, also the factory in the companion).
   *
   * @param offset     See below
   * @param length     The class name is represented as offset and length in the `chrs` array.
   *                   The (public) constructors of ClassBType take a BTypeName, which are
   *                   hash-consed. This ensures that two ClassBType instances for the same name
   *                   have the same offset and length.
   *
   * Not a case class because that would expose the (Int, Int) constructor (didn't find a way to
   * make it private, also the factory in the companion).
   */
  class ClassBType private(val offset: Int, val length: Int) extends RefBType {
    /**
     * Construct a ClassBType for a given (intenred) class name.
     *
     * @param n The class name as a slice of the `chrs` array, without the surrounding 'L' and ';'.
     *          Note that `classSymbol.javaBinaryName` returns exactly such a name.
     */
    def this(n: BTypeName) = this(n.start, n.length)

    /**
     * Construct a ClassBType for a given java class name.
     *
     * @param s A class name of the form "java/lang/String", without the surrounding 'L' and ';'.
     */
    def this(s: String) = this({
      assert(!(s.head == 'L' && s.last == ';'), s"Descriptor instead of internal name: $s")
      createNewName(s)
    })

    /**
     * The internal name of a class is the string returned by java.lang.Class.getName, with all '.'
     * replaced by '/'. For example "java/lang/String".
     */
    def internalName: String = new String(chrs, offset, length)

    /**
     * @return The class name without the package prefix
     */
    def simpleName: String = internalName.split("/").last

    /**
     * Custom equals / hashCode are needed because this is not a case class.
     */
    override def equals(o: Any): Boolean = (this eq o.asInstanceOf[Object]) || (o match {
      case ClassBType(`offset`, `length`) => true
      case _ => false
    })

    override def hashCode: Int = {
      import scala.runtime.Statics
      var acc: Int = -889275714
      acc = Statics.mix(acc, offset)
      acc = Statics.mix(acc, length)
      Statics.finalizeHash(acc, 2)
    }
  }

  object ClassBType {
    def apply(n: BTypeName): ClassBType = new ClassBType(n)
    def apply(s: String): ClassBType = new ClassBType(s)

    def unapply(c: ClassBType): Option[(Int, Int)] =
      if (c == null) None
      else Some((c.offset, c.length))
  }

  case class ArrayBType(componentType: BType) extends RefBType {
    def dimension: Int = componentType match {
      case a: ArrayBType => 1 + a.dimension
      case _ => 1
    }

    def elementType: BType = componentType match {
      case a: ArrayBType => a.elementType
      case t => t
    }
  }

  case class MethodBType(argumentTypes: List[BType], returnType: BType) extends BType {
    private def this(types: (List[BType], BType)) = this(types._1, types._2)
    def this(descriptor: String) = this(MethodBType.decomposeMethodDescriptor(descriptor))
  }

  object MethodBType {
    private def decomposeMethodDescriptor(descriptor: String): (List[BType], BType) = {
      val chars = descriptor.toCharArray
      assert(chars(0) == '(', s"Not a valid method descriptor: $descriptor")
      var i = 1
      val argTypes = new ListBuffer[BType]
      while (chars(i) != ')') {
        val (argType, next) = BType.fromNonMethodDescriptor(chars, i)
        argTypes += argType
        i = next
      }
      val (resType, _) = BType.fromNonMethodDescriptor(chars, i + 1) // `i + 1` to skip the ')'
      (argTypes.toList, resType)
    }
    def apply(descriptor: String) = {
      val (argTypes, resType) = decomposeMethodDescriptor(descriptor)
      new MethodBType(argTypes, resType)
    }
  }

  val BOXED_UNIT    = ClassBType("java/lang/Void")
  val BOXED_BOOLEAN = ClassBType("java/lang/Boolean")
  val BOXED_BYTE    = ClassBType("java/lang/Byte")
  val BOXED_SHORT   = ClassBType("java/lang/Short")
  val BOXED_CHAR    = ClassBType("java/lang/Character")
  val BOXED_INT     = ClassBType("java/lang/Integer")
  val BOXED_LONG    = ClassBType("java/lang/Long")
  val BOXED_FLOAT   = ClassBType("java/lang/Float")
  val BOXED_DOUBLE  = ClassBType("java/lang/Double")

  /*
   * RT_NOTHING and RT_NULL exist at run-time only. They are the bytecode-level manifestation (in
   * method signatures only) of what shows up as NothingClass resp. NullClass in Scala ASTs.
   *
   * Therefore, when RT_NOTHING or RT_NULL are to be emitted, a mapping is needed: the internal
   * names of NothingClass and NullClass can't be emitted as-is.
   */
  val RT_NOTHING = ClassBType("scala/runtime/Nothing$")
  val RT_NULL    = ClassBType("scala/runtime/Null$")
  val CT_NOTHING = ClassBType("scala/Nothing")
  val CT_NULL    = ClassBType("scala/Null")

  val srBooleanRef = ClassBType("scala/runtime/BooleanRef")
  val srByteRef    = ClassBType("scala/runtime/ByteRef")
  val srCharRef    = ClassBType("scala/runtime/CharRef")
  val srIntRef     = ClassBType("scala/runtime/IntRef")
  val srLongRef    = ClassBType("scala/runtime/LongRef")
  val srFloatRef   = ClassBType("scala/runtime/FloatRef")
  val srDoubleRef  = ClassBType("scala/runtime/DoubleRef")

  /**
   * Map from type kinds to the Java reference types.
   * Useful when pushing class literals onto the operand stack (ldc instruction taking a class
   * literal).
   * @see Predef.classOf
   * @see genConstant()
   *
   * TODO @lry rename to "boxedClassOfPrimitive" or so, check usages
   */
  val classLiteral = immutable.Map[BType, ClassBType](
    UNIT   -> BOXED_UNIT,
    BOOL   -> BOXED_BOOLEAN,
    BYTE   -> BOXED_BYTE,
    SHORT  -> BOXED_SHORT,
    CHAR   -> BOXED_CHAR,
    INT    -> BOXED_INT,
    LONG   -> BOXED_LONG,
    FLOAT  -> BOXED_FLOAT,
    DOUBLE -> BOXED_DOUBLE
  )

  case class MethodNameAndType(name: String, descriptor: String)

  val asmBoxTo: immutable.Map[BType, MethodNameAndType] = {
    Map(
      BOOL   -> MethodNameAndType("boxToBoolean",   "(Z)Ljava/lang/Boolean;"  ) ,
      BYTE   -> MethodNameAndType("boxToByte",      "(B)Ljava/lang/Byte;"     ) ,
      CHAR   -> MethodNameAndType("boxToCharacter", "(C)Ljava/lang/Character;") ,
      SHORT  -> MethodNameAndType("boxToShort",     "(S)Ljava/lang/Short;"    ) ,
      INT    -> MethodNameAndType("boxToInteger",   "(I)Ljava/lang/Integer;"  ) ,
      LONG   -> MethodNameAndType("boxToLong",      "(J)Ljava/lang/Long;"     ) ,
      FLOAT  -> MethodNameAndType("boxToFloat",     "(F)Ljava/lang/Float;"    ) ,
      DOUBLE -> MethodNameAndType("boxToDouble",    "(D)Ljava/lang/Double;"   )
    )
  }

  val asmUnboxTo: immutable.Map[BType, MethodNameAndType] = {
    Map(
      BOOL   -> MethodNameAndType("unboxToBoolean", "(Ljava/lang/Object;)Z") ,
      BYTE   -> MethodNameAndType("unboxToByte",    "(Ljava/lang/Object;)B") ,
      CHAR   -> MethodNameAndType("unboxToChar",    "(Ljava/lang/Object;)C") ,
      SHORT  -> MethodNameAndType("unboxToShort",   "(Ljava/lang/Object;)S") ,
      INT    -> MethodNameAndType("unboxToInt",     "(Ljava/lang/Object;)I") ,
      LONG   -> MethodNameAndType("unboxToLong",    "(Ljava/lang/Object;)J") ,
      FLOAT  -> MethodNameAndType("unboxToFloat",   "(Ljava/lang/Object;)F") ,
      DOUBLE -> MethodNameAndType("unboxToDouble",  "(Ljava/lang/Object;)D")
    )
  }
}
