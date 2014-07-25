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
      case ClassBType(internalName) => "L" + internalName + ";"
      case ArrayBType(component)    => "[" + component
      case MethodBType(args, res)   => "(" + args.mkString + ")" + res
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
      case ClassBType(internalName) => asm.Type.getObjectType(internalName) // see (*) above
      case a: ArrayBType            => asm.Type.getObjectType(a.descriptor)
      case m: MethodBType           => asm.Type.getMethodType(m.descriptor)
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
      case ClassBType(internalName) => internalName
      case a: ArrayBType            => a.descriptor
    }
  }

  /**
   * InnerClass and EnclosingMethod attributes (EnclosingMethod is displayed as OUTERCLASS in asm).
   *
   * In this summary, "class" means "class or interface".
   *
   * JLS: http://docs.oracle.com/javase/specs/jls/se8/html/index.html
   * JVMS: http://docs.oracle.com/javase/specs/jvms/se8/html/index.html
   *
   * Terminology
   * -----------
   *
   *  - Nested class (JLS 8): class whose declaration occurs within the body of another class
   *
   *  - Top-level class (JLS 8): non-nested class
   *
   *  - Inner class (JLS 8.1.3): nested class that is not (explicitly or implicitly) static
   *
   *  - Member class (JLS 8.5): class directly enclosed in the body of a class (and not, for
   *    example, defined in a method). Member classes cannot be anonymous. May be static.
   *
   *  - Local class (JLS 14.3): nested, non-anonymous class that is not a member of a class
   *    - cannot be static (therefore they are "inner" classes)
   *    - can be defined in a method, a constructor or in an initializer block
   *
   *  - Initializer block (JLS 8.6 / 8.7): block of statements in a java class
   *    - static initializer: executed before constructor body
   *    - instance initializer: exectued when class is initialized (instance creation, static
   *      field access, ...)
   *
   *
   * InnerClass
   * ----------
   *
   * The JVMS 4.7.6 requires an entry for every class mentioned in a CONSTANT_Class_info in the
   * constant pool (CP) that is not a member of a package (JLS 7.1).
   *
   * The JLS 13.1, points 9. / 10. requires: a class must reference (in the CP)
   *  - its immediately enclosing class
   *  - all of its member classes
   *  - all local and anonymous classes that appear elsewhere (method, constructor, initializer
   *    block, field initializer)
   *
   * In a comment, the 4.7.6 spec says: this implies an entry in the InnerClass attribute for
   *  - All enclosing classes (except the outermost, which is top-level)
   *    - My comment: not sure how this is implied, below (*) a Java counter-example.
   *      In any case, the Java compiler seems to add all enclosing classes, even if they are not
   *      otherwise mentioned in the CP. So we should do the same.
   *  - All nested classes (including anonymous and local, but not transitively)
   *
   * Fields in the InnerClass entries:
   *  - inner class: the (nested) class C we are talking about
   *  - outer class: the class of which C is a member. Has to be null for non-members, i.e. for
   *                 local and anonymous classes.
   *  - inner name:  A string with the simple name of the inner class. Null for anonymous classes.
   *  - flags:       access property flags, details in JVMS, table in 4.7.6.
   *
   *
   * Note 1: when a nested class is present in the InnerClass attribute, all of its enclosing
   * classes have to be present as well (by the rules above). Example:
   *
   *   class Outer { class I1 { class I2 { } } }
   *   class User { Outer.I1.I2 foo() { } }
   *
   * The return type "Outer.I1.I2" puts "Outer$I1$I2" in the CP, therefore the class is added to the
   * InnerClass attribute. For this entry, the "outer class" field will be "Outer$I1". This in turn
   * adds "Outer$I1" to the CP, which requires adding that class to the InnerClass attribute.
   * (For local / anonymous classes this would not be the case, since the "outer class" attribute
   *  would be empty. However, no class (other than the enclosing class) can refer to them, as they
   *  have no name.)
   *
   * In the current implementation of the Scala compiler, when adding a class to the InnerClass
   * attribute, all of its enclosing classes will be added as well. Javac seems to do the same,
   * see (*).
   *
   *
   * Note 2: If a class name is mentioned only in a CONSTANT_Utf8_info, but not in a
   * CONSTANT_Class_info, the JVMS does not require an entry in the InnerClass attribute. However,
   * the Java compiler seems to add such classes anyway. For example, when using an annotation, the
   * annotation class is stored as a CONSTANT_Utf8_info in the CP:
   *
   *   @O.Ann void foo() { }
   *
   * adds "const #13 = Asciz LO$Ann;;" in the constant pool. The "RuntimeInvisibleAnnotations"
   * attribute refers to that constant pool entry. Even though there is no other reference to
   * `O.Ann`, the java compiler adds an entry for that class to the InnerClass attribute (which
   * entails adding a CONSTANT_Class_info for the class).
   *
   *
   *
   * EnclosingMethod
   * ---------------
   *
   * JVMS 4.7.7: the attribute must be present "if and only if it represents a local class
   * or an anonymous class" (i.e. not for member classes).
   *
   * Fields:
   *  - class:  the enclosing class
   *  - method: the enclosing method (or constructor). Null if the class is not enclosed by a
   *            method, i.e. for
   *             - local or anonymous classes defined in (static or non-static) initializer blocks
   *             - anonymous classes defined in initializer blocks or field initializers
   *
   *            Note: the field is required for anonymous classes defined within local variable
   *            initializers (within a method), Java example below (**).
   *
   *            Currently, the Scala compiler sets "method" to the class constructor for classes
   *            defined in initializer blocks or field initializers. This is probably OK, since the
   *            Scala compiler desugars these statements into to the primary constructor.
   *
   *
   * (*)
   *   public class Test {
   *     void foo() {
   *       class Foo1 {
   *         // constructor statement block
   *         {
   *           class Foo2 {
   *             class Foo3 { }
   *           }
   *         }
   *       }
   *     }
   *   }
   *
   * The class file Test$1Foo1$1Foo2$Foo3 has no reference to the class Test$1Foo1, however it
   * still contains an InnerClass attribute for Test$1Foo1.
   * Maybe this is just because the Java compiler follows the JVMS comment ("InnerClasses
   * information for each enclosing class").
   *
   *
   * (**)
   *   void foo() {
   *     // anonymous class defined in local variable initializer expression.
   *     Runnable x = true ? (new Runnable() {
   *       public void run() { return; }
   *     }) : null;
   *   }
   *
   * The EnclosingMethod attribute of the anonymous class mentions "foo" in the "method" field.
   *
   *
   * Java Compatibility
   * ------------------
   *
   * In the InnerClass entry for classes in top-level modules, the "outer class" is emitted as the
   * mirror class (or the existing companion class), i.e. C1 is nested in T (not T$).
   * For classes nested in a nested object, the "outer class" is the module class: C2 is nested in T$N$
   * object T {
   *   class C1
   *   object N { class C2 }
   * }
   *
   * Reason: java compat. It's a "best effort" "solution". If you want to use "C1" from Java, you
   * can write "T.C1", and the Java compiler will translate that to the classfile T$C1.
   *
   * If we would emit the "outer class" of C1 as "T$", then in Java you'd need to write "T$.C1"
   * because the java compiler looks at the InnerClass attribute to find if an inner class exists.
   * However, the Java compiler would then translate the '.' to '$' and you'd get the class name
   * "T$$C1". This class file obviously does not exist.
   *
   * Directly using the encoded class name "T$C1" in Java does not work: since the classfile
   * describes a nested class, the Java compiler hides it from the classpath and will report
   * "cannot find symbol T$C1". This means that the class T.N.C2 cannot be referenced from a
   * Java source file in any way.
   *
   *
   * STATIC flag
   * -----------
   *
   * Java: static nested classes have the "static" flag in the InnerClass attribute. This is not the
   * case for local classes defined within a static method, even though such classes, as they are
   * defined in a static context, don't take an "outer" instance.
   * Non-static nested classes (inner classes, including local classes defined in a non-static
   * method) take an "outer" instance on construction.
   *
   * Scala: Explicitouter adds an "outer" parameter to nested classes, except for classes defined
   * in a static context, i.e. when all outer classes are module classes.
   *   package p
   *   object O1 {
   *     class C1 // static
   *     object O2 {
   *       def f = {
   *         class C2 { // static
   *           class C3 // non-static, needs outer
   *         }
   *       }
   *     }
   *   }
   *
   * Int the InnerClass attribute, the `static` flag is added for all classes defined in a static
   * context, i.e. also for C2. This is different than in Java.
   *
   *
   * Mirror Classes
   * --------------
   *
   * TODO: innerclass attributes on mirror class, bean info class
   */

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
     * Construct a ClassBType from the (intenred) internal name of a class.
     *
     * @param internalName The internal name as a slice of the `chrs` array. The internal name does
     *                     not have the surrounding 'L' and ';'. Note that
     *                     `classSymbol.javaBinaryName` returns exactly such a name.
     */
    def this(internalName: BTypeName) = this(internalName.start, internalName.length)

    /**
     * Construct a ClassBType from the internal name of a class.
     *
     * @param internalName The internal name of a class has the form "java/lang/String", without the
     *                     surrounding 'L' and ';'.
     */
    def this(internalName: String) = this({
      assert(!(internalName.head == 'L' && internalName.last == ';'), s"Descriptor instead of internal name: $internalName")
      createNewName(internalName)
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
      case c: ClassBType => c.offset == this.offset && c.length == this.length
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
    def apply(internalName: BTypeName): ClassBType = new ClassBType(internalName)
    def apply(internalName: String): ClassBType = new ClassBType(internalName)

    /**
     * Pattern matching on a ClassBType extracts the `internalName` of the class.
     */
    def unapply(c: ClassBType): Option[String] =
      if (c == null) None
      else Some(c.internalName)
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
