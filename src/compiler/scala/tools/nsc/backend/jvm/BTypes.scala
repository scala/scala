/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm

import scala.annotation.switch
import scala.collection.concurrent.TrieMap
import scala.reflect.internal.util.Position
import scala.tools.asm
import asm.Opcodes
import scala.tools.asm.tree.{MethodNode, MethodInsnNode, InnerClassNode, ClassNode}
import scala.tools.nsc.backend.jvm.BTypes.{InlineInfo, MethodInlineInfo}
import scala.tools.nsc.backend.jvm.BackendReporting._
import scala.tools.nsc.backend.jvm.opt._
import scala.collection.convert.decorateAsScala._
import scala.tools.nsc.settings.ScalaSettings

/**
 * The BTypes component defines The BType class hierarchy. A BType stores all type information
 * that is required after building the ASM nodes. This includes optimizations, generation of
 * InnerClass attributes and generation of stack map frames.
 *
 * The representation is immutable and independent of the compiler data structures, hence it can
 * be queried by concurrent threads.
 */
abstract class BTypes {
  import BTypes.InternalName

  // Some core BTypes are required here, in class BType, where no Global instance is available.
  // The Global is only available in the subclass BTypesFromSymbols. We cannot depend on the actual
  // implementation (CoreBTypesProxy) here because it has members that refer to global.Symbol.
  val coreBTypes: CoreBTypesProxyGlobalIndependent[this.type]
  import coreBTypes._

  /**
   * Tools for parsing classfiles, used by the inliner.
   */
  val byteCodeRepository: ByteCodeRepository

  val localOpt: LocalOpt[this.type]

  val inliner: Inliner[this.type]

  val closureOptimizer: ClosureOptimizer[this.type]

  val callGraph: CallGraph[this.type]

  val backendReporting: BackendReporting

  // Allows to define per-run caches here and in the CallGraph component, which don't have a global
  def recordPerRunCache[T <: collection.generic.Clearable](cache: T): T

  // Allows access to the compiler settings for backend components that don't have a global in scope
  def compilerSettings: ScalaSettings


  /**
   * A map from internal names to ClassBTypes. Every ClassBType is added to this map on its
   * construction.
   *
   * This map is used when computing stack map frames. The asm.ClassWriter invokes the method
   * `getCommonSuperClass`. In this method we need to obtain the ClassBType for a given internal
   * name. The method assumes that every class type that appears in the bytecode exists in the map.
   *
   * Concurrent because stack map frames are computed when in the class writer, which might run
   * on multiple classes concurrently.
   */
  val classBTypeFromInternalName: collection.concurrent.Map[InternalName, ClassBType] = recordPerRunCache(TrieMap.empty)

  /**
   * Store the position of every MethodInsnNode during code generation. This allows each callsite
   * in the call graph to remember its source position, which is required for inliner warnings.
   */
  val callsitePositions: collection.concurrent.Map[MethodInsnNode, Position] = recordPerRunCache(TrieMap.empty)

  /**
   * Contains the internal names of all classes that are defined in Java source files of the current
   * compilation run (mixed compilation). Used for more detailed error reporting.
   */
  val javaDefinedClasses: collection.mutable.Set[InternalName] = recordPerRunCache(collection.mutable.Set.empty)

  /**
   * Cache, contains methods whose unreachable instructions are eliminated.
   *
   * The ASM Analyzer class does not compute any frame information for unreachable instructions.
   * Transformations that use an analyzer (including inlining) therefore require unreachable code
   * to be eliminated.
   *
   * This cache allows running dead code elimination whenever an analyzer is used. If the method
   * is already optimized, DCE can return early.
   */
  val unreachableCodeEliminated: collection.mutable.Set[MethodNode] = recordPerRunCache(collection.mutable.Set.empty)

  /**
   * Obtain the BType for a type descriptor or internal name. For class descriptors, the ClassBType
   * is constructed by parsing the corresponding classfile.
   * 
   * Some JVM operations use either a full descriptor or only an internal name. Example:
   *   ANEWARRAY java/lang/String    // a new array of strings (internal name for the String class)
   *   ANEWARRAY [Ljava/lang/String; // a new array of array of string (full descriptor for the String class)
   *
   * This method supports both descriptors and internal names.
   */
  def bTypeForDescriptorOrInternalNameFromClassfile(desc: String): BType = (desc(0): @switch) match {
    case 'V'                     => UNIT
    case 'Z'                     => BOOL
    case 'C'                     => CHAR
    case 'B'                     => BYTE
    case 'S'                     => SHORT
    case 'I'                     => INT
    case 'F'                     => FLOAT
    case 'J'                     => LONG
    case 'D'                     => DOUBLE
    case '['                     => ArrayBType(bTypeForDescriptorOrInternalNameFromClassfile(desc.substring(1)))
    case 'L' if desc.last == ';' => classBTypeFromParsedClassfile(desc.substring(1, desc.length - 1))
    case _                       => classBTypeFromParsedClassfile(desc)
  }

  /**
   * Parse the classfile for `internalName` and construct the [[ClassBType]]. If the classfile cannot
   * be found in the `byteCodeRepository`, the `info` of the resulting ClassBType is undefined.
   */
  def classBTypeFromParsedClassfile(internalName: InternalName): ClassBType = {
    classBTypeFromInternalName.getOrElse(internalName, {
      val res = ClassBType(internalName)
      byteCodeRepository.classNode(internalName) match {
        case Left(msg) => res.info = Left(NoClassBTypeInfoMissingBytecode(msg)); res
        case Right(c)  => setClassInfoFromParsedClassfile(c, res)
      }
    })
  }

  /**
   * Construct the [[ClassBType]] for a parsed classfile.
   */
  def classBTypeFromClassNode(classNode: ClassNode): ClassBType = {
    classBTypeFromInternalName.getOrElse(classNode.name, {
      setClassInfoFromParsedClassfile(classNode, ClassBType(classNode.name))
    })
  }

  private def setClassInfoFromParsedClassfile(classNode: ClassNode, classBType: ClassBType): ClassBType = {
    val superClass = classNode.superName match {
      case null =>
        assert(classNode.name == ObjectReference.internalName, s"class with missing super type: ${classNode.name}")
        None
      case superName =>
        Some(classBTypeFromParsedClassfile(superName))
    }

    val interfaces: List[ClassBType] = classNode.interfaces.asScala.map(classBTypeFromParsedClassfile)(collection.breakOut)

    val flags = classNode.access

    /**
     * Find all nested classes of classNode. The innerClasses attribute contains all nested classes
     * that are declared inside classNode or used in the bytecode of classNode. So some of them are
     * nested in some other class than classNode, and we need to filter them.
     *
     * For member classes, innerClassNode.outerName is defined, so we compare that to classNode.name.
     *
     * For local and anonymous classes, innerClassNode.outerName is null. Such classes are required
     * to have an EnclosingMethod attribute declaring the outer class. So we keep those local and
     * anonymous classes whose outerClass is classNode.name.
     */
    def nestedInCurrentClass(innerClassNode: InnerClassNode): Boolean = {
      (innerClassNode.outerName != null && innerClassNode.outerName == classNode.name) ||
      (innerClassNode.outerName == null && {
        val classNodeForInnerClass = byteCodeRepository.classNode(innerClassNode.name).get // TODO: don't get here, but set the info to Left at the end
        classNodeForInnerClass.outerClass == classNode.name
      })
    }

    val nestedClasses: List[ClassBType] = classNode.innerClasses.asScala.collect({
      case i if nestedInCurrentClass(i) => classBTypeFromParsedClassfile(i.name)
    })(collection.breakOut)

    // if classNode is a nested class, it has an innerClass attribute for itself. in this
    // case we build the NestedInfo.
    val nestedInfo = classNode.innerClasses.asScala.find(_.name == classNode.name) map {
      case innerEntry =>
        val enclosingClass =
          if (innerEntry.outerName != null) {
            // if classNode is a member class, the outerName is non-null
            classBTypeFromParsedClassfile(innerEntry.outerName)
          } else {
            // for anonymous or local classes, the outerName is null, but the enclosing class is
            // stored in the EnclosingMethod attribute (which ASM encodes in classNode.outerClass).
            classBTypeFromParsedClassfile(classNode.outerClass)
          }
        val staticFlag = (innerEntry.access & Opcodes.ACC_STATIC) != 0
        NestedInfo(enclosingClass, Option(innerEntry.outerName), Option(innerEntry.innerName), staticFlag)
    }

    val inlineInfo = inlineInfoFromClassfile(classNode)

    classBType.info = Right(ClassInfo(superClass, interfaces, flags, nestedClasses, nestedInfo, inlineInfo))
    classBType
  }

  /**
   * Build the InlineInfo for a class. For Scala classes, the information is stored in the
   * ScalaInlineInfo attribute. If the attribute is missing, the InlineInfo is built using the
   * metadata available in the classfile (ACC_FINAL flags, etc).
   */
  def inlineInfoFromClassfile(classNode: ClassNode): InlineInfo = {
    def fromClassfileAttribute: Option[InlineInfo] = {
      if (classNode.attrs == null) None
      else classNode.attrs.asScala.collect({ case a: InlineInfoAttribute => a}).headOption.map(_.inlineInfo)
    }

    def fromClassfileWithoutAttribute = {
      val warning = {
        val isScala = classNode.attrs != null && classNode.attrs.asScala.exists(a => a.`type` == BTypes.ScalaAttributeName || a.`type` == BTypes.ScalaSigAttributeName)
        if (isScala) Some(NoInlineInfoAttribute(classNode.name))
        else None
      }
      // when building MethodInlineInfos for the members of a ClassSymbol, we exclude those methods
      // in scalaPrimitives. This is necessary because some of them have non-erased types, which would
      // require special handling. Excluding is OK because they are never inlined.
      // Here we are parsing from a classfile and we don't need to do anything special. Many of these
      // primitives don't even exist, for example Any.isInstanceOf.
      val methodInfos = classNode.methods.asScala.map(methodNode => {
        val info = MethodInlineInfo(
          effectivelyFinal                    = BytecodeUtils.isFinalMethod(methodNode),
          traitMethodWithStaticImplementation = false,
          annotatedInline                     = false,
          annotatedNoInline                   = false)
        (methodNode.name + methodNode.desc, info)
      }).toMap
      InlineInfo(
        traitImplClassSelfType = None,
        isEffectivelyFinal = BytecodeUtils.isFinalClass(classNode),
        methodInfos = methodInfos,
        warning)
    }

    // The InlineInfo is built from the classfile (not from the symbol) for all classes that are NOT
    // being compiled. For those classes, the info is only needed if the inliner is enabled, othewise
    // we can save the memory.
    if (!compilerSettings.YoptInlinerEnabled) BTypes.EmptyInlineInfo
    else fromClassfileAttribute getOrElse fromClassfileWithoutAttribute
  }

  /**
   * A BType is either a primitive type, a ClassBType, an ArrayBType of one of these, or a MethodType
   * referring to BTypes.
   */
  sealed trait BType {
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

    final def isNullType             = this == RT_NULL
    final def isNothingType          = this == RT_NOTHING

    final def isBoxed = this.isClass && boxedClasses(this.asClassBType)

    final def isIntSizedType = this == BOOL || this == CHAR || this == BYTE ||
                               this == SHORT || this == INT
    final def isIntegralType = this == INT || this == BYTE || this == LONG ||
                               this == CHAR || this == SHORT
    final def isRealType     = this == FLOAT || this == DOUBLE
    final def isNumericType  = isIntegralType || isRealType
    final def isWideType     = size == 2

    /*
     * Subtype check `this <:< other` on BTypes that takes into account the JVM built-in numeric
     * promotions (e.g. BYTE to INT). Its operation can be visualized more easily in terms of the
     * Java bytecode type hierarchy.
     */
    final def conformsTo(other: BType): Either[NoClassBTypeInfo, Boolean] = tryEither(Right({
      assert(isRef || isPrimitive, s"conformsTo cannot handle $this")
      assert(other.isRef || other.isPrimitive, s"conformsTo cannot handle $other")

      this match {
        case ArrayBType(component) =>
          if (other == ObjectReference || other == jlCloneableReference || other == jioSerializableReference) true
          else other match {
            case ArrayBType(otherComponent) => component.conformsTo(otherComponent).orThrow
            case _ => false
          }

        case classType: ClassBType =>
          if (isBoxed) {
            if (other.isBoxed) this == other
            else if (other == ObjectReference) true
            else other match {
              case otherClassType: ClassBType => classType.isSubtypeOf(otherClassType).orThrow // e.g., java/lang/Double conforms to java/lang/Number
              case _ => false
            }
          } else if (isNullType) {
            if (other.isNothingType) false
            else if (other.isPrimitive) false
            else true // Null conforms to all classes (except Nothing) and arrays.
          } else if (isNothingType) {
            true
          } else other match {
            case otherClassType: ClassBType => classType.isSubtypeOf(otherClassType).orThrow
            // case ArrayBType(_) => this.isNullType   // documentation only, because `if (isNullType)` above covers this case
            case _ =>
              // isNothingType ||                      // documentation only, because `if (isNothingType)` above covers this case
              false
          }

        case UNIT =>
          other == UNIT
        case BOOL | BYTE | SHORT | CHAR =>
          this == other || other == INT || other == LONG // TODO Actually, BOOL does NOT conform to LONG. Even with adapt().
        case _ =>
          assert(isPrimitive && other.isPrimitive, s"Expected primitive types $this - $other")
          this == other
      }
    }))

    /**
     * Compute the upper bound of two types.
     * Takes promotions of numeric primitives into account.
     */
    final def maxType(other: BType): BType = this match {
      case pt: PrimitiveBType => pt.maxValueType(other)

      case _: ArrayBType | _: ClassBType =>
        if (isNothingType)       return other
        if (other.isNothingType) return this
        if (this == other)       return this

        assert(other.isRef, s"Cannot compute maxType: $this, $other")
        // Approximate `lub`. The common type of two references is always ObjectReference.
        ObjectReference

      case _: MethodBType =>
        assertionError(s"unexpected method type when computing maxType: $this")
    }

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

    def asRefBType       : RefBType       = this.asInstanceOf[RefBType]
    def asArrayBType     : ArrayBType     = this.asInstanceOf[ArrayBType]
    def asClassBType     : ClassBType     = this.asInstanceOf[ClassBType]
    def asPrimitiveBType : PrimitiveBType = this.asInstanceOf[PrimitiveBType]
  }

  sealed trait PrimitiveBType extends BType {

    /**
     * The upper bound of two primitive types. The `other` type has to be either a primitive
     * type or Nothing.
     *
     * The maxValueType of (Char, Byte) and of (Char, Short) is Int, to encompass the negative
     * values of Byte and Short. See ticket #2087.
     */
    final def maxValueType(other: BType): BType = {

      def uncomparable: Nothing = assertionError(s"Cannot compute maxValueType: $this, $other")

      if (!other.isPrimitive && !other.isNothingType) uncomparable

      if (other.isNothingType) return this
      if (this == other)       return this

      this match {
        case BYTE =>
          if (other == CHAR)            INT
          else if (other.isNumericType) other
          else                          uncomparable

        case SHORT =>
          other match {
            case BYTE                          => SHORT
            case CHAR                          => INT
            case INT  | LONG  | FLOAT | DOUBLE => other
            case _                             => uncomparable
          }

        case CHAR =>
          other match {
            case BYTE | SHORT                 => INT
            case INT  | LONG | FLOAT | DOUBLE => other
            case _                            => uncomparable
          }

        case INT =>
          other match {
            case BYTE | SHORT | CHAR   => INT
            case LONG | FLOAT | DOUBLE => other
            case _                     => uncomparable
          }

        case LONG =>
          if (other.isIntegralType)  LONG
          else if (other.isRealType) DOUBLE
          else                       uncomparable

        case FLOAT =>
          if (other == DOUBLE)          DOUBLE
          else if (other.isNumericType) FLOAT
          else                          uncomparable

        case DOUBLE =>
          if (other.isNumericType) DOUBLE
          else                     uncomparable

        case UNIT | BOOL => uncomparable
      }
    }
  }

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
   *    - instance initializer: executed when class is initialized (instance creation, static
   *      field access, ...)
   *
   *  - A static nested class can be defined as
   *    - a static member class (explicitly static), or
   *    - a member class of an interface (implicitly static)
   *    - local classes are never static, even if they are defined in a static method.
   *
   *   Note: it is NOT the case that all inner classes (non-static) have an outer pointer. Example:
   *     class C { static void foo { class D {} } }
   *   The class D is an inner class (non-static), but javac does not add an outer pointer to it.
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
   *  - all local and anonymous classes that are referenced (or declared) elsewhere (method,
   *    constructor, initializer block, field initializer)
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
   *                 local and anonymous classes. NOTE: this co-incides with the presence of an
   *                 EnclosingMethod attribute (see below)
   *  - inner name:  A string with the simple name of the inner class. Null for anonymous classes.
   *  - flags:       access property flags, details in JVMS, table in 4.7.6. Static flag: see
   *                 discussion below.
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
   * The attribute is misnamed, it should be called "EnclosingClass". It has to be defined for all
   * local and anonymous classes, no matter if there is an enclosing method or not. Accordingly, the
   * "class" field (see below) must be always defined, while the "method" field may be null.
   *
   * NOTE: When an EnclosingMethod attribute is requried (local and anonymous classes), the "outer"
   * field in the InnerClass table must be null.
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
   *            For local and anonymous classes in initializer blocks or field initializers, and
   *            class-level anonymous classes, the scala compiler sets the "method" field to null.
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
   * Java: static member classes have the static flag in the InnerClass attribute, for example B in
   *   class A { static class B { } }
   *
   * The spec is not very clear about when the static flag should be emitted. It says: "Marked or
   * implicitly static in source."
   *
   * The presence of the static flag does NOT coincide with the absence of an "outer" field in the
   * class. The java compiler never puts the static flag for local classes, even if they don't have
   * an outer pointer:
   *
   *   class A {
   *     void f()        { class B {} }
   *     static void g() { class C {} }
   *   }
   *
   * B has an outer pointer, C doesn't. Both B and C are NOT marked static in the InnerClass table.
   *
   * It seems sane to follow the same principle in the Scala compiler. So:
   *
   *   package p
   *   object O1 {
   *     class C1 // static inner class
   *     object O2 { // static inner module
   *       def f = {
   *         class C2 { // non-static inner class, even though there's no outer pointer
   *           class C3 // non-static, has an outer pointer
   *         }
   *       }
   *     }
   *   }
   *
   *
   * Traits Members
   * --------------
   *
   * Some trait methods don't exist in the generated interface, but only in the implementation class
   * (private methods in traits for example). Since EnclosingMethod expresses a source-level property,
   * but the source-level enclosing method doesn't exist in the classfile, we the enclosing method
   * is null (the enclosing class is still emitted).
   * See BCodeAsmCommon.considerAsTopLevelImplementationArtifact
   *
   *
   * Implementation Classes, Specialized Classes, Delambdafy:method closure classes
   * ------------------------------------------------------------------------------
   *
   * Trait implementation classes and specialized classes are always considered top-level. Again,
   * the InnerClass / EnclosingMethod attributes describe a source-level properties. The impl
   * classes are compilation artifacts.
   *
   * The same is true for delambdafy:method closure classes. These classes are generated at
   * top-level in the delambdafy phase, no special support is required in the backend.
   *
   *
   * Mirror Classes
   * --------------
   *
   * TODO: innerclass attributes on mirror class, bean info class
   */

  /**
   * A ClassBType represents a class or interface type. The necessary information to build a
   * ClassBType is extracted from compiler symbols and types, see BTypesFromSymbols.
   *
   * The `info` field contains either the class information on an error message why the info could
   * not be computed. There are two reasons for an erroneous info:
   *   1. The ClassBType was built from a class symbol that stems from a java source file, and the
   *      symbol's type could not be completed successfully (SI-9111)
   *   2. The ClassBType should be built from a classfile, but the class could not be found on the
   *      compilation classpath.
   *
   * Note that all ClassBTypes required in a non-optimized run are built during code generation from
   * the class symbols referenced by the ASTs, so they have a valid info. Therefore the backend
   * often invokes `info.get` (which asserts the info to exist) when reading data from the ClassBType.
   *
   * The inliner on the other hand uses ClassBTypes that are built from classfiles, which may have
   * a missing info. In order not to crash the compiler unnecessarily, the inliner does not force
   * infos using `get`, but it reports inliner warnings for missing infos that prevent inlining.
   */
  final case class ClassBType(internalName: InternalName) extends RefBType {
    /**
     * Write-once variable allows initializing a cyclic graph of infos. This is required for
     * nested classes. Example: for the definition `class A { class B }` we have
     *
     *   B.info.nestedInfo.outerClass == A
     *   A.info.nestedClasses contains B
     */
    private var _info: Either[NoClassBTypeInfo, ClassInfo] = null

    def info: Either[NoClassBTypeInfo, ClassInfo] = {
      assert(_info != null, s"ClassBType.info not yet assigned: $this")
      _info
    }

    def info_=(i: Either[NoClassBTypeInfo, ClassInfo]): Unit = {
      assert(_info == null, s"Cannot set ClassBType.info multiple times: $this")
      _info = i
      checkInfoConsistency()
    }

    classBTypeFromInternalName(internalName) = this

    private def checkInfoConsistency(): Unit = {
      if (info.isLeft) return

      // we assert some properties. however, some of the linked ClassBType (members, superClass,
      // interfaces) may not yet have an `_info` (initialization of cyclic structures). so we do a
      // best-effort verification. also we don't report an error if the info is a Left.
      def ifInit(c: ClassBType)(p: ClassBType => Boolean): Boolean = c._info == null || c.info.isLeft || p(c)

      def isJLO(t: ClassBType) = t.internalName == ObjectReference.internalName

      assert(!ClassBType.isInternalPhantomType(internalName), s"Cannot create ClassBType for phantom type $this")

      assert(
        if (info.get.superClass.isEmpty) { isJLO(this) || (isCompilingPrimitive && ClassBType.hasNoSuper(internalName)) }
        else if (isInterface.get) isJLO(info.get.superClass.get)
        else !isJLO(this) && ifInit(info.get.superClass.get)(!_.isInterface.get),
        s"Invalid superClass in $this: ${info.get.superClass}"
      )
      assert(
        info.get.interfaces.forall(c => ifInit(c)(_.isInterface.get)),
        s"Invalid interfaces in $this: ${info.get.interfaces}"
      )

      assert(info.get.nestedClasses.forall(c => ifInit(c)(_.isNestedClass.get)), info.get.nestedClasses)
    }

    /**
     * @return The class name without the package prefix
     */
    def simpleName: String = internalName.split("/").last

    def isInterface: Either[NoClassBTypeInfo, Boolean] = info.map(i => (i.flags & asm.Opcodes.ACC_INTERFACE) != 0)

    def superClassesTransitive: Either[NoClassBTypeInfo, List[ClassBType]] = info.flatMap(i => i.superClass match {
      case None => Right(Nil)
      case Some(sc) =>  sc.superClassesTransitive.map(sc :: _)
    })

    /**
     * The prefix of the internal name until the last '/', or the empty string.
     */
    def packageInternalName: String = {
      val name = internalName
      name.lastIndexOf('/') match {
        case -1 => ""
        case i  => name.substring(0, i)
      }
    }

    def isPublic: Either[NoClassBTypeInfo, Boolean] = info.map(i => (i.flags & asm.Opcodes.ACC_PUBLIC) != 0)

    def isNestedClass: Either[NoClassBTypeInfo, Boolean] = info.map(_.nestedInfo.isDefined)

    def enclosingNestedClassesChain: Either[NoClassBTypeInfo, List[ClassBType]] = {
      isNestedClass.flatMap(isNested => {
        // if isNested is true, we know that info.get is defined, and nestedInfo.get is also defined.
        if (isNested) info.get.nestedInfo.get.enclosingClass.enclosingNestedClassesChain.map(this :: _)
        else Right(Nil)
      })
    }

    def innerClassAttributeEntry: Either[NoClassBTypeInfo, Option[InnerClassEntry]] = info.map(i => i.nestedInfo map {
      case NestedInfo(_, outerName, innerName, isStaticNestedClass) =>
        InnerClassEntry(
          internalName,
          outerName.orNull,
          innerName.orNull,
          GenBCode.mkFlags(
            // the static flag in the InnerClass table has a special meaning, see InnerClass comment
            i.flags & ~Opcodes.ACC_STATIC,
            if (isStaticNestedClass) Opcodes.ACC_STATIC else 0
          ) & BCodeAsmCommon.INNER_CLASSES_FLAGS
        )
    })

    def inlineInfoAttribute: Either[NoClassBTypeInfo, InlineInfoAttribute] = info.map(i => {
      // InlineInfos are serialized for classes being compiled. For those the info was built by
      // buildInlineInfoFromClassSymbol, which only adds a warning under SI-9111, which in turn
      // only happens for class symbols of java source files.
      // we could put this assertion into InlineInfoAttribute, but it is more safe to put it here
      // where it affect only GenBCode, and not add any assertion to GenASM in 2.11.6.
      assert(i.inlineInfo.warning.isEmpty, i.inlineInfo.warning)
      InlineInfoAttribute(i.inlineInfo)
    })

    def isSubtypeOf(other: ClassBType): Either[NoClassBTypeInfo, Boolean] = try {
      if (this == other) return Right(true)
      if (isInterface.orThrow) {
        if (other == ObjectReference) return Right(true) // interfaces conform to Object
        if (!other.isInterface.orThrow) return Right(false)   // this is an interface, the other is some class other than object. interfaces cannot extend classes, so the result is false.
        // else: this and other are both interfaces. continue to (*)
      } else {
        val sc = info.orThrow.superClass
        if (sc.isDefined && sc.get.isSubtypeOf(other).orThrow) return Right(true) // the superclass of this class conforms to other
        if (!other.isInterface.orThrow) return Right(false) // this and other are both classes, and the superclass of this does not conform
        // else: this is a class, the other is an interface. continue to (*)
      }

      // (*) check if some interface of this class conforms to other.
      Right(info.orThrow.interfaces.exists(_.isSubtypeOf(other).orThrow))
    } catch {
      case Invalid(noInfo: NoClassBTypeInfo) => Left(noInfo)
    }

    /**
     * Finding the least upper bound in agreement with the bytecode verifier
     * Background:
     *   http://gallium.inria.fr/~xleroy/publi/bytecode-verification-JAR.pdf
     *   http://comments.gmane.org/gmane.comp.java.vm.languages/2293
     *   https://issues.scala-lang.org/browse/SI-3872
     */
    def jvmWiseLUB(other: ClassBType): Either[NoClassBTypeInfo, ClassBType] = {
      def isNotNullOrNothing(c: ClassBType) = !c.isNullType && !c.isNothingType
      assert(isNotNullOrNothing(this) && isNotNullOrNothing(other), s"jvmWiseLUB for null or nothing: $this - $other")

      tryEither {
        val res: ClassBType = (this.isInterface.orThrow, other.isInterface.orThrow) match {
          case (true, true) =>
            // exercised by test/files/run/t4761.scala
            if (other.isSubtypeOf(this).orThrow) this
            else if (this.isSubtypeOf(other).orThrow) other
            else ObjectReference

          case (true, false) =>
            if (other.isSubtypeOf(this).orThrow) this else ObjectReference

          case (false, true) =>
            if (this.isSubtypeOf(other).orThrow) other else ObjectReference

          case _ =>
            // TODO @lry I don't really understand the reasoning here.
            // Both this and other are classes. The code takes (transitively) all superclasses and
            // finds the first common one.
            // MOST LIKELY the answer can be found here, see the comments and links by Miguel:
            //  - https://issues.scala-lang.org/browse/SI-3872
            firstCommonSuffix(this :: this.superClassesTransitive.orThrow, other :: other.superClassesTransitive.orThrow)
        }

        assert(isNotNullOrNothing(res), s"jvmWiseLUB computed: $res")
        Right(res)
      }
    }

    private def firstCommonSuffix(as: List[ClassBType], bs: List[ClassBType]): ClassBType = {
      var chainA = as
      var chainB = bs
      var fcs: ClassBType = null
      do {
        if      (chainB contains chainA.head) fcs = chainA.head
        else if (chainA contains chainB.head) fcs = chainB.head
        else {
          chainA = chainA.tail
          chainB = chainB.tail
        }
      } while (fcs == null)
      fcs
    }
  }

  object ClassBType {
    // Primitive classes have no super class. A ClassBType for those is only created when
    // they are actually being compiled (e.g., when compiling scala/Boolean.scala).
    private val hasNoSuper = Set(
      "scala/Unit",
      "scala/Boolean",
      "scala/Char",
      "scala/Byte",
      "scala/Short",
      "scala/Int",
      "scala/Float",
      "scala/Long",
      "scala/Double"
    )

    private val isInternalPhantomType = Set(
      "scala/Null",
      "scala/Nothing"
    )
  }

  /**
   * The type info for a class. Used for symboltable-independent subtype checks in the backend.
   *
   * @param superClass    The super class, not defined for class java/lang/Object.
   * @param interfaces    All transitively implemented interfaces, except for those inherited
   *                      through the superclass.
   * @param flags         The java flags, obtained through `javaFlags`. Used also to derive
   *                      the flags for InnerClass entries.
   * @param nestedClasses Classes nested in this class. Those need to be added to the
   *                      InnerClass table, see the InnerClass spec summary above.
   * @param nestedInfo    If this describes a nested class, information for the InnerClass table.
   * @param inlineInfo    Information about this class for the inliner.
   */
  final case class ClassInfo(superClass: Option[ClassBType], interfaces: List[ClassBType], flags: Int,
                             nestedClasses: List[ClassBType], nestedInfo: Option[NestedInfo],
                             inlineInfo: InlineInfo)

  /**
   * Information required to add a class to an InnerClass table.
   * The spec summary above explains what information is required for the InnerClass entry.
   *
   * @param enclosingClass      The enclosing class, if it is also nested. When adding a class
   *                            to the InnerClass table, enclosing nested classes are also added.
   * @param outerName           The outerName field in the InnerClass entry, may be None.
   * @param innerName           The innerName field, may be None.
   * @param isStaticNestedClass True if this is a static nested class (not inner class) (*)
   *
   * (*) Note that the STATIC flag in ClassInfo.flags, obtained through javaFlags(classSym), is not
   * correct for the InnerClass entry, see javaFlags. The static flag in the InnerClass describes
   * a source-level property: if the class is in a static context (does not have an outer pointer).
   * This is checked when building the NestedInfo.
   */
  final case class NestedInfo(enclosingClass: ClassBType,
                              outerName: Option[String],
                              innerName: Option[String],
                              isStaticNestedClass: Boolean)

  /**
   * This class holds the data for an entry in the InnerClass table. See the InnerClass summary
   * above in this file.
   *
   * There's some overlap with the class NestedInfo, but it's not exactly the same and cleaner to
   * keep separate.
   * @param name      The internal name of the class.
   * @param outerName The internal name of the outer class, may be null.
   * @param innerName The simple name of the inner class, may be null.
   * @param flags     The flags for this class in the InnerClass entry.
   */
  final case class InnerClassEntry(name: String, outerName: String, innerName: String, flags: Int)

  final case class ArrayBType(componentType: BType) extends RefBType {
    def dimension: Int = componentType match {
      case a: ArrayBType => 1 + a.dimension
      case _ => 1
    }

    def elementType: BType = componentType match {
      case a: ArrayBType => a.elementType
      case t => t
    }
  }

  final case class MethodBType(argumentTypes: List[BType], returnType: BType) extends BType

  /* Some definitions that are required for the implementation of BTypes. They are abstract because
   * initializing them requires information from types / symbols, which is not accessible here in
   * BTypes.
   *
   * They are defs (not vals) because they are implemented using vars (see comment on CoreBTypes).
   */

  /**
   * Just a named pair, used in CoreBTypes.asmBoxTo/asmUnboxTo.
   */
  final case class MethodNameAndType(name: String, methodType: MethodBType)

  /**
   * True if the current compilation unit is of a primitive class (scala.Boolean et al).
   * Used only in assertions. Abstract here because its implementation depends on global.
   */
  def isCompilingPrimitive: Boolean
}

object BTypes {
  /**
   * A marker for strings that represent class internal names.
   * Ideally the type would be incompatible with String, for example by making it a value class.
   * But that would create overhead in a Collection[InternalName].
   */
  type InternalName = String

  /**
   * Metadata about a ClassBType, used by the inliner.
   *
   * More information may be added in the future to enable more elaborate inlinine heuristics.
   *
   * @param traitImplClassSelfType `Some(tp)` if this InlineInfo describes a trait, and the `self`
   *                               parameter type of the methods in the implementation class is not
   *                               the trait itself. Example:
   *                                 trait T { self: U => def f = 1 }
   *                               Generates something like:
   *                                 class T$class { static def f(self: U) = 1 }
   *
   *                               In order to inline a trat method call, the INVOKEINTERFACE is
   *                               rewritten to an INVOKESTATIC of the impl class, so we need the
   *                               self type (U) to get the right signature.
   *
   *                               `None` if the self type is the interface type, or if this
   *                               InlineInfo does not describe a trait.
   *
   * @param isEffectivelyFinal     True if the class cannot have subclasses: final classes, module
   *                               classes, trait impl classes.
   *
   * @param methodInfos            The [[MethodInlineInfo]]s for the methods declared in this class.
   *                               The map is indexed by the string s"$name$descriptor" (to
   *                               disambiguate overloads).
   *
   * @param warning                Contains an warning message if an error occurred when building this
   *                               InlineInfo, for example if some classfile could not be found on
   *                               the classpath. This warning can be reported later by the inliner.
   */
  final case class InlineInfo(traitImplClassSelfType: Option[InternalName],
                              isEffectivelyFinal: Boolean,
                              methodInfos: Map[String, MethodInlineInfo],
                              warning: Option[ClassInlineInfoWarning])

  val EmptyInlineInfo = InlineInfo(None, false, Map.empty, None)

  /**
   * Metadata about a method, used by the inliner.
   *
   * @param effectivelyFinal                    True if the method cannot be overridden (in Scala)
   * @param traitMethodWithStaticImplementation True if the method is an interface method method of
   *                                            a trait method and has a static counterpart in the
   *                                            implementation class.
   * @param annotatedInline                     True if the method is annotated `@inline`
   * @param annotatedNoInline                   True if the method is annotated `@noinline`
   */
  final case class MethodInlineInfo(effectivelyFinal: Boolean,
                                    traitMethodWithStaticImplementation: Boolean,
                                    annotatedInline: Boolean,
                                    annotatedNoInline: Boolean)

  // no static way (without symbol table instance) to get to nme.ScalaATTR / ScalaSignatureATTR
  val ScalaAttributeName    = "Scala"
  val ScalaSigAttributeName = "ScalaSig"
}