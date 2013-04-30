/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm

import scala.tools.asm
import scala.annotation.switch
import scala.collection.{ immutable, mutable }
import scala.tools.nsc.io.AbstractFile
import collection.convert.Wrappers.JListWrapper

/*
 *  Utilities to mediate between types as represented in Scala ASTs and ASM trees.
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded
 *  @version 1.0
 *
 */
abstract class BCodeTypes extends SubComponent with BytecodeWriters {

  import global._

  object BType {

    import global.chrs

    // ------------- sorts -------------

    val VOID   : Int =  0
    val BOOLEAN: Int =  1
    val CHAR   : Int =  2
    val BYTE   : Int =  3
    val SHORT  : Int =  4
    val INT    : Int =  5
    val FLOAT  : Int =  6
    val LONG   : Int =  7
    val DOUBLE : Int =  8
    val ARRAY  : Int =  9
    val OBJECT : Int = 10
    val METHOD : Int = 11

    // ------------- primitive types -------------

    val VOID_TYPE    = new BType(VOID,    ('V' << 24) | (5 << 16) | (0 << 8) | 0, 1)
    val BOOLEAN_TYPE = new BType(BOOLEAN, ('Z' << 24) | (0 << 16) | (5 << 8) | 1, 1)
    val CHAR_TYPE    = new BType(CHAR,    ('C' << 24) | (0 << 16) | (6 << 8) | 1, 1)
    val BYTE_TYPE    = new BType(BYTE,    ('B' << 24) | (0 << 16) | (5 << 8) | 1, 1)
    val SHORT_TYPE   = new BType(SHORT,   ('S' << 24) | (0 << 16) | (7 << 8) | 1, 1)
    val INT_TYPE     = new BType(INT,     ('I' << 24) | (0 << 16) | (0 << 8) | 1, 1)
    val FLOAT_TYPE   = new BType(FLOAT,   ('F' << 24) | (2 << 16) | (2 << 8) | 1, 1)
    val LONG_TYPE    = new BType(LONG,    ('J' << 24) | (1 << 16) | (1 << 8) | 2, 1)
    val DOUBLE_TYPE  = new BType(DOUBLE,  ('D' << 24) | (3 << 16) | (3 << 8) | 2, 1)

    /*
     * Returns the Java type corresponding to the given type descriptor.
     *
     * @param off the offset of this descriptor in the chrs buffer.
     * @return the Java type corresponding to the given type descriptor.
     *
     * can-multi-thread
     */
    def getType(off: Int): BType = {
      var len = 0
      chrs(off) match {
        case 'V' => VOID_TYPE
        case 'Z' => BOOLEAN_TYPE
        case 'C' => CHAR_TYPE
        case 'B' => BYTE_TYPE
        case 'S' => SHORT_TYPE
        case 'I' => INT_TYPE
        case 'F' => FLOAT_TYPE
        case 'J' => LONG_TYPE
        case 'D' => DOUBLE_TYPE
        case '[' =>
          len = 1
          while (chrs(off + len) == '[') {
            len += 1
          }
          if (chrs(off + len) == 'L') {
            len += 1
            while (chrs(off + len) != ';') {
              len += 1
            }
          }
          new BType(ARRAY, off, len + 1)
        case 'L' =>
          len = 1
          while (chrs(off + len) != ';') {
            len += 1
          }
          new BType(OBJECT, off + 1, len - 1)
        // case '(':
        case _ =>
          assert(chrs(off) == '(')
          var resPos = off + 1
          while(chrs(resPos) != ')') { resPos += 1 }
          val resType = getType(resPos + 1)
          val len = resPos - off + 1 + resType.len;
          new BType(
            METHOD,
            off,
            if(resType.hasObjectSort) {
              len + 2 // "+ 2" accounts for the "L ... ;" in a descriptor for a non-array reference.
            } else {
              len
            }
          )
      }
    }

    /* Params denote an internal name.
     *  can-multi-thread
     */
    def getObjectType(index: Int, length: Int): BType = {
      val sort = if(chrs(index) == '[') ARRAY else OBJECT;
      new BType(sort, index, length)
    }

    /*
     * @param typeDescriptor a field or method type descriptor.
     *
     * must-single-thread
     */
    def getType(typeDescriptor: String): BType = {
      val n = global.newTypeName(typeDescriptor)
      getType(n.start)
    }

    /*
     * @param methodDescriptor a method descriptor.
     *
     * must-single-thread
     */
    def getMethodType(methodDescriptor: String): BType = {
      val n = global.newTypeName(methodDescriptor)
      new BType(BType.METHOD, n.start, n.length) // TODO assert isValidMethodDescriptor
    }

    /*
     * Returns the Java method type corresponding to the given argument and return types.
     *
     * @param returnType the return type of the method.
     * @param argumentTypes the argument types of the method.
     * @return the Java type corresponding to the given argument and return types.
     *
     * must-single-thread
     */
    def getMethodType(returnType: BType, argumentTypes: Array[BType]): BType = {
      val n = global.newTypeName(getMethodDescriptor(returnType, argumentTypes))
      new BType(BType.METHOD, n.start, n.length)
    }

    /*
     * Returns the Java types corresponding to the argument types of method descriptor whose first argument starts at idx0.
     *
     * @param idx0 index into chrs of the first argument.
     * @return the Java types corresponding to the argument types of the given method descriptor.
     *
     * can-multi-thread
     */
    private def getArgumentTypes(idx0: Int): Array[BType] = {
      assert(chrs(idx0 - 1) == '(', "doesn't look like a method descriptor.")
      val args = new Array[BType](getArgumentCount(idx0))
      var off = idx0
      var size = 0
      while (chrs(off) != ')') {
        args(size) = getType(off)
        off += args(size).len
        if(args(size).sort == OBJECT) { off += 2 }
        // debug: assert("LVZBSCIJFD[)".contains(chrs(off)))
        size += 1
      }
      // debug: var check = 0; while(check < args.length) { assert(args(check) != null); check += 1 }
      args
    }

    /*
     * Returns the Java types corresponding to the argument types of the given
     * method descriptor.
     *
     * @param methodDescriptor a method descriptor.
     * @return the Java types corresponding to the argument types of the given method descriptor.
     *
     * must-single-thread
     */
    def getArgumentTypes(methodDescriptor: String): Array[BType] = {
      val n = global.newTypeName(methodDescriptor)
      getArgumentTypes(n.start + 1)
    }

    /*
     * Returns the number of argument types of this method type, whose first argument starts at idx0.
     *
     * @param idx0 index into chrs of the first argument.
     * @return the number of argument types of this method type.
     *
     * can-multi-thread
     */
    private def getArgumentCount(idx0: Int): Int = {
      assert(chrs(idx0 - 1) == '(', "doesn't look like a method descriptor.")
      var off  = idx0
      var size = 0
      var keepGoing = true
      while (keepGoing) {
        val car = chrs(off)
        off += 1
        if (car == ')') {
          keepGoing = false
        } else if (car == 'L') {
          while (chrs(off) != ';') { off += 1 }
          off += 1
          size += 1
        } else if (car != '[') {
          size += 1
        }
      }

      size
    }

    /*
     * Returns the Java type corresponding to the return type of the given
     * method descriptor.
     *
     * @param methodDescriptor a method descriptor.
     * @return the Java type corresponding to the return type of the given method descriptor.
     *
     * must-single-thread
     */
    def getReturnType(methodDescriptor: String): BType = {
      val n     = global.newTypeName(methodDescriptor)
      val delta = n.pos(')') // `delta` is relative to the Name's zero-based start position, not a valid index into chrs.
      assert(delta < n.length, "not a valid method descriptor: " + methodDescriptor)
      getType(n.start + delta + 1)
    }

    /*
     * Returns the descriptor corresponding to the given argument and return types.
     * Note: no BType is created here for the resulting method descriptor,
     *       if that's desired the invoker is responsible for that.
     *
     * @param returnType the return type of the method.
     * @param argumentTypes the argument types of the method.
     * @return the descriptor corresponding to the given argument and return types.
     *
     * can-multi-thread
     */
    def getMethodDescriptor(
        returnType: BType,
        argumentTypes: Array[BType]): String =
    {
      val buf = new StringBuffer()
      buf.append('(')
      var i = 0
      while(i < argumentTypes.length) {
        argumentTypes(i).getDescriptor(buf)
        i += 1
      }
      buf.append(')')
      returnType.getDescriptor(buf)
      buf.toString()
    }

  } // end of object BType

  // when compiling the Scala library, some assertions don't hold (e.g., scala.Boolean has null superClass although it's not an interface)
  val isCompilingStdLib = !(settings.sourcepath.isDefault)

  /*
   *  An item of queue-3 (the last queue before serializing to disk) contains three of these
   *  (one for each of mirror, plain, and bean classes).
   *
   *  @param jclassName  internal name of the class
   *  @param jclassBytes bytecode emitted for the class SubItem3 represents
   */
  case class SubItem3(
    jclassName:  String,
    jclassBytes: Array[Byte]
  )

  /*
   * must-single-thread
   */
  def getFileForClassfile(base: AbstractFile, clsName: String, suffix: String): AbstractFile = {
    getFile(base, clsName, suffix)
  }

  /*
   * must-single-thread
   */
  def getOutFolder(csym: Symbol, cName: String, cunit: CompilationUnit): _root_.scala.tools.nsc.io.AbstractFile = {
    try {
      outputDirectory(csym)
    } catch {
      case ex: Throwable =>
        cunit.error(cunit.body.pos, "Couldn't create file for class " + cName + "\n" + ex.getMessage)
        null
    }
  }

  /*
   * Based on ASM's Type class. Namer's chrs is used in this class for the same purposes as the `buf` char array in asm.Type.
   *
   * All methods of this classs can-multi-thread
   */
  final class BType(val sort: Int, val off: Int, val len: Int) {

    import global.chrs

    /*
     * can-multi-thread
     */
    def toASMType: scala.tools.asm.Type = {
      import scala.tools.asm
      // using `asm.Type.SHORT` instead of `BType.SHORT` because otherwise "warning: could not emit switch for @switch annotated match"
      (sort: @switch) match {
        case asm.Type.VOID    => asm.Type.VOID_TYPE
        case asm.Type.BOOLEAN => asm.Type.BOOLEAN_TYPE
        case asm.Type.CHAR    => asm.Type.CHAR_TYPE
        case asm.Type.BYTE    => asm.Type.BYTE_TYPE
        case asm.Type.SHORT   => asm.Type.SHORT_TYPE
        case asm.Type.INT     => asm.Type.INT_TYPE
        case asm.Type.FLOAT   => asm.Type.FLOAT_TYPE
        case asm.Type.LONG    => asm.Type.LONG_TYPE
        case asm.Type.DOUBLE  => asm.Type.DOUBLE_TYPE
        case asm.Type.ARRAY   |
             asm.Type.OBJECT  => asm.Type.getObjectType(getInternalName)
        case asm.Type.METHOD  => asm.Type.getMethodType(getDescriptor)
      }
    }

    /*
     * Unlike for ICode's REFERENCE, isBoxedType(t) implies isReferenceType(t)
     * Also, `isReferenceType(RT_NOTHING) == true` , similarly for RT_NULL.
     * Use isNullType() , isNothingType() to detect Nothing and Null.
     *
     * can-multi-thread
     */
    def hasObjectSort = (sort == BType.OBJECT)

    /*
     * Returns the number of dimensions of this array type. This method should
     * only be used for an array type.
     *
     * @return the number of dimensions of this array type.
     *
     * can-multi-thread
     */
    def getDimensions: Int = {
      var i = 1
      while (chrs(off + i) == '[') {
        i += 1
      }
      i
    }

    /*
     * Returns the (ultimate) element type of this array type.
     * This method should only be used for an array type.
     *
     * @return Returns the type of the elements of this array type.
     *
     * can-multi-thread
     */
    def getElementType: BType = {
      assert(isArray, "Asked for the element type of a non-array type: " + this)
      BType.getType(off + getDimensions)
    }

    /*
     * Returns the internal name of the class corresponding to this object or
     * array type. The internal name of a class is its fully qualified name (as
     * returned by Class.getName(), where '.' are replaced by '/'. This method
     * should only be used for an object or array type.
     *
     * @return the internal name of the class corresponding to this object type.
     *
     * can-multi-thread
     */
    def getInternalName: String = {
      new String(chrs, off, len)
    }

    /*
     * @return the prefix of the internal name until the last '/' (if '/' present), empty string otherwise.
     *
     * can-multi-thread
     */
    def getRuntimePackage: String = {
      assert(hasObjectSort, "not of object sort: " + toString)
      val iname = getInternalName
      val idx = iname.lastIndexOf('/')
      if(idx == -1) ""
      else iname.substring(0, idx)
    }

    /*
     * @return the suffix of the internal name until the last '/' (if '/' present), internal name otherwise.
     *
     * can-multi-thread
     */
    def getSimpleName: String = {
      assert(hasObjectSort, "not of object sort: " + toString)
      val iname = getInternalName
      val idx = iname.lastIndexOf('/')
      if(idx == -1) iname
      else iname.substring(idx + 1)
    }

    /*
     * Returns the argument types of methods of this type.
     * This method should only be used for method types.
     *
     * @return the argument types of methods of this type.
     *
     * can-multi-thread
     */
    def getArgumentTypes: Array[BType] = {
      BType.getArgumentTypes(off + 1)
    }

    /*
     * Returns the number of arguments of methods of this type.
     * This method should only be used for method types.
     *
     * @return the number of arguments of methods of this type.
     *
     * can-multi-thread
     */
    def getArgumentCount: Int = {
      BType.getArgumentCount(off + 1)
    }

    /*
     * Returns the return type of methods of this type.
     * This method should only be used for method types.
     *
     * @return the return type of methods of this type.
     *
     * can-multi-thread
     */
    def getReturnType: BType = {
      assert(chrs(off) == '(', "doesn't look like a method descriptor: " + toString)
      var resPos = off + 1
      while(chrs(resPos) != ')') { resPos += 1 }
      BType.getType(resPos + 1)
    }

    /*
     *  Given a zero-based formal-param-position, return its corresponding local-var-index,
     *  taking into account the JVM-type-sizes of preceding formal params.
     */
    def convertFormalParamPosToLocalVarIdx(paramPos: Int, isInstanceMethod: Boolean): Int = {
      assert(sort == asm.Type.METHOD)
      val paramTypes = getArgumentTypes
      var local = 0
      (0 until paramPos) foreach { argPos => local += paramTypes(argPos).getSize }

      local + (if(isInstanceMethod) 1 else 0)
    }

    /*
     *  Given a local-var-index, return its corresponding zero-based formal-param-position,
     *  taking into account the JVM-type-sizes of preceding formal params.
     */
    def convertLocalVarIdxToFormalParamPos(localIdx: Int, isInstanceMethod: Boolean): Int = {
      assert(sort == asm.Type.METHOD)
      val paramTypes = getArgumentTypes
      var remaining  = (if(isInstanceMethod) (localIdx - 1) else localIdx)
      assert(remaining >= 0)
      var result     = 0
      while(remaining > 0) {
        remaining -= paramTypes(result).getSize
        result    += 1
      }
      assert(remaining == 0)

      result
    }

    // ------------------------------------------------------------------------
    // Inspector methods
    // ------------------------------------------------------------------------

    def isPrimitiveOrVoid = (sort <  BType.ARRAY) // can-multi-thread
    def isValueType       = (sort <  BType.ARRAY) // can-multi-thread
    def isArray           = (sort == BType.ARRAY) // can-multi-thread
    def isUnitType        = (sort == BType.VOID)  // can-multi-thread

    def isRefOrArrayType   = { hasObjectSort ||  isArray    } // can-multi-thread
    def isNonUnitValueType = { isValueType   && !isUnitType } // can-multi-thread

    def isNonSpecial  = { !isValueType && !isArray && !isPhantomType   } // can-multi-thread
    def isNothingType = { (this == RT_NOTHING) || (this == CT_NOTHING) } // can-multi-thread
    def isNullType    = { (this == RT_NULL)    || (this == CT_NULL)    } // can-multi-thread
    def isPhantomType = { isNothingType || isNullType } // can-multi-thread

    /*
     * can-multi-thread
     */
    def isBoxed = {
      this match {
        case BOXED_UNIT  | BOXED_BOOLEAN | BOXED_CHAR   |
             BOXED_BYTE  | BOXED_SHORT   | BOXED_INT    |
             BOXED_FLOAT | BOXED_LONG    | BOXED_DOUBLE
          => true
        case _
          => false
      }
    }

    /* On the JVM,
     *    BOOL, BYTE, CHAR, SHORT, and INT
     *  are like Ints for the purpose of lub calculation.
     *
     * can-multi-thread
     */
    def isIntSizedType = {
      (sort : @switch) match {
        case BType.BOOLEAN | BType.CHAR  |
             BType.BYTE    | BType.SHORT | BType.INT
          => true
        case _
          => false
      }
    }

    /* On the JVM, similar to isIntSizedType except that BOOL isn't integral while LONG is.
     *
     * can-multi-thread
     */
    def isIntegralType = {
      (sort : @switch) match {
        case BType.CHAR  |
             BType.BYTE  | BType.SHORT | BType.INT |
             BType.LONG
          => true
        case _
          => false
      }
    }

    /* On the JVM, FLOAT and DOUBLE.
     *
     * can-multi-thread
     */
    def isRealType = { (sort == BType.FLOAT ) || (sort == BType.DOUBLE) }

    def isNumericType = (isIntegralType || isRealType) // can-multi-thread

    /* Is this type a category 2 type in JVM terms? (ie, is it LONG or DOUBLE?)
     *
     * can-multi-thread
     */
    def isWideType = (getSize == 2)

    def isClosureClass: Boolean = {
      val tr = exemplars.get(this); (tr != null && tr.isLambda)
    }

    def isCapturedCellRef: Boolean = {
      this == srBooleanRef || this == srByteRef  ||
      this == srCharRef    ||
      this == srIntRef     ||
      this == srLongRef    ||
      this == srFloatRef   || this == srDoubleRef
    }

    /*
     * Element vs. Component type of an array:
     * Quoting from the JVMS, Sec. 2.4 "Reference Types and Values"
     *
     *   An array type consists of a component type with a single dimension (whose
     *   length is not given by the type). The component type of an array type may itself be
     *   an array type. If, starting from any array type, one considers its component type,
     *   and then (if that is also an array type) the component type of that type, and so on,
     *   eventually one must reach a component type that is not an array type; this is called
     *   the element type of the array type. The element type of an array type is necessarily
     *   either a primitive type, or a class type, or an interface type.
     *
     */

    /* The type of items this array holds.
     *
     * can-multi-thread
     */
    def getComponentType: BType = {
      assert(isArray, "Asked for the component type of a non-array type: " + this)
      BType.getType(off + 1)
    }

    // ------------------------------------------------------------------------
    // Conversion to type descriptors
    // ------------------------------------------------------------------------

    /*
     * @return the descriptor corresponding to this Java type.
     *
     * can-multi-thread
     */
    def getDescriptor: String = {
      val buf = new StringBuffer()
      getDescriptor(buf)
      buf.toString()
    }

    /*
     * Appends the descriptor corresponding to this Java type to the given string buffer.
     *
     * @param buf the string buffer to which the descriptor must be appended.
     *
     * can-multi-thread
     */
    private def getDescriptor(buf: StringBuffer) {
      if (isPrimitiveOrVoid) {
        // descriptor is in byte 3 of 'off' for primitive types (buf == null)
        buf.append(((off & 0xFF000000) >>> 24).asInstanceOf[Char])
      } else if (sort == BType.OBJECT) {
        buf.append('L')
        buf.append(chrs, off, len)
        buf.append(';')
      } else { // sort == ARRAY || sort == METHOD
        buf.append(chrs, off, len)
      }
    }

    // ------------------------------------------------------------------------
    // Corresponding size and opcodes
    // ------------------------------------------------------------------------

    /*
     * Returns the size of values of this type.
     * This method must not be used for method types.
     *
     * @return the size of values of this type, i.e., 2 for <tt>long</tt> and
     *         <tt>double</tt>, 0 for <tt>void</tt> and 1 otherwise.
     *
     * can-multi-thread
     */
    def getSize: Int = {
      // the size is in byte 0 of 'off' for primitive types (buf == null)
      if(isPrimitiveOrVoid) (off & 0xFF) else 1
    }

    /*
     * Returns a JVM instruction opcode adapted to this Java type. This method
     * must not be used for method types.
     *
     * @param opcode a JVM instruction opcode. This opcode must be one of ILOAD,
     *        ISTORE, IALOAD, IASTORE, IADD, ISUB, IMUL, IDIV, IREM, INEG, ISHL,
     *        ISHR, IUSHR, IAND, IOR, IXOR and IRETURN.
     * @return an opcode that is similar to the given opcode, but adapted to
     *         this Java type. For example, if this type is <tt>float</tt> and
     *         <tt>opcode</tt> is IRETURN, this method returns FRETURN.
     *
     * can-multi-thread
     */
    def getOpcode(opcode: Int): Int = {
      import scala.tools.asm.Opcodes
      if (opcode == Opcodes.IALOAD || opcode == Opcodes.IASTORE) {
        // the offset for IALOAD or IASTORE is in byte 1 of 'off' for
        // primitive types (buf == null)
        opcode + (if(isPrimitiveOrVoid) (off & 0xFF00) >> 8 else 4)
      } else {
        // the offset for other instructions is in byte 2 of 'off' for
        // primitive types (buf == null)
        opcode + (if (isPrimitiveOrVoid) (off & 0xFF0000) >> 16 else 4)
      }
    }

    // ------------------------------------------------------------------------
    // Equals, hashCode and toString
    // ------------------------------------------------------------------------

    /*
     * Tests if the given object is equal to this type.
     *
     * @param o the object to be compared to this type.
     * @return <tt>true</tt> if the given object is equal to this type.
     *
     * can-multi-thread
     */
    override def equals(o: Any): Boolean = {
      if (!(o.isInstanceOf[BType])) {
        return false
      }
      val t = o.asInstanceOf[BType]
      if (this eq t) {
        return true
      }
      if (sort != t.sort) {
        return false
      }
      if (sort >= BType.ARRAY) {
        if (len != t.len) {
          return false
        }
        // sort checked already
        if (off == t.off) {
          return true
        }
        var i = 0
        while(i < len) {
          if (chrs(off + i) != chrs(t.off + i)) {
            return false
          }
          i += 1
        }
        // If we reach here, we could update the largest of (this.off, t.off) to match the other, so as to simplify future == comparisons.
        // But that would require a var rather than val.
      }
      true
    }

    /*
     * @return a hash code value for this type.
     *
     * can-multi-thread
     */
    override def hashCode(): Int = {
      var hc = 13 * sort;
      if (sort >= BType.ARRAY) {
        var i = off
        val end = i + len
        while (i < end) {
          hc = 17 * (hc + chrs(i))
          i += 1
        }
      }
      hc
    }

    /*
     * @return the descriptor of this type.
     *
     * can-multi-thread
     */
    override def toString: String = { getDescriptor }

  }

  /*
   * Creates a TypeName and the BType token for it.
   * This method does not add to `innerClassBufferASM`, use `internalName()` or `asmType()` or `toTypeKind()` for that.
   *
   * must-single-thread
   */
  def brefType(iname: String): BType = { brefType(newTypeName(iname.toCharArray(), 0, iname.length())) }

  /*
   * Creates a BType token for the TypeName received as argument.
   * This method does not add to `innerClassBufferASM`, use `internalName()` or `asmType()` or `toTypeKind()` for that.
   *
   *  can-multi-thread
   */
  def brefType(iname: TypeName): BType = { BType.getObjectType(iname.start, iname.length) }

  // due to keyboard economy only
  val UNIT   = BType.VOID_TYPE
  val BOOL   = BType.BOOLEAN_TYPE
  val CHAR   = BType.CHAR_TYPE
  val BYTE   = BType.BYTE_TYPE
  val SHORT  = BType.SHORT_TYPE
  val INT    = BType.INT_TYPE
  val LONG   = BType.LONG_TYPE
  val FLOAT  = BType.FLOAT_TYPE
  val DOUBLE = BType.DOUBLE_TYPE

  /*
   * RT_NOTHING and RT_NULL exist at run-time only.
   * They are the bytecode-level manifestation (in method signatures only) of what shows up as NothingClass resp. NullClass in Scala ASTs.
   * Therefore, when RT_NOTHING or RT_NULL are to be emitted,
   * a mapping is needed: the internal names of NothingClass and NullClass can't be emitted as-is.
   */
  val RT_NOTHING = brefType("scala/runtime/Nothing$")
  val RT_NULL    = brefType("scala/runtime/Null$")
  val CT_NOTHING = brefType("scala/Nothing") // TODO needed?
  val CT_NULL    = brefType("scala/Null")    // TODO needed?

  val srBooleanRef = brefType("scala/runtime/BooleanRef")
  val srByteRef    = brefType("scala/runtime/ByteRef")
  val srCharRef    = brefType("scala/runtime/CharRef")
  val srIntRef     = brefType("scala/runtime/IntRef")
  val srLongRef    = brefType("scala/runtime/LongRef")
  val srFloatRef   = brefType("scala/runtime/FloatRef")
  val srDoubleRef  = brefType("scala/runtime/DoubleRef")

  val srBoxedUnit  = brefType("scala/runtime/BoxedUnit")

  val ObjectReference   = brefType("java/lang/Object")
  val AnyRefReference   = ObjectReference
  val objArrayReference = arrayOf(ObjectReference)
  // special names
  var StringReference          : BType = null
  var ThrowableReference       : BType = null
  var jlCloneableReference     : BType = null // java/lang/Cloneable
  var jlNPEReference           : BType = null // java/lang/NullPointerException
  var jioSerializableReference : BType = null // java/io/Serializable
  var scalaSerializableReference  : BType = null // scala/Serializable
  var classCastExceptionReference : BType = null // java/lang/ClassCastException
  val StringBuilderClassName   = "scala/collection/mutable/StringBuilder"

  var lateClosureInterfaces: Array[Tracked] = null // the only interface a Late-Closure-Class implements is scala.Serializable

  /* A map from scala primitive type-symbols to BTypes */
  var primitiveTypeMap: Map[Symbol, BType] = null
  /* A map from scala type-symbols for Nothing and Null to (runtime version) BTypes */
  var phantomTypeMap:   Map[Symbol, BType] = null
  /* Maps the method symbol for a box method to the boxed type of the result.
   *  For example, the method symbol for `Byte.box()`) is mapped to the BType `Ljava/lang/Integer;`. */
  var boxResultType:    Map[Symbol, BType] = null
  /* Maps the method symbol for an unbox method to the primitive type of the result.
   *  For example, the method symbol for `Byte.unbox()`) is mapped to the BType BYTE. */
  var unboxResultType:  Map[Symbol, BType] = null

  var hashMethodSym: Symbol = null // scala.runtime.ScalaRunTime.hash

  var AndroidParcelableInterface: Symbol = null
  var AndroidCreatorClass       : Symbol = null // this is an inner class, use asmType() to get hold of its BType while tracking in innerClassBufferASM
  var androidCreatorType        : BType  = null

  var BeanInfoAttr: Symbol = null

  /* The Object => String overload. */
  var String_valueOf: Symbol = null

  var ArrayInterfaces: Set[Tracked] = null

  var StringBuilderReference: BType = null

  // scala.FunctionX and scala.runtim.AbstractFunctionX
  val FunctionReference                 = new Array[Tracked](definitions.MaxFunctionArity + 1)
  val AbstractFunctionReference         = new Array[Tracked](definitions.MaxFunctionArity + 1)
  val abstractFunctionArityMap = mutable.Map.empty[BType, Int]

  var PartialFunctionReference:         BType = null // scala.PartialFunction
  var AbstractPartialFunctionReference: BType = null // scala.runtime.AbstractPartialFunction

  /*
   * must-single-thread
   */
  def initBCodeTypes() {

    import definitions._

    primitiveTypeMap =
      Map(
        UnitClass     -> UNIT,
        BooleanClass  -> BOOL,
        CharClass     -> CHAR,
        ByteClass     -> BYTE,
        ShortClass    -> SHORT,
        IntClass      -> INT,
        LongClass     -> LONG,
        FloatClass    -> FLOAT,
        DoubleClass   -> DOUBLE
      )

    phantomTypeMap =
      Map(
        NothingClass -> RT_NOTHING,
        NullClass    -> RT_NULL,
        NothingClass -> RT_NOTHING, // we map on purpose to RT_NOTHING, getting rid of the distinction compile-time vs. runtime for NullClass.
        NullClass    -> RT_NULL     // ditto.
      )

    boxResultType =
      for(Pair(csym, msym) <- definitions.boxMethod)
      yield (msym -> classLiteral(primitiveTypeMap(csym)))

    unboxResultType =
      for(Pair(csym, msym) <- definitions.unboxMethod)
      yield (msym -> primitiveTypeMap(csym))

    // boxed classes are looked up in the `exemplars` map by jvmWiseLUB().
    // Other than that, they aren't needed there (e.g., `isSubtypeOf()` special-cases boxed classes, similarly for others).
    val boxedClasses = List(BoxedBooleanClass, BoxedCharacterClass, BoxedByteClass, BoxedShortClass, BoxedIntClass, BoxedLongClass, BoxedFloatClass, BoxedDoubleClass)
    for(csym <- boxedClasses) {
      val key = brefType(csym.javaBinaryName.toTypeName)
      val tr  = buildExemplar(key, csym)
      symExemplars.put(csym, tr)
      exemplars.put(tr.c, tr)
    }

    // reversePrimitiveMap = (primitiveTypeMap map { case (s, pt) => (s.tpe, pt) } map (_.swap)).toMap

    hashMethodSym = getMember(ScalaRunTimeModule, nme.hash_)

    // TODO avoiding going through through missingHook for every line in the REPL: https://github.com/scala/scala/commit/8d962ed4ddd310cc784121c426a2e3f56a112540
    AndroidParcelableInterface = rootMirror.getClassIfDefined("android.os.Parcelable")
    AndroidCreatorClass        = rootMirror.getClassIfDefined("android.os.Parcelable$Creator")

    // the following couldn't be an eager vals in Phase constructors:
    // that might cause cycles before Global has finished initialization.
    BeanInfoAttr = rootMirror.getRequiredClass("scala.beans.BeanInfo")

    String_valueOf = {
      getMember(StringModule, nme.valueOf) filter (sym =>
        sym.info.paramTypes match {
          case List(pt) => pt.typeSymbol == ObjectClass
          case _        => false
        }
      )
    }

    ArrayInterfaces = Set(JavaCloneableClass, JavaSerializableClass) map exemplar

    StringReference             = exemplar(StringClass).c
    StringBuilderReference      = exemplar(StringBuilderClass).c
    ThrowableReference          = exemplar(ThrowableClass).c
    jlCloneableReference        = exemplar(JavaCloneableClass).c
    jlNPEReference              = exemplar(NullPointerExceptionClass).c
    jioSerializableReference    = exemplar(JavaSerializableClass).c
    scalaSerializableReference  = exemplar(SerializableClass).c
    classCastExceptionReference = exemplar(ClassCastExceptionClass).c

    lateClosureInterfaces = Array(exemplar(SerializableClass))

    /*
     *  The bytecode emitter special-cases String concatenation, in that three methods of `JCodeMethodN`
     *  ( `genStartConcat()` , `genStringConcat()` , and `genEndConcat()` )
     *  don't obtain the method descriptor of the callee via `asmMethodType()` (as normally done)
     *  but directly emit callsites on StringBuilder using literal constant for method descriptors.
     *  In order to make sure those method descriptors are available as BTypes, they are initialized here.
     */
    BType.getMethodType("()V")                   // necessary for JCodeMethodN.genStartConcat
    BType.getMethodType("()Ljava/lang/String;")  // necessary for JCodeMethodN.genEndConcat

    PartialFunctionReference    = exemplar(PartialFunctionClass).c
    for(idx <- 0 to definitions.MaxFunctionArity) {
      FunctionReference(idx)           = exemplar(FunctionClass(idx))
      AbstractFunctionReference(idx)   = exemplar(AbstractFunctionClass(idx))
      abstractFunctionArityMap        += (AbstractFunctionReference(idx).c -> idx)
      AbstractPartialFunctionReference = exemplar(AbstractPartialFunctionClass).c
    }

    initBCodeOpt()
  }

  /*
   * must-single-thread
   */
  def clearBCodeTypes() {
    symExemplars.clear()
    exemplars.clear()
  }

  val BOXED_UNIT    = brefType("java/lang/Void")
  val BOXED_BOOLEAN = brefType("java/lang/Boolean")
  val BOXED_BYTE    = brefType("java/lang/Byte")
  val BOXED_SHORT   = brefType("java/lang/Short")
  val BOXED_CHAR    = brefType("java/lang/Character")
  val BOXED_INT     = brefType("java/lang/Integer")
  val BOXED_LONG    = brefType("java/lang/Long")
  val BOXED_FLOAT   = brefType("java/lang/Float")
  val BOXED_DOUBLE  = brefType("java/lang/Double")

  /*  Map from type kinds to the Java reference types.
   *  Useful when pushing class literals onto the operand stack (ldc instruction taking a class literal).
   *  @see Predef.classOf
   *  @see genConstant()
   */
  val classLiteral = immutable.Map[BType, BType](
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

  case class MethodNameAndType(mname: String, mdesc: String)

  val asmBoxTo: Map[BType, MethodNameAndType] = {
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

  val asmUnboxTo: Map[BType, MethodNameAndType] = {
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

  val CLASS_CONSTRUCTOR_NAME    = "<clinit>"
  val INSTANCE_CONSTRUCTOR_NAME = "<init>"

  val PublicStatic      = asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_STATIC
  val PublicStaticFinal = asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_STATIC | asm.Opcodes.ACC_FINAL

  val strMODULE_INSTANCE_FIELD = nme.MODULE_INSTANCE_FIELD.toString

  // ------------------------------------------------
  // accessory maps tracking the isInterface, innerClasses, superClass, and supportedInterfaces relations,
  // allowing answering `conforms()` without resorting to typer.
  // ------------------------------------------------

  val exemplars       = new java.util.concurrent.ConcurrentHashMap[BType,  Tracked]
  val symExemplars    = new java.util.concurrent.ConcurrentHashMap[Symbol, Tracked]

  /*
   *  Typically, a question about a BType can be answered only by using the BType as lookup key in one or more maps.
   *  A `Tracked` object saves time by holding together information required to answer those questions:
   *
   *    - `sc`     denotes the bytecode-level superclass if any, null otherwise
   *
   *    - `ifaces` denotes the interfaces explicitly declared.
   *               Not included are those transitively supported, but the utility method `allLeafIfaces()` can be used for that.
   *
   *    - `innersChain` denotes the containing classes for a non-package-level class `c`, null otherwise.
   *               Note: the optimizer may inline anonymous closures, thus eliding those inner classes
   *               (no physical class file is emitted for elided classes).
   *               Before committing `innersChain` to bytecode, cross-check with the list of elided classes (SI-6546).
   *
   *  All methods of this class can-multi-thread
   */
  case class Tracked(c: BType, flags: Int, sc: Tracked, ifaces: Array[Tracked], innersChain: Array[InnerClassEntry]) {

    // not a case-field because we initialize it only for JVM classes we emit.
    private var _directMemberClasses: List[BType] = null

    def directMemberClasses: List[BType] = {
      assert(_directMemberClasses != null, s"getter directMemberClasses() invoked too early for $c")
      _directMemberClasses
    }

    def directMemberClasses_=(bs: List[BType]) {
      if(_directMemberClasses != null) {
        // TODO we enter here when both mirror class and plain class are emitted for the same ModuleClassSymbol.
        assert(_directMemberClasses == bs.sortBy(_.off))
      }
      _directMemberClasses = bs.sortBy(_.off)
    }

    /* `isCompilingStdLib` saves the day when compiling:
     *     (1) scala.Nothing (the test `c.isNonSpecial` fails for it)
     *     (2) scala.Boolean (it has null superClass and is not an interface)
     */
    assert(c.isNonSpecial || isCompilingStdLib /*(1)*/, "non well-formed plain-type: " + this)
    assert(
        if(sc == null) { (c == ObjectReference) || isInterface || isCompilingStdLib /*(2)*/ }
        else           { (c != ObjectReference) && !sc.isInterface }
      , "non well-formed plain-type: " + this
    )
    assert(ifaces.forall(i => i.c.isNonSpecial && i.isInterface), "non well-formed plain-type: " + this)

    import asm.Opcodes._
    def hasFlags(mask: Int) = (flags & mask) != 0
    def isPrivate    = hasFlags(ACC_PRIVATE)
    def isPublic     = hasFlags(ACC_PUBLIC)
    def isAbstract   = hasFlags(ACC_ABSTRACT)
    def isInterface  = hasFlags(ACC_INTERFACE)
    def isFinal      = hasFlags(ACC_FINAL)
    def isSynthetic  = hasFlags(ACC_SYNTHETIC)
    def isSuper      = hasFlags(ACC_SUPER)
    def isDeprecated = hasFlags(ACC_DEPRECATED)
    def isInnerClass = { innersChain != null }
    def isTraditionalClosureClass = {
      isInnerClass && isFinal && (c.getSimpleName.contains(tpnme.ANON_FUN_NAME.toString)) && isFunctionType(c)
    }
    def isLambda = {
      // ie isLCC || isTraditionalClosureClass
      isFinal && (c.getSimpleName.contains(tpnme.ANON_FUN_NAME.toString)) && isFunctionType(c)
    }
    def isSerializable = { isSubtypeOf(jioSerializableReference) }

    /* can-multi-thread */
    def superClasses: List[Tracked] = {
      if(sc == null) Nil else sc :: sc.superClasses
    }

    /* can-multi-thread */
    def isSubtypeOf(other: BType): Boolean = {
      assert(other.isNonSpecial, "so called special cases have to be handled in BCodeTypes.conforms()")

      if(c == other) return true;

      val otherIsIface = exemplars.get(other).isInterface

      if(this.isInterface) {
        if(other == ObjectReference) return true;
        if(!otherIsIface) return false;
      }
      else {
        if(sc != null && sc.isSubtypeOf(other)) return true;
        if(!otherIsIface) return false;
      }

      var idx = 0
      while(idx < ifaces.length) {
        if(ifaces(idx).isSubtypeOf(other)) return true;
        idx += 1
      }

      false
    }

    /*
     *  The `ifaces` field lists only those interfaces declared by `c`
     *  From the set of all supported interfaces, this method discards those which are supertypes of others in the set.
     */
    def allLeafIfaces: Set[Tracked] = {
      if(sc == null) { ifaces.toSet }
      else { minimizeInterfaces(ifaces.toSet ++ sc.allLeafIfaces) }
    }

    /*
     *  This type may not support in its entirety the interface given by the argument, however it may support some of its super-interfaces.
     *  We visualize each such supported subset of the argument's functionality as a "branch". This method returns all such branches.
     *
     *  In other words, let Ri be a branch supported by `ib`,
     *  this method returns all Ri such that this <:< Ri, where each Ri is maximally deep.
     */
    def supportedBranches(ib: Tracked): Set[Tracked] = {
      assert(ib.isInterface, "Non-interface argument: " + ib)

      val result: Set[Tracked] =
        if(this.isSubtypeOf(ib.c)) { Set(ib) }
        else { ib.ifaces.toSet[Tracked].flatMap( bi => supportedBranches(bi) ) }

      checkAllInterfaces(result)

      result
    }

    override def toString = { c.toString }

  }

  /* must-single-thread */
  final def isDeprecated(sym: Symbol): Boolean = { sym.annotations exists (_ matches definitions.DeprecatedAttr) }

  /* must-single-thread */
  final def hasInternalName(sym: Symbol) = { sym.isClass || (sym.isModule && !sym.isMethod) }

  /* must-single-thread */
  def getSuperInterfaces(csym: Symbol): List[Symbol] = {

      // Additional interface parents based on annotations and other cues
      def newParentForAttr(attr: Symbol): Option[Symbol] = attr match {
        case definitions.RemoteAttr       => Some(definitions.RemoteInterfaceClass)
        case _ => None
      }

      /* Drop redundant interfaces (which are implemented by some other parent) from the immediate parents.
       *  In other words, no two interfaces in the result are related by subtyping.
       *  This method works on Symbols, a similar one (not duplicate) works on Tracked instances.
       */
      def minimizeInterfaces(lstIfaces: List[Symbol]): List[Symbol] = {
        var rest   = lstIfaces
        var leaves = List.empty[Symbol]
        while(!rest.isEmpty) {
          val candidate = rest.head
          val nonLeaf = leaves exists { lsym => lsym isSubClass candidate }
          if(!nonLeaf) {
            leaves = candidate :: (leaves filterNot { lsym => candidate isSubClass lsym })
          }
          rest = rest.tail
        }

        leaves
      }

    val superInterfaces0: List[Symbol] = csym.mixinClasses
    val superInterfaces:  List[Symbol] = {
      val sups = (superInterfaces0 ++ csym.annotations.flatMap(ann => newParentForAttr(ann.symbol)))

      sups.distinct
    };

    assert(!superInterfaces.contains(NoSymbol), "found NoSymbol among: " + superInterfaces.mkString)
    assert(superInterfaces.forall(s => s.isInterface || s.isTrait), "found non-interface among: " + superInterfaces.mkString)

    minimizeInterfaces(superInterfaces)
  }

  final def exemplarIfExisting(iname: String): Tracked = {
    val bt = lookupRefBTypeIfExisting(iname)
    if(bt != null) exemplars.get(bt)
    else null
  }

  final def lookupExemplar(iname: String) = {
    exemplars.get(lookupRefBType(iname))
  }

  /*
   * Records the superClass and supportedInterfaces relations,
   * so that afterwards queries can be answered without resorting to typer.
   * This method does not add to `innerClassBufferASM`, use `internalName()` or `asmType()` or `toTypeKind()` for that.
   * On the other hand, this method does record the inner-class status of the argument, via `buildExemplar()`.
   *
   * must-single-thread
   */
  final def exemplar(csym0: Symbol): Tracked = {
    assert(csym0 != NoSymbol, "NoSymbol can't be tracked")

    val csym = {
      if(csym0.isJavaDefined && csym0.isModuleClass) csym0.linkedClassOfClass
      else if(csym0.isModule) csym0.moduleClass
      else csym0 // we track only module-classes and plain-classes
    }

    assert(!primitiveTypeMap.contains(csym) || isCompilingStdLib, "primitive types not tracked here: " + csym.fullName)
    assert(!phantomTypeMap.contains(csym),   "phantom types not tracked here: " + csym.fullName)

    val opt = symExemplars.get(csym)
    if(opt != null) {
      return opt
    }

    val key = brefType(csym.javaBinaryName.toTypeName)
    assert(key.isNonSpecial || isCompilingStdLib, "Not a class to track: " + csym.fullName)

    // TODO accomodate the fix for SI-5031 of https://github.com/scala/scala/commit/0527b2549bcada2fda2201daa630369b377d0877
    // TODO Weaken this assertion? buildExemplar() needs to be updated, too. In the meantime, pos/t5031_3 has been moved to test/disabled/pos.
    assert(!exemplars.containsKey(key), "Maps `symExemplars` and `exemplars` got out of synch.")
    val tr = buildExemplar(key, csym)
    symExemplars.put(csym, tr)
    if(csym != csym0) { symExemplars.put(csym0, tr) }
    exemplars.put(tr.c, tr) // tr.c is the hash-consed, internalized, canonical representative for csym's key.
    tr
  }

  val EMPTY_TRACKED_SET  = Set.empty[Tracked]

  val EMPTY_TRACKED_ARRAY  = Array.empty[Tracked]
  val EMPTY_STRING_ARRAY   = Array.empty[String]
  val EMPTY_INT_ARRAY      = Array.empty[Int]
  val EMPTY_LABEL_ARRAY    = Array.empty[asm.Label]
  val EMPTY_BTYPE_ARRAY    = Array.empty[BType]
  val EMPTY_InnerClassEntry_ARRAY = Array.empty[InnerClassEntry]

  /*
   * must-single-thread
   */
  private def buildExemplar(key: BType, csym: Symbol): Tracked = {
    val sc =
     if(csym.isImplClass) definitions.ObjectClass
     else csym.superClass
    assert(
      if(csym == definitions.ObjectClass)
        sc == NoSymbol
      else if(csym.isInterface)
        sc == definitions.ObjectClass
      else
        ((sc != NoSymbol) && !sc.isInterface) || isCompilingStdLib,
      "superClass out of order"
    )
    val ifaces    = getSuperInterfaces(csym) map exemplar;
    val ifacesArr =
     if(ifaces.isEmpty) EMPTY_TRACKED_ARRAY
     else {
      val arr = new Array[Tracked](ifaces.size)
      ifaces.copyToArray(arr)
      arr
     }

    val flags = mkFlags(
      javaFlags(csym),
      if(isDeprecated(csym)) asm.Opcodes.ACC_DEPRECATED else 0 // ASM pseudo access flag
    )

    val tsc = if(sc == NoSymbol) null else exemplar(sc)

    val innersChain = saveInnerClassesFor(csym, key)

    Tracked(key, flags, tsc, ifacesArr, innersChain)
  }

  // ---------------- utilities around interfaces represented by Tracked instances. ----------------

  /*  Drop redundant interfaces (those which are implemented by some other).
   *  In other words, no two interfaces in the result are related by subtyping.
   *  This method works on Tracked elements, a similar one (not duplicate) works on Symbols.
   */
  def minimizeInterfaces(lstIfaces: Set[Tracked]): Set[Tracked] = {
    checkAllInterfaces(lstIfaces)
    var rest   = lstIfaces.toList
    var leaves = List.empty[Tracked]
    while(!rest.isEmpty) {
      val candidate = rest.head
      val nonLeaf = leaves exists { leaf => leaf.isSubtypeOf(candidate.c) }
      if(!nonLeaf) {
        leaves = candidate :: (leaves filterNot { leaf => candidate.isSubtypeOf(leaf.c) })
      }
      rest = rest.tail
    }

    leaves.toSet
  }

  def allInterfaces(is: Iterable[Tracked]): Boolean = { is forall { i => i.isInterface } }
  def nonInterfaces(is: Iterable[Tracked]): Iterable[Tracked] = { is filterNot { i => i.isInterface } }

  def checkAllInterfaces(ifaces: Iterable[Tracked]) {
    assert(allInterfaces(ifaces), "Non-interfaces: " + nonInterfaces(ifaces).mkString)
  }

  /*
   *  Returns the intersection of two sets of interfaces.
   */
  def intersection(ifacesA: Set[Tracked], ifacesB: Set[Tracked]): Set[Tracked] = {
    var acc: Set[Tracked] = Set()
    for(ia <- ifacesA; ib <- ifacesB) {
      val ab = ia.supportedBranches(ib)
      val ba = ib.supportedBranches(ia)
      acc = minimizeInterfaces(acc ++ ab ++ ba)
    }
    checkAllInterfaces(acc)

    acc
  }

  // ---------------- inspector methods on BType  ----------------

  /* can-multi-thread */
  final def mkArray(xs: List[BType]): Array[BType] = {
    if(xs.isEmpty) { return EMPTY_BTYPE_ARRAY }
    val a = new Array[BType](xs.size); xs.copyToArray(a); a
  }
  /* can-multi-thread */
  final def mkArray(xs: List[String]): Array[String] = {
    if(xs.isEmpty) { return EMPTY_STRING_ARRAY }
    val a = new Array[String](xs.size); xs.copyToArray(a); a
  }
  /* can-multi-thread */
  final def mkArray(xs: List[asm.Label]): Array[asm.Label] = {
    if(xs.isEmpty) { return EMPTY_LABEL_ARRAY }
    val a = new Array[asm.Label](xs.size); xs.copyToArray(a); a
  }
  /* can-multi-thread */
  final def mkArray(xs: List[Int]): Array[Int] = {
    if(xs.isEmpty) { return EMPTY_INT_ARRAY }
    val a = new Array[Int](xs.size); xs.copyToArray(a); a
  }
  /* can-multi-thread */
  final def mkArray(xs: List[Tracked]): Array[Tracked] = {
    if(xs.isEmpty) { return EMPTY_TRACKED_ARRAY }
    val a = new Array[Tracked](xs.size); xs.copyToArray(a); a
  }

  /*
   * can-multi-thread
   */
  final def mkArrayReverse(xs: List[String]): Array[String] = {
    val len = xs.size
    if(len == 0) { return EMPTY_STRING_ARRAY }
    val a = new Array[String](len)
    var i = len - 1
    var rest = xs
    while(!rest.isEmpty) {
      a(i) = rest.head
      rest = rest.tail
      i -= 1
    }
    a
  }

  /*
   * can-multi-thread
   */
  final def mkArrayReverse(xs: List[Int]): Array[Int] = {
    val len = xs.size
    if(len == 0) { return EMPTY_INT_ARRAY }
    val a = new Array[Int](len)
    var i = len - 1
    var rest = xs
    while(!rest.isEmpty) {
      a(i) = rest.head
      rest = rest.tail
      i -= 1
    }
    a
  }

  /*
   * can-multi-thread
   */
  final def mkArrayReverse(xs: List[asm.Label]): Array[asm.Label] = {
    val len = xs.size
    if(len == 0) { return EMPTY_LABEL_ARRAY }
    val a = new Array[asm.Label](len)
    var i = len - 1
    var rest = xs
    while(!rest.isEmpty) {
      a(i) = rest.head
      rest = rest.tail
      i -= 1
    }
    a
  }

  /*
   * The type of 1-dimensional arrays of `elem` type.
   * The invoker is responsible for tracking (if needed) the inner class given by the elem BType.
   *
   * must-single-thread
   */
  final def arrayOf(elem: BType): BType = {
    assert(!(elem.isUnitType), "The element type of an array can't be: " + elem)
    brefType("[" + elem.getDescriptor)
  }

  /*
   * The type of N-dimensional arrays of `elem` type.
   * The invoker is responsible for tracking (if needed) the inner class given by the elem BType.
   *
   * must-single-thread
   */
  final def arrayN(elem: BType, dims: Int): BType = {
    assert(dims > 0)
    assert(!(elem.isUnitType) && !(elem.isPhantomType),
           "The element type of an array type is necessarily either a primitive type, or a class type, or an interface type.")
    val desc = ("[" * dims) + elem.getDescriptor
    brefType(desc)
  }

  /*
   * Subtype check `a <:< b` on BTypes that takes into account the JVM built-in numeric promotions (e.g. BYTE to INT).
   * Its operation can be visualized more easily in terms of the Java bytecode type hierarchy.
   * This method used to be called, in the ICode world, TypeKind.<:<()
   *
   * can-multi-thread
   */
  final def conforms(a: BType, b: BType): Boolean = {
    if(a.isArray) { // may be null
      /* Array subtyping is covariant here, as in Java bytecode. Also necessary for Java interop. */
      if((b == jlCloneableReference)     ||
         (b == jioSerializableReference) ||
         (b == AnyRefReference))    { true  }
      else if(b.isArray)            { conforms(a.getComponentType, b.getComponentType) }
      else                          { false }
    }
    else if(a.isBoxed) { // may be null
      if(b.isBoxed)                 { a == b }
      else if(b == AnyRefReference) { true  }
      else if(!(b.hasObjectSort))   { false }
      else                          { exemplars.get(a).isSubtypeOf(b) } // e.g., java/lang/Double conforms to java/lang/Number
    }
    else if(a.isNullType) { // known to be null
      if(b.isNothingType)      { false }
      else if(b.isValueType)   { false }
      else                     { true  }
    }
    else if(a.isNothingType) { // known to be Nothing
      true
    }
    else if(a.isUnitType) {
      b.isUnitType
    }
    else if(a.hasObjectSort) { // may be null
      if(a.isNothingType)      { true  }
      else if(b.hasObjectSort) { exemplars.get(a).isSubtypeOf(b) }
      else if(b.isArray)       { a.isNullType } // documentation only, because `if(a.isNullType)` (above) covers this case already.
      else                     { false }
    }
    else {

        def msg = "(a: " + a + ", b: " + b + ")"

      assert(a.isNonUnitValueType, "a isn't a non-Unit value type. " + msg)
      assert(b.isValueType, "b isn't a value type. " + msg)

      (a eq b) || (a match {
        case BOOL | BYTE | SHORT | CHAR => b == INT || b == LONG // TODO Actually, BOOL does NOT conform to LONG. Even with adapt().
        case _                          => a == b
      })
    }
  }

  /* The maxValueType of (Char, Byte) and of (Char, Short) is Int, to encompass the negative values of Byte and Short. See ticket #2087.
   *
   * can-multi-thread
   */
  def maxValueType(a: BType, other: BType): BType = {
    assert(a.isValueType, "maxValueType() is defined only for 1st arg valuetypes (2nd arg doesn't matter).")

        def uncomparable: Nothing = {
          abort("Uncomparable BTypes: " + a.toString + " with " + other.toString)
        }

    if(a.isNothingType)      return other;
    if(other.isNothingType)  return a;
    if(a == other)           return a;

    a match {

      case UNIT => uncomparable
      case BOOL => uncomparable

      case BYTE =>
        if (other == CHAR)            INT
        else if(other.isNumericType)  other
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
        if(other.isIntegralType)   LONG
        else if(other.isRealType)  DOUBLE
        else                       uncomparable

      case FLOAT =>
        if (other == DOUBLE)          DOUBLE
        else if(other.isNumericType)  FLOAT
        else                          uncomparable

      case DOUBLE =>
        if(other.isNumericType)  DOUBLE
        else                     uncomparable

      case _ => uncomparable
    }
  }

  /* Takes promotions of numeric primitives into account.
   *
   *  can-multi-thread
   */
  final def maxType(a: BType, other: BType): BType = {
    if(a.isValueType) { maxValueType(a, other) }
    else {
      if(a.isNothingType)     return other;
      if(other.isNothingType) return a;
      if(a == other)          return a;
       // Approximate `lub`. The common type of two references is always AnyRef.
       // For 'real' least upper bound wrt to subclassing use method 'lub'.
      assert(a.isArray || a.isBoxed || a.hasObjectSort, "This is not a valuetype and it's not something else, what is it? " + a)
      // TODO For some reason, ICode thinks `REFERENCE(...).maxType(BOXED(whatever))` is `uncomparable`. Here, that has maxType AnyRefReference.
      //      BTW, when swapping arguments, ICode says BOXED(whatever).maxType(REFERENCE(...)) == AnyRefReference, so I guess the above was an oversight in REFERENCE.maxType()
      if(other.isRefOrArrayType) { AnyRefReference }
      else                       { abort("Uncomparable BTypes: " + a.toString + " with " + other.toString) }
    }
  }

  /*
   * Whether the argument (the signature of a method) takes as argument
   * one ore more Function or PartialFunction (in particular an anonymous closure).
   *
   * can-multi-thread
   */
  final def isHigherOrderMethod(mtype: BType): Boolean = {
    assert(mtype.sort == BType.METHOD)

    val ats = mtype.getArgumentTypes
    var idx = 0
    while(idx < ats.length) {
      val t = ats(idx)
      if(isFunctionType(t) || isPartialFunctionType(t)) {
        return true
      }
      idx += 1
    }
    false
  }

  /*
   *  Whether the argument is a subtype of
   *    scala.PartialFunction[-A, +B] extends (A => B)
   *  N.B.: this method returns true for a scala.runtime.AbstractPartialFunction
   *
   *  can-multi-thread
   */
  def isPartialFunctionType(t: BType): Boolean = {
    (t.hasObjectSort) && exemplars.get(t).isSubtypeOf(PartialFunctionReference)
  }

  /*
   *  Whether the argument is a subtype of
   *    scala.runtime.AbstractPartialFunction[-T1, +R] extends Function1[T1, R] with PartialFunction[T1, R]
   *
   *  can-multi-thread
   */
  def isAbstractPartialFunctionType(t: BType): Boolean = {
    (t.hasObjectSort) && exemplars.get(t).isSubtypeOf(AbstractPartialFunctionReference)
  }

  /*
   *  Whether the argument is a subtype of scala.FunctionX where 0 <= X <= definitions.MaxFunctionArity
   *
   *  can-multi-thread
   */
  def isFunctionType(t: BType): Boolean = {
    if(!t.hasObjectSort) return false
    var idx = 0
    val et: Tracked = exemplars.get(t)
    while(idx <= definitions.MaxFunctionArity) {
      if(et.isSubtypeOf(FunctionReference(idx).c)) {
        return true
      }
      idx += 1
    }
    false
  }

  /*
   *  Whether the argument is a subtype of scala.runtime.AbstractFunctionX where 0 <= X <= definitions.MaxFunctionArity
   *
   *  can-multi-thread
   */
  def isAbstractFunctionType(t: BType): Boolean = {
    if(!t.hasObjectSort) return false
    var idx = 0
    val et: Tracked = exemplars.get(t)
    while(idx <= definitions.MaxFunctionArity) {
      if(et.isSubtypeOf(AbstractFunctionReference(idx).c)) {
        return true
      }
      idx += 1
    }
    false
  }

  /*
   *  For an argument of exactly one of the types
   *  scala.runtime.AbstractFunctionX where 0 <= X <= definitions.MaxFunctionArity
   *  returns the function arity, -1 otherwise.
   *
   *  can-multi-thread
   */
  def abstractFunctionArity(t: BType): Int = {
    abstractFunctionArityMap.getOrElse(t, -1)
  }

  /* Just a namespace for utilities that encapsulate MethodVisitor idioms.
   *  In the ASM world, org.objectweb.asm.commons.InstructionAdapter plays a similar role,
   *  but the methods here allow choosing when to transition from ICode to ASM types
   *  (including not at all, e.g. for performance).
   */
  abstract class JCodeMethodN {

    def jmethod: asm.MethodVisitor

    import asm.Opcodes;
    import icodes.opcodes.{ InvokeStyle, Static, Dynamic,  SuperCall }

    final def emit(opc: Int) { jmethod.visitInsn(opc) }

    /*
     * can-multi-thread
     */
    final def genPrimitiveArithmetic(op: icodes.ArithmeticOp, kind: BType) {

      import icodes.{ ADD, SUB, MUL, DIV, REM, NOT }

      op match {

        case ADD => add(kind)
        case SUB => sub(kind)
        case MUL => mul(kind)
        case DIV => div(kind)
        case REM => rem(kind)

        case NOT =>
          if(kind.isIntSizedType) {
            emit(Opcodes.ICONST_M1)
            emit(Opcodes.IXOR)
          } else if(kind == LONG) {
            jmethod.visitLdcInsn(new java.lang.Long(-1))
            jmethod.visitInsn(Opcodes.LXOR)
          } else {
            abort("Impossible to negate an " + kind)
          }

        case _ =>
          abort("Unknown arithmetic primitive " + op)
      }

    } // end of method genPrimitiveArithmetic()

    /*
     * can-multi-thread
     */
    final def genPrimitiveLogical(op: /* LogicalOp */ Int, kind: BType) {

      import scalaPrimitives.{ AND, OR, XOR }

      ((op, kind): @unchecked) match {
        case (AND, LONG) => emit(Opcodes.LAND)
        case (AND, INT)  => emit(Opcodes.IAND)
        case (AND, _)    =>
          emit(Opcodes.IAND)
          if (kind != BOOL) { emitT2T(INT, kind) }

        case (OR, LONG) => emit(Opcodes.LOR)
        case (OR, INT)  => emit(Opcodes.IOR)
        case (OR, _) =>
          emit(Opcodes.IOR)
          if (kind != BOOL) { emitT2T(INT, kind) }

        case (XOR, LONG) => emit(Opcodes.LXOR)
        case (XOR, INT)  => emit(Opcodes.IXOR)
        case (XOR, _) =>
          emit(Opcodes.IXOR)
          if (kind != BOOL) { emitT2T(INT, kind) }
      }

    } // end of method genPrimitiveLogical()

    /*
     * can-multi-thread
     */
    final def genPrimitiveShift(op: /* ShiftOp */ Int, kind: BType) {

      import scalaPrimitives.{ LSL, ASR, LSR }

      ((op, kind): @unchecked) match {
        case (LSL, LONG) => emit(Opcodes.LSHL)
        case (LSL, INT)  => emit(Opcodes.ISHL)
        case (LSL, _) =>
          emit(Opcodes.ISHL)
          emitT2T(INT, kind)

        case (ASR, LONG) => emit(Opcodes.LSHR)
        case (ASR, INT)  => emit(Opcodes.ISHR)
        case (ASR, _) =>
          emit(Opcodes.ISHR)
          emitT2T(INT, kind)

        case (LSR, LONG) => emit(Opcodes.LUSHR)
        case (LSR, INT)  => emit(Opcodes.IUSHR)
        case (LSR, _) =>
          emit(Opcodes.IUSHR)
          emitT2T(INT, kind)
      }

    } // end of method genPrimitiveShift()

    /*
     * can-multi-thread
     */
    final def genPrimitiveComparison(op: icodes.ComparisonOp, kind: BType) {

      import icodes.{ CMPL, CMP, CMPG }

      ((op, kind): @unchecked) match {
        case (CMP,  LONG)   => emit(Opcodes.LCMP)
        case (CMPL, FLOAT)  => emit(Opcodes.FCMPL)
        case (CMPG, FLOAT)  => emit(Opcodes.FCMPG)
        case (CMPL, DOUBLE) => emit(Opcodes.DCMPL)
        case (CMPG, DOUBLE) => emit(Opcodes.DCMPL) // http://docs.oracle.com/javase/specs/jvms/se5.0/html/Instructions2.doc3.html
      }

    } // end of method genPrimitiveComparison()

    /*
     * can-multi-thread
     */
    final def genStartConcat {
      jmethod.visitTypeInsn(Opcodes.NEW, StringBuilderClassName)
      jmethod.visitInsn(Opcodes.DUP)
      invokespecial(
        StringBuilderClassName,
        INSTANCE_CONSTRUCTOR_NAME,
        "()V"
      )
    }

    /*
     * can-multi-thread
     */
    final def genStringConcat(el: BType) {

      val jtype =
        if(el.isArray || el.hasObjectSort) JAVA_LANG_OBJECT
        else el;

      val bt = BType.getMethodType(StringBuilderReference, Array(jtype))

      invokevirtual(StringBuilderClassName, "append", bt.getDescriptor)
    }

    /*
     * can-multi-thread
     */
    final def genEndConcat {
      invokevirtual(StringBuilderClassName, "toString", "()Ljava/lang/String;")
    }

    /*
     * Emits one or more conversion instructions based on the types given as arguments.
     *
     * @param from The type of the value to be converted into another type.
     * @param to   The type the value will be converted into.
     *
     * can-multi-thread
     */
    final def emitT2T(from: BType, to: BType) {

      assert(
        from.isNonUnitValueType && to.isNonUnitValueType,
        s"Cannot emit primitive conversion from $from to $to"
      )

          def pickOne(opcs: Array[Int]) { // TODO index on to.sort
            val chosen = (to: @unchecked) match {
              case BYTE   => opcs(0)
              case SHORT  => opcs(1)
              case CHAR   => opcs(2)
              case INT    => opcs(3)
              case LONG   => opcs(4)
              case FLOAT  => opcs(5)
              case DOUBLE => opcs(6)
            }
            if(chosen != -1) { emit(chosen) }
          }

      if(from == to) { return }
      // the only conversion involving BOOL that is allowed is (BOOL -> BOOL)
      assert(from != BOOL && to != BOOL, s"inconvertible types : $from -> $to")

      // We're done with BOOL already
      (from.sort: @switch) match {

        // using `asm.Type.SHORT` instead of `BType.SHORT` because otherwise "warning: could not emit switch for @switch annotated match"

        case asm.Type.BYTE  => pickOne(JCodeMethodN.fromByteT2T)
        case asm.Type.SHORT => pickOne(JCodeMethodN.fromShortT2T)
        case asm.Type.CHAR  => pickOne(JCodeMethodN.fromCharT2T)
        case asm.Type.INT   => pickOne(JCodeMethodN.fromIntT2T)

        case asm.Type.FLOAT  =>
          import asm.Opcodes.{ F2L, F2D, F2I }
          (to.sort: @switch) match {
            case asm.Type.LONG    => emit(F2L)
            case asm.Type.DOUBLE  => emit(F2D)
            case _                => emit(F2I); emitT2T(INT, to)
          }

        case asm.Type.LONG   =>
          import asm.Opcodes.{ L2F, L2D, L2I }
          (to.sort: @switch) match {
            case asm.Type.FLOAT   => emit(L2F)
            case asm.Type.DOUBLE  => emit(L2D)
            case _             => emit(L2I); emitT2T(INT, to)
          }

        case asm.Type.DOUBLE =>
          import asm.Opcodes.{ D2L, D2F, D2I }
          (to.sort: @switch) match {
            case asm.Type.FLOAT   => emit(D2F)
            case asm.Type.LONG    => emit(D2L)
            case _             => emit(D2I); emitT2T(INT, to)
          }
      }
    } // end of emitT2T()

    // can-multi-thread
    final def aconst(cst: AnyRef) {
      if (cst == null) { emit(Opcodes.ACONST_NULL) }
      else             { jmethod.visitLdcInsn(cst) }
    }

    // can-multi-thread
    final def boolconst(b: Boolean) { iconst(if(b) 1 else 0) }

    // can-multi-thread
    final def iconst(cst: Int) {
      if (cst >= -1 && cst <= 5) {
        emit(Opcodes.ICONST_0 + cst)
      } else if (cst >= java.lang.Byte.MIN_VALUE && cst <= java.lang.Byte.MAX_VALUE) {
        jmethod.visitIntInsn(Opcodes.BIPUSH, cst)
      } else if (cst >= java.lang.Short.MIN_VALUE && cst <= java.lang.Short.MAX_VALUE) {
        jmethod.visitIntInsn(Opcodes.SIPUSH, cst)
      } else {
        jmethod.visitLdcInsn(new Integer(cst))
      }
    }

    // can-multi-thread
    final def lconst(cst: Long) {
      if (cst == 0L || cst == 1L) {
        emit(Opcodes.LCONST_0 + cst.asInstanceOf[Int])
      } else {
        jmethod.visitLdcInsn(new java.lang.Long(cst))
      }
    }

    // can-multi-thread
    final def fconst(cst: Float) {
      val bits: Int = java.lang.Float.floatToIntBits(cst)
      if (bits == 0L || bits == 0x3f800000 || bits == 0x40000000) { // 0..2
        emit(Opcodes.FCONST_0 + cst.asInstanceOf[Int])
      } else {
        jmethod.visitLdcInsn(new java.lang.Float(cst))
      }
    }

    // can-multi-thread
    final def dconst(cst: Double) {
      val bits: Long = java.lang.Double.doubleToLongBits(cst)
      if (bits == 0L || bits == 0x3ff0000000000000L) { // +0.0d and 1.0d
        emit(Opcodes.DCONST_0 + cst.asInstanceOf[Int])
      } else {
        jmethod.visitLdcInsn(new java.lang.Double(cst))
      }
    }

    // can-multi-thread
    final def newarray(elem: BType) {
      if(elem.isRefOrArrayType || elem.isPhantomType ) {
        /* phantom type at play in `Array(null)`, SI-1513. On the other hand, Array(()) has element type `scala.runtime.BoxedUnit` which hasObjectSort. */
        jmethod.visitTypeInsn(Opcodes.ANEWARRAY, elem.getInternalName)
      } else {
        val rand = {
          // using `asm.Type.SHORT` instead of `BType.SHORT` because otherwise "warning: could not emit switch for @switch annotated match"
          (elem.sort: @switch) match {
            case asm.Type.BOOLEAN => Opcodes.T_BOOLEAN
            case asm.Type.BYTE    => Opcodes.T_BYTE
            case asm.Type.SHORT   => Opcodes.T_SHORT
            case asm.Type.CHAR    => Opcodes.T_CHAR
            case asm.Type.INT     => Opcodes.T_INT
            case asm.Type.LONG    => Opcodes.T_LONG
            case asm.Type.FLOAT   => Opcodes.T_FLOAT
            case asm.Type.DOUBLE  => Opcodes.T_DOUBLE
          }
        }
        jmethod.visitIntInsn(Opcodes.NEWARRAY, rand)
      }
    }


    final def load( idx: Int, tk: BType) { emitVarInsn(Opcodes.ILOAD,  idx, tk) } // can-multi-thread
    final def store(idx: Int, tk: BType) { emitVarInsn(Opcodes.ISTORE, idx, tk) } // can-multi-thread

    final def aload( tk: BType) { emitTypeBased(JCodeMethodN.aloadOpcodes,  tk) } // can-multi-thread
    final def astore(tk: BType) { emitTypeBased(JCodeMethodN.astoreOpcodes, tk) } // can-multi-thread

    final def neg(tk: BType) { emitPrimitive(JCodeMethodN.negOpcodes, tk) } // can-multi-thread
    final def add(tk: BType) { emitPrimitive(JCodeMethodN.addOpcodes, tk) } // can-multi-thread
    final def sub(tk: BType) { emitPrimitive(JCodeMethodN.subOpcodes, tk) } // can-multi-thread
    final def mul(tk: BType) { emitPrimitive(JCodeMethodN.mulOpcodes, tk) } // can-multi-thread
    final def div(tk: BType) { emitPrimitive(JCodeMethodN.divOpcodes, tk) } // can-multi-thread
    final def rem(tk: BType) { emitPrimitive(JCodeMethodN.remOpcodes, tk) } // can-multi-thread

    // can-multi-thread
    final def invokespecial(owner: String, name: String, desc: String) {
      jmethod.visitMethodInsn(Opcodes.INVOKESPECIAL, owner, name, desc)
    }
    // can-multi-thread
    final def invokestatic(owner: String, name: String, desc: String) {
      jmethod.visitMethodInsn(Opcodes.INVOKESTATIC, owner, name, desc)
    }
    // can-multi-thread
    final def invokeinterface(owner: String, name: String, desc: String) {
      jmethod.visitMethodInsn(Opcodes.INVOKEINTERFACE, owner, name, desc)
    }
    // can-multi-thread
    final def invokevirtual(owner: String, name: String, desc: String) {
      jmethod.visitMethodInsn(Opcodes.INVOKEVIRTUAL, owner, name, desc)
    }

    // can-multi-thread
    final def goTo(label: asm.Label) { jmethod.visitJumpInsn(Opcodes.GOTO, label) }
    // can-multi-thread
    final def emitIF(cond: icodes.TestOp, label: asm.Label)      { jmethod.visitJumpInsn(cond.opcodeIF,     label) }
    // can-multi-thread
    final def emitIF_ICMP(cond: icodes.TestOp, label: asm.Label) { jmethod.visitJumpInsn(cond.opcodeIFICMP, label) }
    // can-multi-thread
    final def emitIF_ACMP(cond: icodes.TestOp, label: asm.Label) {
      assert((cond == icodes.EQ) || (cond == icodes.NE), cond)
      val opc = (if(cond == icodes.EQ) Opcodes.IF_ACMPEQ else Opcodes.IF_ACMPNE)
      jmethod.visitJumpInsn(opc, label)
    }
    // can-multi-thread
    final def emitIFNONNULL(label: asm.Label) { jmethod.visitJumpInsn(Opcodes.IFNONNULL, label) }
    // can-multi-thread
    final def emitIFNULL   (label: asm.Label) { jmethod.visitJumpInsn(Opcodes.IFNULL,    label) }

    // can-multi-thread
    final def emitRETURN(tk: BType) {
      if(tk == UNIT) { emit(Opcodes.RETURN) }
      else           { emitTypeBased(JCodeMethodN.returnOpcodes, tk)      }
    }

    /* Emits one of tableswitch or lookoupswitch.
     *
     * can-multi-thread
     */
    final def emitSWITCH(keys: Array[Int], branches: Array[asm.Label], defaultBranch: asm.Label, minDensity: Double) {
      assert(keys.length == branches.length)

      // For empty keys, it makes sense emitting LOOKUPSWITCH with defaultBranch only.
      // Similar to what javac emits for a switch statement consisting only of a default case.
      if (keys.length == 0) {
        jmethod.visitLookupSwitchInsn(defaultBranch, keys, branches)
        return
      }

      // sort `keys` by increasing key, keeping `branches` in sync. TODO FIXME use quicksort
      var i = 1
      while (i < keys.length) {
        var j = 1
        while (j <= keys.length - i) {
          if (keys(j) < keys(j - 1)) {
            val tmp     = keys(j)
            keys(j)     = keys(j - 1)
            keys(j - 1) = tmp
            val tmpL        = branches(j)
            branches(j)     = branches(j - 1)
            branches(j - 1) = tmpL
          }
          j += 1
        }
        i += 1
      }

      // check for duplicate keys to avoid "VerifyError: unsorted lookupswitch" (SI-6011)
      i = 1
      while (i < keys.length) {
        if(keys(i-1) == keys(i)) {
          abort("duplicate keys in SWITCH, can't pick arbitrarily one of them to evict, see SI-6011.")
        }
        i += 1
      }

      val keyMin = keys(0)
      val keyMax = keys(keys.length - 1)

      val isDenseEnough: Boolean = {
        /* Calculate in long to guard against overflow. TODO what overflow? */
        val keyRangeD: Double = (keyMax.asInstanceOf[Long] - keyMin + 1).asInstanceOf[Double]
        val klenD:     Double = keys.length
        val kdensity:  Double = (klenD / keyRangeD)

        kdensity >= minDensity
      }

      if (isDenseEnough) {
        // use a table in which holes are filled with defaultBranch.
        val keyRange    = (keyMax - keyMin + 1)
        val newBranches = new Array[asm.Label](keyRange)
        var oldPos = 0
        var i = 0
        while(i < keyRange) {
          val key = keyMin + i;
          if (keys(oldPos) == key) {
            newBranches(i) = branches(oldPos)
            oldPos += 1
          } else {
            newBranches(i) = defaultBranch
          }
          i += 1
        }
        assert(oldPos == keys.length, "emitSWITCH")
        jmethod.visitTableSwitchInsn(keyMin, keyMax, defaultBranch, newBranches: _*)
      } else {
        jmethod.visitLookupSwitchInsn(defaultBranch, keys, branches)
      }
    }

    // internal helpers -- not part of the public API of `jcode`
    // don't make private otherwise inlining will suffer

    // can-multi-thread
    final def emitVarInsn(opc: Int, idx: Int, tk: BType) {
      assert((opc == Opcodes.ILOAD) || (opc == Opcodes.ISTORE), opc)
      jmethod.visitVarInsn(tk.getOpcode(opc), idx)
    }

    // ---------------- array load and store ----------------

    // can-multi-thread
    final def emitTypeBased(opcs: Array[Int], tk: BType) {
      assert(tk != UNIT, tk)
      val opc = {
        if(tk.isRefOrArrayType) {  opcs(0) }
        else if(tk.isIntSizedType) {
          (tk: @unchecked) match {
            case BOOL | BYTE     => opcs(1)
            case SHORT           => opcs(2)
            case CHAR            => opcs(3)
            case INT             => opcs(4)
          }
        } else {
          (tk: @unchecked) match {
            case LONG            => opcs(5)
            case FLOAT           => opcs(6)
            case DOUBLE          => opcs(7)
          }
        }
      }
      emit(opc)
    }

    // ---------------- primitive operations ----------------

     // can-multi-thread
    final def emitPrimitive(opcs: Array[Int], tk: BType) {
      val opc = {
        // using `asm.Type.SHORT` instead of `BType.SHORT` because otherwise "warning: could not emit switch for @switch annotated match"
        (tk.sort: @switch) match {
          case asm.Type.LONG   => opcs(1)
          case asm.Type.FLOAT  => opcs(2)
          case asm.Type.DOUBLE => opcs(3)
          case _ => opcs(0)
        }
      }
      emit(opc)
    }

    // can-multi-thread
    final def drop(tk: BType) { emit(if(tk.isWideType) Opcodes.POP2 else Opcodes.POP) }

    // can-multi-thread
    final def dup(tk: BType)  { emit(if(tk.isWideType) Opcodes.DUP2 else Opcodes.DUP) }

    // ---------------- type checks and casts ----------------

    // can-multi-thread
    final def isInstance(tk: BType) {
      jmethod.visitTypeInsn(Opcodes.INSTANCEOF, tk.getInternalName)
    }

    // can-multi-thread
    final def checkCast(tk: BType) {
      assert(tk.isRefOrArrayType, "checkcast on primitive type: " + tk)
      // TODO ICode also requires: but that's too much, right? assert(!isBoxedType(tk),     "checkcast on boxed type: " + tk)
      jmethod.visitTypeInsn(Opcodes.CHECKCAST, tk.getInternalName)
    }

  } // end of class JCodeMethodN

  /* Constant-valued val-members of JCodeMethodN at the companion object, so as to avoid re-initializing them multiple times. */
  object JCodeMethodN {

    import asm.Opcodes._

    // ---------------- conversions ----------------

    val fromByteT2T  = { Array( -1,  -1, I2C,  -1, I2L, I2F, I2D) } // do nothing for (BYTE -> SHORT) and for (BYTE -> INT)
    val fromCharT2T  = { Array(I2B, I2S,  -1,  -1, I2L, I2F, I2D) } // for (CHAR  -> INT) do nothing
    val fromShortT2T = { Array(I2B,  -1, I2C,  -1, I2L, I2F, I2D) } // for (SHORT -> INT) do nothing
    val fromIntT2T   = { Array(I2B, I2S, I2C,  -1, I2L, I2F, I2D) }

    // ---------------- array load and store ----------------

    val aloadOpcodes  = { Array(AALOAD,  BALOAD,  SALOAD,  CALOAD,  IALOAD,  LALOAD,  FALOAD,  DALOAD)  }
    val astoreOpcodes = { Array(AASTORE, BASTORE, SASTORE, CASTORE, IASTORE, LASTORE, FASTORE, DASTORE) }
    val returnOpcodes = { Array(ARETURN, IRETURN, IRETURN, IRETURN, IRETURN, LRETURN, FRETURN, DRETURN) }

    // ---------------- primitive operations ----------------

    val negOpcodes: Array[Int] = { Array(INEG, LNEG, FNEG, DNEG) }
    val addOpcodes: Array[Int] = { Array(IADD, LADD, FADD, DADD) }
    val subOpcodes: Array[Int] = { Array(ISUB, LSUB, FSUB, DSUB) }
    val mulOpcodes: Array[Int] = { Array(IMUL, LMUL, FMUL, DMUL) }
    val divOpcodes: Array[Int] = { Array(IDIV, LDIV, FDIV, DDIV) }
    val remOpcodes: Array[Int] = { Array(IREM, LREM, FREM, DREM) }

  } // end of object JCodeMethodN

  // ---------------- adapted from scalaPrimitives ----------------

  /* Given `code` reports the src TypeKind of the coercion indicated by `code`.
   * To find the dst TypeKind, `ScalaPrimitives.generatedKind(code)` can be used.
   *
   * can-multi-thread
   */
  final def coercionFrom(code: Int): BType = {
    import scalaPrimitives._
    (code: @switch) match {
      case B2B | B2C | B2S | B2I | B2L | B2F | B2D => BYTE
      case S2B | S2S | S2C | S2I | S2L | S2F | S2D => SHORT
      case C2B | C2S | C2C | C2I | C2L | C2F | C2D => CHAR
      case I2B | I2S | I2C | I2I | I2L | I2F | I2D => INT
      case L2B | L2S | L2C | L2I | L2L | L2F | L2D => LONG
      case F2B | F2S | F2C | F2I | F2L | F2F | F2D => FLOAT
      case D2B | D2S | D2C | D2I | D2L | D2F | D2D => DOUBLE
    }
  }

  /* If code is a coercion primitive, the result type.
   *
   * can-multi-thread
   */
  final def coercionTo(code: Int): BType = {
    import scalaPrimitives._
    (code: @scala.annotation.switch) match {
      case B2B | C2B | S2B | I2B | L2B | F2B | D2B => BYTE
      case B2C | C2C | S2C | I2C | L2C | F2C | D2C => CHAR
      case B2S | C2S | S2S | I2S | L2S | F2S | D2S => SHORT
      case B2I | C2I | S2I | I2I | L2I | F2I | D2I => INT
      case B2L | C2L | S2L | I2L | L2L | F2L | D2L => LONG
      case B2F | C2F | S2F | I2F | L2F | F2F | D2F => FLOAT
      case B2D | C2D | S2D | I2D | L2D | F2D | D2D => DOUBLE
    }
  }

  final val typeOfArrayOp: Map[Int, BType] = {
    import scalaPrimitives._
    Map(
      (List(ZARRAY_LENGTH, ZARRAY_GET, ZARRAY_SET) map (_ -> BOOL))   ++
      (List(BARRAY_LENGTH, BARRAY_GET, BARRAY_SET) map (_ -> BYTE))   ++
      (List(SARRAY_LENGTH, SARRAY_GET, SARRAY_SET) map (_ -> SHORT))  ++
      (List(CARRAY_LENGTH, CARRAY_GET, CARRAY_SET) map (_ -> CHAR))   ++
      (List(IARRAY_LENGTH, IARRAY_GET, IARRAY_SET) map (_ -> INT))    ++
      (List(LARRAY_LENGTH, LARRAY_GET, LARRAY_SET) map (_ -> LONG))   ++
      (List(FARRAY_LENGTH, FARRAY_GET, FARRAY_SET) map (_ -> FLOAT))  ++
      (List(DARRAY_LENGTH, DARRAY_GET, DARRAY_SET) map (_ -> DOUBLE)) ++
      (List(OARRAY_LENGTH, OARRAY_GET, OARRAY_SET) map (_ -> ObjectReference)) : _*
    )
  }

  val INNER_CLASSES_FLAGS =
    (asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_PRIVATE   | asm.Opcodes.ACC_PROTECTED |
     asm.Opcodes.ACC_STATIC | asm.Opcodes.ACC_INTERFACE | asm.Opcodes.ACC_ABSTRACT)

  // ---------------- InnerClasses attribute (JVMS 4.7.6) ----------------

  /*
   * @param name the internal name of an inner class.
   * @param outerName the internal name of the class to which the inner class belongs.
   *                  May be `null` for non-member inner classes (ie for a Java local class or a Java anonymous class).
   * @param innerName the (simple) name of the inner class inside its enclosing class. It's `null` for anonymous inner classes.
   * @param access the access flags of the inner class as originally declared in the enclosing class.
   */
  case class InnerClassEntry(name: String, outerName: String, innerName: String, access: Int) {
    assert(name != null, "Null isn't good as class name in an InnerClassEntry.")
  }

  /* For given symbol return a symbol corresponding to a class that should be declared as inner class.
   *
   *  For example:
   *  class A {
   *    class B
   *    object C
   *  }
   *
   *  then method will return:
   *    NoSymbol for A,
   *    the same symbol for A.B (corresponding to A$B class), and
   *    A$C$ symbol for A.C.
   *
   * must-single-thread
   */
  private def innerClassSymbolFor(s: Symbol): Symbol =
    if (s.isClass) s else if (s.isModule) s.moduleClass else NoSymbol

  /*
   *  Computes the chain of inner-class (over the is-member-of relation) for the given argument.
   *  The resulting chain will be cached in `exemplars`.
   *
   *  The chain thus cached is valid during this compiler run, see in contrast
   *  `innerClassBufferASM` for a cache that is valid only for the class being emitted.
   *
   *  The argument can be any symbol, but given that this method is invoked only from `buildExemplar()`,
   *  in practice it has been vetted to be a class-symbol.
   *
   *  Returns:
   *
   *    - a non-empty array of entries for an inner-class argument.
   *      The array's first element is the outermost top-level class,
   *      the array's last element corresponds to csym.
   *
   *    - null otherwise.
   *
   *  This method does not add to `innerClassBufferASM`, use instead `exemplar()` for that.
   *
   *  must-single-thread
   */
  final def saveInnerClassesFor(csym: Symbol, csymTK: BType): Array[InnerClassEntry] = {

    val ics = innerClassSymbolFor(csym)
    if(ics == NoSymbol) {
      return null
    }
    assert(ics == csym, "Disagreement between innerClassSymbolFor() and exemplar()'s tracked symbol for the same input: " + csym.fullName)

    var chain: List[Symbol] = Nil
    var x = ics
    while(x ne NoSymbol) {
      assert(x.isClass, "not a class symbol: " + x.fullName)
      val isInner = !x.rawowner.isPackageClass
      if (isInner) {
        chain ::= x
        x = innerClassSymbolFor(x.rawowner)
      } else {
        x = NoSymbol
      }
    }

    // now that we have all of `ics` , `csym` , and soon the inner-classes-chain, it's too tempting not to cache.
    if(chain.isEmpty) { null }
    else {
      val arr = new Array[InnerClassEntry](chain.size)
      (chain map toInnerClassEntry).copyToArray(arr)

      arr
    }
  }

  /*
   * must-single-thread
   */
  private def toInnerClassEntry(innerSym: Symbol): InnerClassEntry = {

        /* The outer name for this inner class. Note that it returns null
         *  when the inner class should not get an index in the constant pool.
         *  That means non-member classes (anonymous). See Section 4.7.5 in the JVMS.
         */
        def outerName(innerSym: Symbol): Name = {
          if (innerSym.originalEnclosingMethod != NoSymbol)
            null
          else {
            val outerName = innerSym.rawowner.javaBinaryName
            if (isTopLevelModule(innerSym.rawowner)) nme.stripModuleSuffix(outerName)
            else outerName
          }
        }

        def innerName(innerSym: Symbol): String = {
          if (innerSym.isAnonymousClass || innerSym.isAnonymousFunction)
            null
          else
            innerSym.rawname + innerSym.moduleSuffix
        }

    val access = mkFlags(
      if (innerSym.rawowner.hasModuleFlag) asm.Opcodes.ACC_STATIC else 0,
      javaFlags(innerSym),
      if(isDeprecated(innerSym)) asm.Opcodes.ACC_DEPRECATED else 0 // ASM pseudo-access flag
    ) & (INNER_CLASSES_FLAGS | asm.Opcodes.ACC_DEPRECATED)

    val jname = innerSym.javaBinaryName.toString // never null
    val oname = { // null when method-enclosed
      val on = outerName(innerSym)
      if(on == null) null else on.toString
    }
    val iname = { // null for anonymous inner class
      val in = innerName(innerSym)
      if(in == null) null else in.toString
    }

    InnerClassEntry(jname, oname, iname, access)
  }

  var pickledBytes = 0 // statistics

  /*
   * can-multi-thread
   */
  def mkFlags(args: Int*) = args.foldLeft(0)(_ | _)

  /*
   * can-multi-thread
   */
  final def hasPublicBitSet(flags: Int) = ((flags & asm.Opcodes.ACC_PUBLIC) != 0)

  /*
   * must-single-thread
   */
  final def isRemote(s: Symbol) = (s hasAnnotation definitions.RemoteAttr)

  /*
   * Return the Java modifiers for the given symbol.
   * Java modifiers for classes:
   *  - public, abstract, final, strictfp (not used)
   * for interfaces:
   *  - the same as for classes, without 'final'
   * for fields:
   *  - public, private (*)
   *  - static, final
   * for methods:
   *  - the same as for fields, plus:
   *  - abstract, synchronized (not used), strictfp (not used), native (not used)
   *
   *  (*) protected cannot be used, since inner classes 'see' protected members,
   *      and they would fail verification after lifted.
   *
   * must-single-thread
   */
  def javaFlags(sym: Symbol): Int = {
    // constructors of module classes should be private
    // PP: why are they only being marked private at this stage and not earlier?
    val privateFlag =
      sym.isPrivate || (sym.isPrimaryConstructor && isTopLevelModule(sym.owner))

    // Final: the only fields which can receive ACC_FINAL are eager vals.
    // Neither vars nor lazy vals can, because:
    //
    // Source: http://docs.oracle.com/javase/specs/jls/se7/html/jls-17.html#jls-17.5.3
    // "Another problem is that the specification allows aggressive
    // optimization of final fields. Within a thread, it is permissible to
    // reorder reads of a final field with those modifications of a final
    // field that do not take place in the constructor."
    //
    // A var or lazy val which is marked final still has meaning to the
    // scala compiler. The word final is heavily overloaded unfortunately;
    // for us it means "not overridable". At present you can't override
    // vars regardless; this may change.
    //
    // The logic does not check .isFinal (which checks flags for the FINAL flag,
    // and includes symbols marked lateFINAL) instead inspecting rawflags so
    // we can exclude lateFINAL. Such symbols are eligible for inlining, but to
    // avoid breaking proxy software which depends on subclassing, we do not
    // emit ACC_FINAL.
    // Nested objects won't receive ACC_FINAL in order to allow for their overriding.

    val finalFlag = (
         (((sym.rawflags & symtab.Flags.FINAL) != 0) || isTopLevelModule(sym))
      && !sym.enclClass.isInterface
      && !sym.isClassConstructor
      && !sym.isMutable // lazy vals and vars both
    )

    // Primitives are "abstract final" to prohibit instantiation
    // without having to provide any implementations, but that is an
    // illegal combination of modifiers at the bytecode level so
    // suppress final if abstract if present.
    import asm.Opcodes._
    mkFlags(
      if (privateFlag) ACC_PRIVATE else ACC_PUBLIC,
      if (sym.isDeferred || sym.hasAbstractFlag) ACC_ABSTRACT else 0,
      if (sym.isInterface) ACC_INTERFACE else 0,
      if (finalFlag && !sym.hasAbstractFlag) ACC_FINAL else 0,
      if (sym.isStaticMember) ACC_STATIC else 0,
      if (sym.isBridge) ACC_BRIDGE | ACC_SYNTHETIC else 0,
      if (sym.isArtifact) ACC_SYNTHETIC else 0,
      if (sym.isClass && !sym.isInterface) ACC_SUPER else 0,
      if (sym.isVarargsMethod) ACC_VARARGS else 0,
      if (sym.hasFlag(symtab.Flags.SYNCHRONIZED)) ACC_SYNCHRONIZED else 0
    )
  }

  /*
   * must-single-thread
   */
  def javaFieldFlags(sym: Symbol) = {
    javaFlags(sym) | mkFlags(
      if (sym hasAnnotation definitions.TransientAttr) asm.Opcodes.ACC_TRANSIENT else 0,
      if (sym hasAnnotation definitions.VolatileAttr)  asm.Opcodes.ACC_VOLATILE  else 0,
      if (sym.isMutable) 0 else asm.Opcodes.ACC_FINAL
    )
  }

  /*
   * must-single-thread
   */
  def isTopLevelModule(sym: Symbol): Boolean = {
    exitingPickler { sym.isModuleClass && !sym.isImplClass && !sym.isNestedClass }
  }

  /*
   * must-single-thread
   */
  def isStaticModule(sym: Symbol): Boolean = {
    sym.isModuleClass && !sym.isImplClass && !sym.isLifted
  }

  // -----------------------------------------------------------------------------------------
  // finding the least upper bound in agreement with the bytecode verifier (given two internal names handed by ASM)
  // Background:
  //  http://gallium.inria.fr/~xleroy/publi/bytecode-verification-JAR.pdf
  //  http://comments.gmane.org/gmane.comp.java.vm.languages/2293
  //  https://issues.scala-lang.org/browse/SI-3872
  // -----------------------------------------------------------------------------------------

  /*
   * can-multi-thread
   */
  def firstCommonSuffix(as: List[Tracked], bs: List[Tracked]): BType = {
    var chainA = as
    var chainB = bs
    var fcs: Tracked = null
    do {
      if      (chainB contains chainA.head) fcs = chainA.head
      else if (chainA contains chainB.head) fcs = chainB.head
      else {
        chainA = chainA.tail
        chainB = chainB.tail
      }
    } while(fcs == null)
    fcs.c
  }

  /*  An `asm.ClassWriter` that uses `jvmWiseLUB()`
   *  The internal name of the least common ancestor of the types given by inameA and inameB.
   *  It's what ASM needs to know in order to compute stack map frames, http://asm.ow2.org/doc/developer-guide.html#controlflow
   */
  final class CClassWriter(flags: Int) extends asm.ClassWriter(flags) {

    /*
     *  This method is thread re-entrant because chrs never grows during its operation (that's because all TypeNames being looked up have already been entered).
     *  To stress this point, rather than using `newTypeName()` we use `lookupTypeName()`
     *
     *  can-multi-thread
     */
    override def getCommonSuperClass(inameA: String, inameB: String): String = {
      val a = brefType(lookupTypeName(inameA.toCharArray))
      val b = brefType(lookupTypeName(inameB.toCharArray))
      val lca = jvmWiseLUB(a, b)
      val lcaName = lca.getInternalName // don't call javaName because that side-effects innerClassBuffer.
      assert(lcaName != "scala/Any")

      lcaName // ASM caches the answer during the lifetime of a ClassWriter. We outlive that. Not sure whether caching on our side would improve things.
    }

  }

  /*
   *  Finding the least upper bound in agreement with the bytecode verifier (given two internal names handed out by ASM)
   *  Background:
   *    http://gallium.inria.fr/~xleroy/publi/bytecode-verification-JAR.pdf
   *    http://comments.gmane.org/gmane.comp.java.vm.languages/2293
   *    https://issues.scala-lang.org/browse/SI-3872
   *
   *  can-multi-thread
   */
  def jvmWiseLUB(a: BType, b: BType): BType = {

    assert(a.isNonSpecial, "jvmWiseLUB() received a non-plain-class " + a)
    assert(b.isNonSpecial, "jvmWiseLUB() received a non-plain-class " + b)

    val ta = exemplars.get(a)
    val tb = exemplars.get(b)

    val res = Pair(ta.isInterface, tb.isInterface) match {
      case (true, true) =>
        // exercised by test/files/run/t4761.scala
        if      (tb.isSubtypeOf(ta.c)) ta.c
        else if (ta.isSubtypeOf(tb.c)) tb.c
        else ObjectReference
      case (true, false) =>
        if(tb.isSubtypeOf(a)) a else ObjectReference
      case (false, true) =>
        if(ta.isSubtypeOf(b)) b else ObjectReference
      case _ =>
        firstCommonSuffix(ta :: ta.superClasses, tb :: tb.superClasses)
    }
    assert(res.isNonSpecial, "jvmWiseLUB() returned a non-plain-class.")
    res
  }

  // -----------------------------------------------------------------------------------------
  // constants
  // -----------------------------------------------------------------------------------------

  val classfileVersion: Int = settings.target.value match {
    case "jvm-1.5"     => asm.Opcodes.V1_5
    case "jvm-1.6"     => asm.Opcodes.V1_6
    case "jvm-1.7"     => asm.Opcodes.V1_7
  }

  val majorVersion: Int = (classfileVersion & 0xFF)
  val emitStackMapFrame = (majorVersion >= 50)

  val extraProc: Int = mkFlags(
    asm.ClassWriter.COMPUTE_MAXS,
    if(emitStackMapFrame) asm.ClassWriter.COMPUTE_FRAMES else 0
  )

  val JAVA_LANG_OBJECT = brefType("java/lang/Object")
  val JAVA_LANG_STRING = brefType("java/lang/String")

  /*
   * must-single-thread
   */
  object isJavaEntryPoint {

    /*
     * must-single-thread
     */
    def apply(sym: Symbol, csymCompUnit: CompilationUnit): Boolean = {
      def fail(msg: String, pos: Position = sym.pos) = {
        csymCompUnit.warning(sym.pos,
          sym.name + " has a main method with parameter type Array[String], but " + sym.fullName('.') + " will not be a runnable program.\n" +
          "  Reason: " + msg
          // TODO: make this next claim true, if possible
          //   by generating valid main methods as static in module classes
          //   not sure what the jvm allows here
          // + "  You can still run the program by calling it as " + sym.javaSimpleName + " instead."
        )
        false
      }
      def failNoForwarder(msg: String) = {
        fail(msg + ", which means no static forwarder can be generated.\n")
      }
      val possibles = if (sym.hasModuleFlag) (sym.tpe nonPrivateMember nme.main).alternatives else Nil
      val hasApproximate = possibles exists { m =>
        m.info match {
          case MethodType(p :: Nil, _) => p.tpe.typeSymbol == definitions.ArrayClass
          case _                       => false
        }
      }
      // At this point it's a module with a main-looking method, so either succeed or warn that it isn't.
      hasApproximate && {
        // Before erasure so we can identify generic mains.
        enteringErasure {
          val companion     = sym.linkedClassOfClass

          if (definitions.hasJavaMainMethod(companion))
            failNoForwarder("companion contains its own main method")
          else if (companion.tpe.member(nme.main) != NoSymbol)
            // this is only because forwarders aren't smart enough yet
            failNoForwarder("companion contains its own main method (implementation restriction: no main is allowed, regardless of signature)")
          else if (companion.isTrait)
            failNoForwarder("companion is a trait")
          // Now either succeeed, or issue some additional warnings for things which look like
          // attempts to be java main methods.
        else (possibles exists definitions.isJavaMainMethod) || {
            possibles exists { m =>
              m.info match {
                case PolyType(_, _) =>
                  fail("main methods cannot be generic.")
                case MethodType(params, res) =>
                  if (res.typeSymbol :: params exists (_.isAbstractType))
                    fail("main methods cannot refer to type parameters or abstract types.", m.pos)
                  else
                    definitions.isJavaMainMethod(m) || fail("main method must have exact signature (Array[String])Unit", m.pos)
                case tp =>
                  fail("don't know what this is: " + tp, m.pos)
              }
            }
          }
        }
      }
    }

  }

  /*
   * must-single-thread
   */
  def initBytecodeWriter(entryPoints: List[Symbol]): BytecodeWriter = {
    settings.outputDirs.getSingleOutput match {
      case Some(f) if f hasExtension "jar" =>
        // If no main class was specified, see if there's only one
        // entry point among the classes going into the jar.
        if (settings.mainClass.isDefault) {
          entryPoints map (_.fullName('.')) match {
            case Nil      =>
              log("No Main-Class designated or discovered.")
            case name :: Nil =>
              log("Unique entry point: setting Main-Class to " + name)
              settings.mainClass.value = name
            case names =>
              log("No Main-Class due to multiple entry points:\n  " + names.mkString("\n  "))
          }
        }
        else log("Main-Class was specified: " + settings.mainClass.value)

        new DirectToJarfileWriter(f.file)

      case _ => factoryNonJarBytecodeWriter()
    }
  }

  /*
   *  must-single-thread
   */
  def fieldSymbols(cls: Symbol): List[Symbol] = {
    for (f <- cls.info.decls.toList ;
         if !f.isMethod && f.isTerm && !f.isModule
    ) yield f;
  }

  /*
   * can-multi-thread
   */
  def methodSymbols(cd: ClassDef): List[Symbol] = {
    cd.impl.body collect { case dd: DefDef => dd.symbol }
  }

  /*
   * Populates the InnerClasses JVM attribute with `refedInnerClasses`.
   * In additiona to inner classes mentioned somewhere in `jclass` (where `jclass` is a class file being emitted)
   * `refedInnerClasses` should contain those inner classes defined as direct member classes of `jclass`
   * but otherwise not mentioned in `jclass`.
   *
   * `refedInnerClasses` may contain duplicates,
   * need not contain the enclosing inner classes of each inner class it lists (those are looked up for consistency).
   *
   * This method serializes in the InnerClasses JVM attribute in an appropriate order,
   * not necessarily that given by `refedInnerClasses`.
   *
   * can-multi-thread
   */
  final def addInnerClassesASM(jclass: asm.ClassVisitor, refedInnerClasses: Iterable[BType]) {
    // used to detect duplicates.
    val seen = mutable.Map.empty[String, String]
    // result without duplicates, not yet sorted.
    val result = mutable.Set.empty[InnerClassEntry]

    for(s: BType           <- refedInnerClasses;
        e: InnerClassEntry <- exemplars.get(s).innersChain) {

      assert(e.name != null, "saveInnerClassesFor() is broken.") // documentation
      val doAdd = seen.get(e.name) match {
        // TODO is it ok for prevOName to be null? (Someone should really document the invariants of the InnerClasses bytecode attribute)
        case Some(prevOName) =>
          // this occurs e.g. when innerClassBuffer contains both class Thread$State, object Thread$State,
          // i.e. for them it must be the case that oname == java/lang/Thread
          assert(prevOName == e.outerName, "duplicate")
          false
        case None => true
      }

      if(doAdd) {
        seen   += (e.name -> e.outerName)
        result += e
      }

    }
    // sorting ensures inner classes are listed after their enclosing class thus satisfying the Eclipse Java compiler
    for(e <- result.toList sortBy (_.name.toString)) {
      jclass.visitInnerClass(e.name, e.outerName, e.innerName, e.access)
    }

  } // end of method addInnerClassesASM()

  /*
   * Custom attribute (JVMS 4.7.1) "ScalaSig" used as marker only
   * i.e., the pickle is contained in a custom annotation, see:
   *   (1) `addAnnotations()`,
   *   (2) SID # 10 (draft) - Storage of pickled Scala signatures in class files, http://www.scala-lang.org/sid/10
   *   (3) SID # 5 - Internals of Scala Annotations, http://www.scala-lang.org/sid/5
   * That annotation in turn is not related to the "java-generic-signature" (JVMS 4.7.9)
   * other than both ending up encoded as attributes (JVMS 4.7)
   * (with the caveat that the "ScalaSig" attribute is associated to some classes,
   * while the "Signature" attribute can be associated to classes, methods, and fields.)
   *
   */
  trait BCPickles {

    import scala.reflect.internal.pickling.{ PickleFormat, PickleBuffer }

    val versionPickle = {
      val vp = new PickleBuffer(new Array[Byte](16), -1, 0)
      assert(vp.writeIndex == 0, vp)
      vp writeNat PickleFormat.MajorVersion
      vp writeNat PickleFormat.MinorVersion
      vp writeNat 0
      vp
    }

    /*
     * can-multi-thread
     */
    def createJAttribute(name: String, b: Array[Byte], offset: Int, len: Int): asm.Attribute = {
      val dest = new Array[Byte](len);
      System.arraycopy(b, offset, dest, 0, len);
      new asm.CustomAttr(name, dest)
    }

    /*
     * can-multi-thread
     */
    def pickleMarkerLocal = {
      createJAttribute(tpnme.ScalaSignatureATTR.toString, versionPickle.bytes, 0, versionPickle.writeIndex)
    }

    /*
     * can-multi-thread
     */
    def pickleMarkerForeign = {
      createJAttribute(tpnme.ScalaATTR.toString, new Array[Byte](0), 0, 0)
    }

    /*  Returns a ScalaSignature annotation if it must be added to this class, none otherwise.
     *  This annotation must be added to the class' annotations list when generating them.
     *
     *  Depending on whether the returned option is defined, it adds to `jclass` one of:
     *    (a) the ScalaSig marker attribute
     *        (indicating that a scala-signature-annotation aka pickle is present in this class); or
     *    (b) the Scala marker attribute
     *        (indicating that a scala-signature-annotation aka pickle is to be found in another file).
     *
     *
     *  @param jclassName The class file that is being readied.
     *  @param sym    The symbol for which the signature has been entered in the symData map.
     *                This is different than the symbol
     *                that is being generated in the case of a mirror class.
     *  @return       An option that is:
     *                - defined and contains an AnnotationInfo of the ScalaSignature type,
     *                  instantiated with the pickle signature for sym.
     *                - empty if the jclass/sym pair must not contain a pickle.
     *
     *  must-single-thread
     */
    def getAnnotPickle(jclassName: String, sym: Symbol): Option[AnnotationInfo] = {
      currentRun.symData get sym match {
        case Some(pickle) if !nme.isModuleName(newTermName(jclassName)) =>
          val scalaAnnot = {
            val sigBytes = ScalaSigBytes(pickle.bytes.take(pickle.writeIndex))
            AnnotationInfo(sigBytes.sigAnnot, Nil, (nme.bytes, sigBytes) :: Nil)
          }
          pickledBytes += pickle.writeIndex
          currentRun.symData -= sym
          currentRun.symData -= sym.companionSymbol
          Some(scalaAnnot)
        case _ =>
          None
      }
    }

  } // end of trait BCPickles

  trait BCInnerClassGen {

    def debugLevel = settings.debuginfo.indexOfChoice

    val emitSource = debugLevel >= 1
    val emitLines  = debugLevel >= 2
    val emitVars   = debugLevel >= 3

    /*
     *  Contains class-symbols that:
     *    (a) are known to denote inner classes
     *    (b) are mentioned somewhere in the class being generated.
     *
     *  In other words, the lifetime of `innerClassBufferASM` is associated to "the class being generated".
     */
    val innerClassBufferASM = mutable.Set.empty[BType]

    /*
     *  Tracks (if needed) the inner class given by `sym`.
     *
     *  must-single-thread
     */
    final def internalName(sym: Symbol): String = { asmClassType(sym).getInternalName }

    /*
     *  Tracks (if needed) the inner class given by `sym`.
     *
     *  must-single-thread
     */
    final def asmClassType(sym: Symbol): BType = {
      assert(
        hasInternalName(sym),
        {
          val msg0 = if(sym.isAbstractType) "An AbstractTypeSymbol (SI-7122) " else "A symbol ";
          msg0 + "has reached the bytecode emitter, for which no JVM-level internal name can be found: " + sym.fullName
        }
      )
      val phantOpt = phantomTypeMap.get(sym)
      if(phantOpt.isDefined) {
        return phantOpt.get
      }
      val tracked = exemplar(sym)
      val tk = tracked.c
      if(tracked.isInnerClass) {
        innerClassBufferASM += tk
      }

      tk
    }

    /*
     *  Returns the BType for the given type.
     *  Tracks (if needed) the inner class given by `t`.
     *
     * must-single-thread
     */
    final def toTypeKind(t: Type): BType = {

          /* Interfaces have to be handled delicately to avoid introducing spurious errors,
           *  but if we treat them all as AnyRef we lose too much information.
           */
          def newReference(sym0: Symbol): BType = {
            assert(!primitiveTypeMap.contains(sym0), "Use primitiveTypeMap instead.")
            assert(sym0 != definitions.ArrayClass,   "Use arrayOf() instead.")

            if(sym0 == definitions.NullClass)    return RT_NULL;
            if(sym0 == definitions.NothingClass) return RT_NOTHING;

            // Working around SI-5604.  Rather than failing the compile when we see
            // a package here, check if there's a package object.
            val sym = (
              if (!sym0.isPackageClass) sym0
              else sym0.info.member(nme.PACKAGE) match {
                case NoSymbol => abort("Cannot use package as value: " + sym0.fullName)
                case s        => devWarning("Bug: found package class where package object expected.  Converting.") ; s.moduleClass
              }
            )

            // Can't call .toInterface (at this phase) or we trip an assertion.
            // See PackratParser#grow for a method which fails with an apparent mismatch
            // between "object PackratParsers$class" and "trait PackratParsers"
            if (sym.isImplClass) {
              // pos/spec-List.scala is the sole failure if we don't check for NoSymbol
              val traitSym = sym.owner.info.decl(tpnme.interfaceName(sym.name))
              if (traitSym != NoSymbol) {
                // this tracks the inner class in innerClassBufferASM, if needed.
                return asmClassType(traitSym)
              }
            }

            assert(hasInternalName(sym), "Invoked for a symbol lacking JVM internal name: " + sym.fullName)
            assert(!phantomTypeMap.contains(sym), "phantom types not supposed to reach here.")

            val tracked = exemplar(sym)
            val tk = tracked.c
            if(tracked.isInnerClass) {
              innerClassBufferASM += tk
            }

            tk
          }

          def primitiveOrRefType(sym: Symbol): BType = {
            assert(sym != definitions.ArrayClass, "Use primitiveOrArrayOrRefType() instead.")

            primitiveTypeMap.getOrElse(sym, newReference(sym))
          }

          def primitiveOrRefType2(sym: Symbol): BType = {
            primitiveTypeMap.get(sym) match {
              case Some(pt) => pt
              case None =>
                sym match {
                  case definitions.NullClass    => RT_NULL
                  case definitions.NothingClass => RT_NOTHING
                  case _ if sym.isClass         => newReference(sym)
                  case _ =>
                    assert(sym.isType, sym) // it must be compiling Array[a]
                    ObjectReference
                }
            }
          }

      import definitions.ArrayClass

      // Call to .normalize fixes #3003 (follow type aliases). Otherwise, primitiveOrArrayOrRefType() would return ObjectReference.
      t.normalize match {

        case ThisType(sym) =>
          if(sym == ArrayClass) ObjectReference
          else                  phantomTypeMap.getOrElse(sym, exemplar(sym).c)

        case SingleType(_, sym) => primitiveOrRefType(sym)

        case _: ConstantType    => toTypeKind(t.underlying)

        case TypeRef(_, sym, args)    =>
          if(sym == ArrayClass) arrayOf(toTypeKind(args.head))
          else                  primitiveOrRefType2(sym)

        case ClassInfoType(_, _, sym) =>
          assert(sym != ArrayClass, "ClassInfoType to ArrayClass!")
          primitiveOrRefType(sym)

        // !!! Iulian says types which make no sense after erasure should not reach here, which includes the ExistentialType, AnnotatedType, RefinedType.
        case _: ExistentialType     => abort("ExistentialType reached GenBCode: " + t)
        case AnnotatedType(_, w, _) => toTypeKind(w) // TODO test/files/jvm/annotations.scala causes an AnnotatedType to reach here.
        case _: RefinedType         => abort("RefinedType reached GenBCode: "     + t) // defensive: parents map toTypeKind reduceLeft lub

        // For sure WildcardTypes shouldn't reach here either, but when debugging such situations this may come in handy.
        // case WildcardType    => REFERENCE(ObjectClass)
        case norm => abort(
          "Unknown type: %s, %s [%s, %s] TypeRef? %s".format(
            t, norm, t.getClass, norm.getClass, t.isInstanceOf[TypeRef]
          )
        )
      }

    } // end of method toTypeKind()

    /*
     * must-single-thread
     */
    def asmMethodType(msym: Symbol): BType = {
      assert(msym.isMethod, "not a method-symbol: " + msym)
      val resT: BType =
        if (msym.isClassConstructor || msym.isConstructor) BType.VOID_TYPE
        else toTypeKind(msym.tpe.resultType);
      BType.getMethodType( resT, mkArray(msym.tpe.paramTypes map toTypeKind) )
    }

    /*
     *  Returns all direct member inner classes of `csym`,
     *  thus making sure they get entries in the InnerClasses JVM attribute
     *  even if otherwise not mentioned in the class being built.
     *
     *  must-single-thread
     */
    final def trackMemberClasses(csym: Symbol, lateClosuresBTs: List[BType]): List[BType] = {
      val lateInnerClasses = exitingErasure {
        for (sym <- List(csym, csym.linkedClassOfClass); memberc <- sym.info.decls.map(innerClassSymbolFor) if memberc.isClass)
        yield memberc
      }
      // as a precaution, do the following outside the above `exitingErasure` otherwise funny internal names might be computed.
      val result = for(memberc <- lateInnerClasses) yield {
        val tracked = exemplar(memberc)
        val memberCTK = tracked.c
        assert(tracked.isInnerClass, "saveInnerClassesFor() says this was no inner-class after all: " + memberc.fullName)

        memberCTK
      }

      exemplar(csym).directMemberClasses = (result ::: lateClosuresBTs)

      result
    }

    /*
     *  Tracks (if needed) the inner class given by `t`.
     *
     *  must-single-thread
     */
    final def descriptor(t: Type):   String = { toTypeKind(t).getDescriptor   }

    /*
     *  Tracks (if needed) the inner class given by `sym`.
     *
     *  must-single-thread
     */
    final def descriptor(sym: Symbol): String = { asmClassType(sym).getDescriptor }

  } // end of trait BCInnerClassGen

  trait BCAnnotGen extends BCInnerClassGen {

    /*
     *  can-multi-thread
     */
    def ubytesToCharArray(bytes: Array[Byte]): Array[Char] = {
      val ca = new Array[Char](bytes.length)
      var idx = 0
      while(idx < bytes.length) {
        val b: Byte = bytes(idx)
        assert((b & ~0x7f) == 0)
        ca(idx) = b.asInstanceOf[Char]
        idx += 1
      }

      ca
    }

    /*
     *  can-multi-thread
     */
    private def arrEncode(sb: ScalaSigBytes): Array[String] = {
      var strs: List[String]  = Nil
      val bSeven: Array[Byte] = sb.sevenBitsMayBeZero
      // chop into slices of at most 65535 bytes, counting 0x00 as taking two bytes (as per JVMS 4.4.7 The CONSTANT_Utf8_info Structure)
      var prevOffset = 0
      var offset     = 0
      var encLength  = 0
      while(offset < bSeven.size) {
        val deltaEncLength = (if(bSeven(offset) == 0) 2 else 1)
        val newEncLength = encLength.toLong + deltaEncLength
        if(newEncLength >= 65535) {
          val ba     = bSeven.slice(prevOffset, offset)
          strs     ::= new java.lang.String(ubytesToCharArray(ba))
          encLength  = 0
          prevOffset = offset
        } else {
          encLength += deltaEncLength
          offset    += 1
        }
      }
      if(prevOffset < offset) {
        assert(offset == bSeven.length)
        val ba = bSeven.slice(prevOffset, offset)
        strs ::= new java.lang.String(ubytesToCharArray(ba))
      }
      assert(strs.size > 1, "encode instead as one String via strEncode()") // TODO too strict?
      mkArrayReverse(strs)
    }

    /*
     *  can-multi-thread
     */
    private def strEncode(sb: ScalaSigBytes): String = {
      val ca = ubytesToCharArray(sb.sevenBitsMayBeZero)
      new java.lang.String(ca)
      // debug val bvA = new asm.ByteVector; bvA.putUTF8(s)
      // debug val enc: Array[Byte] = scala.reflect.internal.pickling.ByteCodecs.encode(bytes)
      // debug assert(enc(idx) == bvA.getByte(idx + 2))
      // debug assert(bvA.getLength == enc.size + 2)
    }

    /*
     * For arg a LiteralAnnotArg(constt) with const.tag in {ClazzTag, EnumTag}
     * as well as for arg a NestedAnnotArg
     *   must-single-thread
     * Otherwise it's safe to call from multiple threads.
     */
    def emitArgument(av:   asm.AnnotationVisitor,
                     name: String,
                     arg:  ClassfileAnnotArg) {
      arg match {

        case LiteralAnnotArg(const) =>
          if(const.isNonUnitAnyVal) { av.visit(name, const.value) }
          else {
            const.tag match {
              case StringTag  =>
                assert(const.value != null, const) // TODO this invariant isn't documented in `case class Constant`
                av.visit(name, const.stringValue)  // `stringValue` special-cases null, but that execution path isn't exercised for a const with StringTag
              case ClazzTag   => av.visit(name, toTypeKind(const.typeValue).toASMType)
              case EnumTag =>
                val edesc  = descriptor(const.tpe) // the class descriptor of the enumeration class.
                val evalue = const.symbolValue.name.toString // value the actual enumeration value.
                av.visitEnum(name, edesc, evalue)
            }
          }

        case sb@ScalaSigBytes(bytes) =>
          // see http://www.scala-lang.org/sid/10 (Storage of pickled Scala signatures in class files)
          // also JVMS Sec. 4.7.16.1 The element_value structure and JVMS Sec. 4.4.7 The CONSTANT_Utf8_info Structure.
          if (sb.fitsInOneString) {
            av.visit(name, strEncode(sb))
          } else {
            val arrAnnotV: asm.AnnotationVisitor = av.visitArray(name)
            for(arg <- arrEncode(sb)) { arrAnnotV.visit(name, arg) }
            arrAnnotV.visitEnd()
          }          // for the lazy val in ScalaSigBytes to be GC'ed, the invoker of emitAnnotations() should hold the ScalaSigBytes in a method-local var that doesn't escape.

        case ArrayAnnotArg(args) =>
          val arrAnnotV: asm.AnnotationVisitor = av.visitArray(name)
          for(arg <- args) { emitArgument(arrAnnotV, null, arg) }
          arrAnnotV.visitEnd()

        case NestedAnnotArg(annInfo) =>
          val AnnotationInfo(typ, args, assocs) = annInfo
          assert(args.isEmpty, args)
          val desc = descriptor(typ) // the class descriptor of the nested annotation class
          val nestedVisitor = av.visitAnnotation(name, desc)
          emitAssocs(nestedVisitor, assocs)
      }
    }

    /* Whether an annotation should be emitted as a Java annotation
     *   .initialize: if 'annot' is read from pickle, atp might be un-initialized
     *
     * must-single-thread
     */
    private def shouldEmitAnnotation(annot: AnnotationInfo) =
      annot.symbol.initialize.isJavaDefined &&
      annot.matches(definitions.ClassfileAnnotationClass) &&
      annot.args.isEmpty &&
      !annot.matches(definitions.DeprecatedAttr)

    /*
     * In general,
     *   must-single-thread
     * but not  necessarily always.
     */
    def emitAssocs(av: asm.AnnotationVisitor, assocs: List[(Name, ClassfileAnnotArg)]) {
      for ((name, value) <- assocs) {
        emitArgument(av, name.toString(), value)
      }
      av.visitEnd()
    }

    /*
     * must-single-thread
     */
    def emitAnnotations(cw: asm.ClassVisitor, annotations: List[AnnotationInfo]) {
      for(annot <- annotations; if shouldEmitAnnotation(annot)) {
        val AnnotationInfo(typ, args, assocs) = annot
        assert(args.isEmpty, args)
        val av = cw.visitAnnotation(descriptor(typ), true)
        emitAssocs(av, assocs)
      }
    }

    /*
     * must-single-thread
     */
    def emitAnnotations(mw: asm.MethodVisitor, annotations: List[AnnotationInfo]) {
      for(annot <- annotations; if shouldEmitAnnotation(annot)) {
        val AnnotationInfo(typ, args, assocs) = annot
        assert(args.isEmpty, args)
        val av = mw.visitAnnotation(descriptor(typ), true)
        emitAssocs(av, assocs)
      }
    }

    /*
     * must-single-thread
     */
    def emitAnnotations(fw: asm.FieldVisitor, annotations: List[AnnotationInfo]) {
      for(annot <- annotations; if shouldEmitAnnotation(annot)) {
        val AnnotationInfo(typ, args, assocs) = annot
        assert(args.isEmpty, args)
        val av = fw.visitAnnotation(descriptor(typ), true)
        emitAssocs(av, assocs)
      }
    }

    /*
     * must-single-thread
     */
    def emitParamAnnotations(jmethod: asm.MethodVisitor, pannotss: List[List[AnnotationInfo]]) {
      val annotationss = pannotss map (_ filter shouldEmitAnnotation)
      if (annotationss forall (_.isEmpty)) return
      for (Pair(annots, idx) <- annotationss.zipWithIndex;
           annot <- annots) {
        val AnnotationInfo(typ, args, assocs) = annot
        assert(args.isEmpty, args)
        val pannVisitor: asm.AnnotationVisitor = jmethod.visitParameterAnnotation(idx, descriptor(typ), true)
        emitAssocs(pannVisitor, assocs)
      }
    }

  } // end of trait BCAnnotGen

  trait BCJGenSigGen {

    // @M don't generate java generics sigs for (members of) implementation
    // classes, as they are monomorphic (TODO: ok?)
    /*
     * must-single-thread
     */
    private def needsGenericSignature(sym: Symbol) = !(
      // PP: This condition used to include sym.hasExpandedName, but this leads
      // to the total loss of generic information if a private member is
      // accessed from a closure: both the field and the accessor were generated
      // without it.  This is particularly bad because the availability of
      // generic information could disappear as a consequence of a seemingly
      // unrelated change.
         settings.Ynogenericsig
      || sym.isArtifact
      || sym.isLiftedMethod
      || sym.isBridge
      || (sym.ownerChain exists (_.isImplClass))
    )

    def getCurrentCUnit(): CompilationUnit

    /* @return
     *   - `null` if no Java signature is to be added (`null` is what ASM expects in these cases).
     *   - otherwise the signature in question
     *
     * must-single-thread
     */
    def getGenericSignature(sym: Symbol, owner: Symbol): String = {

      if (!needsGenericSignature(sym)) { return null }

      val memberTpe = enteringErasure(owner.thisType.memberInfo(sym))

      val jsOpt: Option[String] = erasure.javaSig(sym, memberTpe)
      if (jsOpt.isEmpty) { return null }

      val sig = jsOpt.get
      log(sig) // This seems useful enough in the general case.

          def wrap(op: => Unit) = {
            try   { op; true }
            catch { case _: Throwable => false }
          }

      if (settings.Xverify) {
        // Run the signature parser to catch bogus signatures.
        val isValidSignature = wrap {
          // Alternative: scala.tools.reflect.SigParser (frontend to sun.reflect.generics.parser.SignatureParser)
          import scala.tools.asm.util.CheckClassAdapter
          if (sym.isMethod)    { CheckClassAdapter checkMethodSignature sig }
          else if (sym.isTerm) { CheckClassAdapter checkFieldSignature  sig }
          else                 { CheckClassAdapter checkClassSignature  sig }
        }

        if(!isValidSignature) {
          getCurrentCUnit().warning(sym.pos,
              """|compiler bug: created invalid generic signature for %s in %s
                 |signature: %s
                 |if this is reproducible, please report bug at https://issues.scala-lang.org/
              """.trim.stripMargin.format(sym, sym.owner.skipPackageObject.fullName, sig))
          return null
        }
      }

      if ((settings.check containsName phaseName)) {
        val normalizedTpe = enteringErasure(erasure.prepareSigMap(memberTpe))
        val bytecodeTpe = owner.thisType.memberInfo(sym)
        if (!sym.isType && !sym.isConstructor && !(erasure.erasure(sym)(normalizedTpe) =:= bytecodeTpe)) {
          getCurrentCUnit().warning(sym.pos,
              """|compiler bug: created generic signature for %s in %s that does not conform to its erasure
                 |signature: %s
                 |original type: %s
                 |normalized type: %s
                 |erasure type: %s
                 |if this is reproducible, please report bug at http://issues.scala-lang.org/
              """.trim.stripMargin.format(sym, sym.owner.skipPackageObject.fullName, sig, memberTpe, normalizedTpe, bytecodeTpe))
           return null
        }
      }

      sig
    }

  } // end of trait BCJGenSigGen

  trait BCForwardersGen extends BCAnnotGen with BCJGenSigGen {

    // -----------------------------------------------------------------------------------------
    // Static forwarders (related to mirror classes but also present in
    // a plain class lacking companion module, for details see `isCandidateForForwarders`).
    // -----------------------------------------------------------------------------------------

    val ExcludedForwarderFlags = {
      import symtab.Flags._
      // Should include DEFERRED but this breaks findMember.
      ( CASE | SPECIALIZED | LIFTED | PROTECTED | STATIC | EXPANDEDNAME | BridgeAndPrivateFlags | MACRO )
    }

    /* Adds a @remote annotation, actual use unknown.
     *
     * Invoked from genMethod() and addForwarder().
     *
     * must-single-thread
     */
    def addRemoteExceptionAnnot(isRemoteClass: Boolean, isJMethodPublic: Boolean, meth: Symbol) {
      val needsAnnotation = (
        (  isRemoteClass ||
           isRemote(meth) && isJMethodPublic
        ) && !(meth.throwsAnnotations contains definitions.RemoteExceptionClass)
      )
      if (needsAnnotation) {
        val c   = Constant(definitions.RemoteExceptionClass.tpe)
        val arg = Literal(c) setType c.tpe
        meth.addAnnotation(appliedType(definitions.ThrowsClass, c.tpe), arg)
      }
    }

    /* Add a forwarder for method m. Used only from addForwarders().
     *
     * must-single-thread
     */
    private def addForwarder(isRemoteClass: Boolean, jclass: asm.ClassVisitor, module: Symbol, m: Symbol) {
      val moduleName     = internalName(module)
      val methodInfo     = module.thisType.memberInfo(m)
      val paramJavaTypes: List[BType] = methodInfo.paramTypes map toTypeKind
      // val paramNames     = 0 until paramJavaTypes.length map ("x_" + _)

      /* Forwarders must not be marked final,
       *  as the JVM will not allow redefinition of a final static method,
       *  and we don't know what classes might be subclassing the companion class.  See SI-4827.
       */
      // TODO: evaluate the other flags we might be dropping on the floor here.
      // TODO: ACC_SYNTHETIC ?
      val flags = PublicStatic | (
        if (m.isVarargsMethod) asm.Opcodes.ACC_VARARGS else 0
      )

      // TODO needed? for(ann <- m.annotations) { ann.symbol.initialize }
      val jgensig = if (m.isDeferred) null else getGenericSignature(m, module); // only add generic signature if method concrete; bug #1745
      addRemoteExceptionAnnot(isRemoteClass, hasPublicBitSet(flags), m)
      val (throws, others) = m.annotations partition (_.symbol == definitions.ThrowsClass)
      val thrownExceptions: List[String] = getExceptions(throws)

      val jReturnType = toTypeKind(methodInfo.resultType)
      val mdesc = BType.getMethodType(jReturnType, mkArray(paramJavaTypes)).getDescriptor
      val mirrorMethodName = m.javaSimpleName.toString
      val mirrorMethod: asm.MethodVisitor = jclass.visitMethod(
        flags,
        mirrorMethodName,
        mdesc,
        jgensig,
        mkArray(thrownExceptions)
      )

      emitAnnotations(mirrorMethod, others)
      emitParamAnnotations(mirrorMethod, m.info.params.map(_.annotations))

      mirrorMethod.visitCode()

      mirrorMethod.visitFieldInsn(asm.Opcodes.GETSTATIC, moduleName, strMODULE_INSTANCE_FIELD, descriptor(module))

      var index = 0
      for(jparamType <- paramJavaTypes) {
        mirrorMethod.visitVarInsn(jparamType.getOpcode(asm.Opcodes.ILOAD), index)
        assert(jparamType.sort != BType.METHOD, jparamType)
        index += jparamType.getSize
      }

      mirrorMethod.visitMethodInsn(asm.Opcodes.INVOKEVIRTUAL, moduleName, mirrorMethodName, asmMethodType(m).getDescriptor)
      mirrorMethod.visitInsn(jReturnType.getOpcode(asm.Opcodes.IRETURN))

      mirrorMethod.visitMaxs(0, 0) // just to follow protocol, dummy arguments
      mirrorMethod.visitEnd()

    }

    /* Add forwarders for all methods defined in `module` that don't conflict
     *  with methods in the companion class of `module`. A conflict arises when
     *  a method with the same name is defined both in a class and its companion object:
     *  method signature is not taken into account.
     *
     * must-single-thread
     */
    def addForwarders(isRemoteClass: Boolean, jclass: asm.ClassVisitor, jclassName: String, moduleClass: Symbol) {
      assert(moduleClass.isModuleClass, moduleClass)
      debuglog("Dumping mirror class for object: " + moduleClass)

      val linkedClass  = moduleClass.companionClass
      lazy val conflictingNames: Set[Name] = {
        (linkedClass.info.members collect { case sym if sym.name.isTermName => sym.name }).toSet
      }
      debuglog("Potentially conflicting names for forwarders: " + conflictingNames)

      for (m <- moduleClass.info.membersBasedOnFlags(ExcludedForwarderFlags, symtab.Flags.METHOD)) {
        if (m.isType || m.isDeferred || (m.owner eq definitions.ObjectClass) || m.isConstructor)
          debuglog("No forwarder for '%s' from %s to '%s'".format(m, jclassName, moduleClass))
        else if (conflictingNames(m.name))
          log("No forwarder for " + m + " due to conflict with " + linkedClass.info.member(m.name))
        else if (m.hasAccessBoundary)
          log(s"No forwarder for non-public member $m")
        else {
          log("Adding static forwarder for '%s' from %s to '%s'".format(m, jclassName, moduleClass))
          addForwarder(isRemoteClass, jclass, moduleClass, m)
        }
      }
    }

    /*
     * Quoting from JVMS 4.7.5 The Exceptions Attribute
     *   "The Exceptions attribute indicates which checked exceptions a method may throw.
     *    There may be at most one Exceptions attribute in each method_info structure."
     *
     * The contents of that attribute are determined by the `String[] exceptions` argument to ASM's ClassVisitor.visitMethod()
     * This method returns such list of internal names.
     *
     * must-single-thread
     */
    def getExceptions(excs: List[AnnotationInfo]): List[String] = {
      for (ThrownException(exc) <- excs.distinct)
      yield internalName(exc)
    }

  } // end of trait BCForwardersGen

  trait BCClassGen extends BCInnerClassGen {

    val MIN_SWITCH_DENSITY = 0.7

    /*
     *  must-single-thread
     */
    def serialVUID(csym: Symbol): Option[Long] = csym getAnnotation definitions.SerialVersionUIDAttr collect {
      case AnnotationInfo(_, Literal(const) :: _, _) => const.longValue
    }

    /*
     *  Add public static final field serialVersionUID with value `id`
     *
     *  can-multi-thread
     */
    def addSerialVUID(id: Long, jclass: asm.ClassVisitor) {
      // add static serialVersionUID field if `clasz` annotated with `@SerialVersionUID(uid: Long)`
      jclass.visitField(
        PublicStaticFinal,
        "serialVersionUID",
        "J",
        null, // no java-generic-signature
        new java.lang.Long(id)
      ).visitEnd()
    }

    /*
     * @param owner internal name of the enclosing class of the class.
     *
     * @param name the name of the method that contains the class.

     * @param methodType the method that contains the class.
     */
    case class EnclMethodEntry(owner: String, name: String, methodType: BType)

    /*
     * @return null if the current class is not internal to a method
     *
     * Quoting from JVMS 4.7.7 The EnclosingMethod Attribute
     *   A class must have an EnclosingMethod attribute if and only if it is a local class or an anonymous class.
     *   A class may have no more than one EnclosingMethod attribute.
     *
     * must-single-thread
     */
    def getEnclosingMethodAttribute(clazz: Symbol): EnclMethodEntry = { // JVMS 4.7.7

          def newEEE(eClass: Symbol, m: Symbol) = {
            EnclMethodEntry(
              internalName(eClass),
              m.javaSimpleName.toString,
              asmMethodType(m)
            )
          }

      var res: EnclMethodEntry = null
      val sym = clazz.originalEnclosingMethod
      if (sym.isMethod) {
        debuglog("enclosing method for %s is %s (in %s)".format(clazz, sym, sym.enclClass))
        res = newEEE(sym.enclClass, sym)
      } else if (clazz.isAnonymousClass) {
        val enclClass = clazz.rawowner
        assert(enclClass.isClass, enclClass)
        val sym = enclClass.primaryConstructor
        if (sym == NoSymbol) {
          log("Ran out of room looking for an enclosing method for %s: no constructor here.".format(enclClass, clazz))
        } else {
          debuglog("enclosing method for %s is %s (in %s)".format(clazz, sym, enclClass))
          res = newEEE(enclClass, sym)
        }
      }

      res
    }

  } // end of trait BCClassGen

  /* basic functionality for class file building of plain, mirror, and beaninfo classes. */
  abstract class JBuilder extends BCInnerClassGen {

  } // end of class JBuilder

  /* functionality for building plain and mirror classes */
  abstract class JCommonBuilder
    extends JBuilder
    with    BCAnnotGen
    with    BCForwardersGen
    with    BCPickles { }

  /* builder of mirror classes */
  class JMirrorBuilder extends JCommonBuilder {

    private var cunit: CompilationUnit = _
    def getCurrentCUnit(): CompilationUnit = cunit;

    /* Generate a mirror class for a top-level module. A mirror class is a class
     *  containing only static methods that forward to the corresponding method
     *  on the MODULE instance of the given Scala object.  It will only be
     *  generated if there is no companion class: if there is, an attempt will
     *  instead be made to add the forwarder methods to the companion class.
     *
     *  must-single-thread
     */
    def genMirrorClass(modsym: Symbol, cunit: CompilationUnit): asm.tree.ClassNode = {
      assert(modsym.companionClass == NoSymbol, modsym)
      innerClassBufferASM.clear()
      this.cunit = cunit
      val moduleName = internalName(modsym) // + "$"
      val mirrorName = moduleName.substring(0, moduleName.length() - 1)

      val flags = (asm.Opcodes.ACC_SUPER | asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_FINAL)
      val mirrorClass = new asm.tree.ClassNode
      mirrorClass.visit(
        classfileVersion,
        flags,
        mirrorName,
        null /* no java-generic-signature */,
        JAVA_LANG_OBJECT.getInternalName,
        EMPTY_STRING_ARRAY
      )

      if(emitSource) {
        mirrorClass.visitSource("" + cunit.source,
                                null /* SourceDebugExtension */)
      }

      val ssa = getAnnotPickle(mirrorName, modsym.companionSymbol)
      mirrorClass.visitAttribute(if(ssa.isDefined) pickleMarkerLocal else pickleMarkerForeign)
      emitAnnotations(mirrorClass, modsym.annotations ++ ssa)

      addForwarders(isRemote(modsym), mirrorClass, mirrorName, modsym)

      innerClassBufferASM ++= trackMemberClasses(modsym, Nil /* TODO what about Late-Closure-Classes */ )
      addInnerClassesASM(mirrorClass, innerClassBufferASM.toList)

      mirrorClass.visitEnd()

      ("" + modsym.name) // this side-effect is necessary, really.

      mirrorClass
    }

  } // end of class JMirrorBuilder

  /* builder of bean info classes */
  class JBeanInfoBuilder extends JBuilder {

    /*
     * Generate a bean info class that describes the given class.
     *
     * @author Ross Judson (ross.judson@soletta.com)
     *
     * must-single-thread
     */
    def genBeanInfoClass(cls: Symbol, cunit: CompilationUnit, fieldSymbols: List[Symbol], methodSymbols: List[Symbol]): asm.tree.ClassNode = {

          def javaSimpleName(s: Symbol): String = { s.javaSimpleName.toString }

      innerClassBufferASM.clear()

      val flags = mkFlags(
        javaFlags(cls),
        if(isDeprecated(cls)) asm.Opcodes.ACC_DEPRECATED else 0 // ASM pseudo access flag
      )

      val beanInfoName  = (internalName(cls) + "BeanInfo")
      val beanInfoClass = new asm.tree.ClassNode
      beanInfoClass.visit(
        classfileVersion,
        flags,
        beanInfoName,
        null, // no java-generic-signature
        "scala/beans/ScalaBeanInfo",
        EMPTY_STRING_ARRAY
      )

      beanInfoClass.visitSource(
        cunit.source.toString,
        null /* SourceDebugExtension */
      )

      var fieldList = List[String]()

      for (f <- fieldSymbols if f.hasGetter;
	         g = f.getter(cls);
	         s = f.setter(cls);
	         if g.isPublic && !(f.name startsWith "$")
          ) {
             // inserting $outer breaks the bean
             fieldList = javaSimpleName(f) :: javaSimpleName(g) :: (if (s != NoSymbol) javaSimpleName(s) else null) :: fieldList
      }

      val methodList: List[String] =
	     for (m <- methodSymbols
	          if !m.isConstructor &&
	          m.isPublic &&
	          !(m.name startsWith "$") &&
	          !m.isGetter &&
	          !m.isSetter)
       yield javaSimpleName(m)

      val constructor = beanInfoClass.visitMethod(
        asm.Opcodes.ACC_PUBLIC,
        INSTANCE_CONSTRUCTOR_NAME,
        "()V",
        null, // no java-generic-signature
        EMPTY_STRING_ARRAY // no throwable exceptions
      )

      val stringArrayJType: BType = arrayOf(JAVA_LANG_STRING)
      val conJType: BType =
        BType.getMethodType(
          BType.VOID_TYPE,
          Array(exemplar(definitions.ClassClass).c, stringArrayJType, stringArrayJType)
        )

      def push(lst: List[String]) {
        var fi = 0
        for (f <- lst) {
          constructor.visitInsn(asm.Opcodes.DUP)
          constructor.visitLdcInsn(new java.lang.Integer(fi))
          if (f == null) { constructor.visitInsn(asm.Opcodes.ACONST_NULL) }
          else           { constructor.visitLdcInsn(f) }
          constructor.visitInsn(JAVA_LANG_STRING.getOpcode(asm.Opcodes.IASTORE))
          fi += 1
        }
      }

      constructor.visitCode()

      constructor.visitVarInsn(asm.Opcodes.ALOAD, 0)
      // push the class
      constructor.visitLdcInsn(exemplar(cls).c)

      // push the string array of field information
      constructor.visitLdcInsn(new java.lang.Integer(fieldList.length))
      constructor.visitTypeInsn(asm.Opcodes.ANEWARRAY, JAVA_LANG_STRING.getInternalName)
      push(fieldList)

      // push the string array of method information
      constructor.visitLdcInsn(new java.lang.Integer(methodList.length))
      constructor.visitTypeInsn(asm.Opcodes.ANEWARRAY, JAVA_LANG_STRING.getInternalName)
      push(methodList)

      // invoke the superclass constructor, which will do the
      // necessary java reflection and create Method objects.
      constructor.visitMethodInsn(asm.Opcodes.INVOKESPECIAL, "scala/beans/ScalaBeanInfo", INSTANCE_CONSTRUCTOR_NAME, conJType.getDescriptor)
      constructor.visitInsn(asm.Opcodes.RETURN)

      constructor.visitMaxs(0, 0) // just to follow protocol, dummy arguments
      constructor.visitEnd()

      innerClassBufferASM ++= trackMemberClasses(cls, Nil /* TODO what about Late-Closure-Classes */ )
      addInnerClassesASM(beanInfoClass, innerClassBufferASM.toList)

      beanInfoClass.visitEnd()

      beanInfoClass
    }

  } // end of class JBeanInfoBuilder

  trait JAndroidBuilder {
    self: BCInnerClassGen =>

    /* From the reference documentation of the Android SDK:
     *  The `Parcelable` interface identifies classes whose instances can be written to and restored from a `Parcel`.
     *  Classes implementing the `Parcelable` interface must also have a static field called `CREATOR`,
     *  which is an object implementing the `Parcelable.Creator` interface.
     */
    val androidFieldName = newTermName("CREATOR")

    /*
     * must-single-thread
     */
    def isAndroidParcelableClass(sym: Symbol) =
      (AndroidParcelableInterface != NoSymbol) &&
      (sym.parentSymbols contains AndroidParcelableInterface)

    /*
     * must-single-thread
     */
    def legacyAddCreatorCode(clinit: asm.MethodVisitor, cnode: asm.tree.ClassNode, thisName: String) {
      // this tracks the inner class in innerClassBufferASM, if needed.
      val androidCreatorType = asmClassType(AndroidCreatorClass)
      val tdesc_creator = androidCreatorType.getDescriptor

      cnode.visitField(
        PublicStaticFinal,
        "CREATOR",
        tdesc_creator,
        null, // no java-generic-signature
        null  // no initial value
      ).visitEnd()

      val moduleName = (thisName + "$")

      // GETSTATIC `moduleName`.MODULE$ : `moduleName`;
      clinit.visitFieldInsn(
        asm.Opcodes.GETSTATIC,
        moduleName,
        strMODULE_INSTANCE_FIELD,
        "L" + moduleName + ";"
      )

      // INVOKEVIRTUAL `moduleName`.CREATOR() : android.os.Parcelable$Creator;
      val bt = BType.getMethodType(androidCreatorType, Array.empty[BType])
      clinit.visitMethodInsn(
        asm.Opcodes.INVOKEVIRTUAL,
        moduleName,
        "CREATOR",
        bt.getDescriptor
      )

      // PUTSTATIC `thisName`.CREATOR;
      clinit.visitFieldInsn(
        asm.Opcodes.PUTSTATIC,
        thisName,
        "CREATOR",
        tdesc_creator
      )
    }

  } // end of trait JAndroidBuilder

  /*
   * Collects (in `result`) all LabelDef nodes enclosed (directly or not) by each node it visits.
   *
   * In other words, this traverser prepares a map giving
   * all labelDefs (the entry-value) having a Tree node (the entry-key) as ancestor.
   * The entry-value for a LabelDef entry-key always contains the entry-key.
   *
   */
  class LabelDefsFinder extends Traverser {
    val result = mutable.Map.empty[Tree, List[LabelDef]]
    var acc: List[LabelDef] = Nil

    /*
     * can-multi-thread
     */
    override def traverse(tree: Tree) {
      val saved = acc
      acc = Nil
      super.traverse(tree)
      // acc contains all LabelDefs found under (but not at) `tree`
      tree match {
        case lblDf: LabelDef => acc ::= lblDf
        case _               => ()
      }
      if(acc.isEmpty) {
        acc = saved
      } else {
        result += (tree -> acc)
        acc = acc ::: saved
      }
    }
  }

  var TF_NULL   : TFValue = null
  var TF_STRING : TFValue = null
  var BoxesRunTime: BType = null

  def initBCodeOpt() {

    TF_NULL = TFValue(RT_NULL, 0)
    val trString = exemplar(global.definitions.StringClass)
    TF_STRING = TFValue(StringReference, TypeFlowConstants.EXACT_MASK)

    // later a few analyses (e.g. refreshInnerClasses) will look up BTypes based on descriptors in instructions
    // we make sure those BTypes can be found via lookup as opposed to creating them on the fly.
    BoxesRunTime = brefType("scala/runtime/BoxesRunTime")
    asmBoxTo.values   foreach { mnat: MethodNameAndType => BType.getMethodType(mnat.mdesc) }
    asmUnboxTo.values foreach { mnat: MethodNameAndType => BType.getMethodType(mnat.mdesc) }
  }

  /*
   *  Represents a lattice element in the type-flow lattice.
   *  For reference-values two additional information items are also tracked:
   *     - their non-nullness status
   *     - whether the runtime value has `btype` as exact type
   *       (useful in determining method implementations targeted by callsites)
   */
  case class TFValue(lca: BType, bits: Int) extends asm.tree.analysis.Value {

    val tr: Tracked = {
      if(lca.isPhantomType || lca.isValueType) { null }
      else if(lca.isArray)  {
        val et = lca.getElementType
        if(et.hasObjectSort) exemplars.get(et) else null
      }
      else {
        assert(lca.isNonSpecial, "Some case was overlooked for: " + lca)
        exemplars.get(lca)
      }
    }

    // check (when possible) cross-consistency with isExact and isNonNull
    assert(if(lca.isNullType) !isNonNull else true, "Contradiction: NullType reported as isNonNull." + lca)
    assert(if(tr != null && tr.isFinal)      isExact    else true, "Contradiction: isFinal  reported as !isExact."  + lca)
    assert(if(tr != null && tr.isInterface)  !isExact   else true, "Contradiction: interface reported as isExact."  + lca)

    override def getSize: Int = {
      assert(lca.sort != BType.METHOD, "Method types don't have size: " + lca)
      lca.getSize
    }

    def isRefOrArray: Boolean = { lca.isRefOrArrayType }

    def isNonNull:  Boolean = { assert(isRefOrArray); (bits & TypeFlowConstants.NON_NULL_MASK) != 0 }
    def isExact:    Boolean = { assert(isRefOrArray); (bits & TypeFlowConstants.EXACT_MASK   ) != 0 }

    def conformsTo(b: TFValue): Boolean = { conforms(this.lca, b.lca) }

  }

  val TF_INT     = TFValue(BType.INT_TYPE,    0)
  val TF_FLOAT   = TFValue(BType.FLOAT_TYPE,  0)
  val TF_LONG    = TFValue(BType.LONG_TYPE,   0)
  val TF_DOUBLE  = TFValue(BType.DOUBLE_TYPE, 0)
  val TF_VOID    = TFValue(BType.VOID_TYPE,   0)

  object TypeFlowConstants {
    val NON_NULL_MASK = 1
    val EXACT_MASK    = 2
  }

  /*
   *  can-multi-thread
   */
  def toBType(t: asm.Type): BType = {
    (t.getSort: @switch) match {
      case asm.Type.VOID    => BType.VOID_TYPE
      case asm.Type.BOOLEAN => BType.BOOLEAN_TYPE
      case asm.Type.CHAR    => BType.CHAR_TYPE
      case asm.Type.BYTE    => BType.BYTE_TYPE
      case asm.Type.SHORT   => BType.SHORT_TYPE
      case asm.Type.INT     => BType.INT_TYPE
      case asm.Type.FLOAT   => BType.FLOAT_TYPE
      case asm.Type.LONG    => BType.LONG_TYPE
      case asm.Type.DOUBLE  => BType.DOUBLE_TYPE
      case asm.Type.ARRAY   |
           asm.Type.OBJECT  |
           asm.Type.METHOD  =>
        // TODO confirm whether this also takes care of the phantom types.
        val key =
          if(t.getSort == asm.Type.METHOD) t.getDescriptor
          else t.getInternalName

        val n = global.lookupTypeName(key.toCharArray)
        new BType(t.getSort, n.start, n.length)
    }
  }

  /*
   * ASM trees represent types as strings (internal names, descriptors).
   * Given that we operate instead on BTypes, conversion is needed when visiting MethodNodes outside GenBCode.
   *
   * can-multi-thread
   */
  def descrToBType(typeDescriptor: String): BType = {
    val c: Char = typeDescriptor(0)
    c match {
      case 'V' => BType.VOID_TYPE
      case 'Z' => BType.BOOLEAN_TYPE
      case 'C' => BType.CHAR_TYPE
      case 'B' => BType.BYTE_TYPE
      case 'S' => BType.SHORT_TYPE
      case 'I' => BType.INT_TYPE
      case 'F' => BType.FLOAT_TYPE
      case 'J' => BType.LONG_TYPE
      case 'D' => BType.DOUBLE_TYPE
      case 'L' =>
        val iname = typeDescriptor.substring(1, typeDescriptor.length() - 1)
        val n = global.lookupTypeName(iname.toCharArray)
        new BType(asm.Type.OBJECT, n.start, n.length)
      case _   =>
        val n = global.lookupTypeName(typeDescriptor.toCharArray)
        BType.getType(n.start)
    }
  }

  /*
   *
   * Use only to lookup reference types, otherwise use `descrToBType()`
   *
   * can-multi-thread
   */
  def lookupRefBType(iname: String): BType = {
    import global.chrs
    val n    = global.lookupTypeName(iname.toCharArray)
    val sort = if(chrs(n.start) == '[') BType.ARRAY else BType.OBJECT;
    new BType(sort, n.start, n.length)
  }

  def lookupRefBTypeIfExisting(iname: String): BType = {
    import global.chrs
    val n    = global.lookupTypeNameIfExisting(iname.toCharArray, false)
    if(n == null) { return null }
    val sort = if(chrs(n.start) == '[') BType.ARRAY else BType.OBJECT;
    new BType(sort, n.start, n.length)
  }

  implicit class MethodIterClassNode(cnode: asm.tree.ClassNode) {

    @inline final def foreachMethod(f: (asm.tree.MethodNode) => Unit) { toMethodList.foreach(f) }

    @inline final def toMethodList: List[asm.tree.MethodNode] = { JListWrapper(cnode.methods).toList }

    @inline final def toFieldList:  List[asm.tree.FieldNode] = { JListWrapper(cnode.fields).toList }

  }

  implicit class InsnIterMethodNode(mnode: asm.tree.MethodNode) {

    @inline final def foreachInsn(f: (asm.tree.AbstractInsnNode) => Unit) { mnode.instructions.foreachInsn(f) }

    @inline final def toList: List[asm.tree.AbstractInsnNode] = { mnode.instructions.toList }

  }

  implicit class InsnIterInsnList(lst: asm.tree.InsnList) {

    @inline final def foreachInsn(f: (asm.tree.AbstractInsnNode) => Unit) {
      val insnIter = lst.iterator()
      while(insnIter.hasNext) {
        f(insnIter.next())
      }
    }

    @inline final def toList: List[asm.tree.AbstractInsnNode] = {
      var result: List[asm.tree.AbstractInsnNode] = Nil
      lst foreachInsn { insn => if(insn != null) { result ::= insn }  }
      result.reverse
    }

  }

  /*
   *  Upon finding a name already seen among previous List elements, adds a numeric postfix to make it unique.
   */
  def uniquify(names: List[String]): List[String] = {
    val seen = mutable.Set.empty[String]

      @scala.annotation.tailrec def uniquified(current: String, attempt: Int): String = {
        if(seen contains current) {
          val currentBis = (current + "$" + attempt.toString)
          if(seen contains currentBis) {
            uniquified(current, attempt + 1)
          } else currentBis
        } else current
      }

    var rest = names
    var result: List[String] = Nil
    while(rest.nonEmpty) {
      val u    = uniquified(rest.head.trim, 1)
      seen    += u
      result ::= u
      rest     = rest.tail
    }

    result.reverse
  }

  def allDifferent[ElemType](xs: Iterable[ElemType]): Boolean = {
    val seen = mutable.Set.empty[ElemType]
    val iter = xs.iterator
    while(iter.hasNext) {
      val nxt = iter.next()
      if(seen contains nxt) { return false }
      seen += nxt
    }
    true
  }

} // end of class BCodeTypes
