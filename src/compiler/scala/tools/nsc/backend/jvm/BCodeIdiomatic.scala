/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package tools.nsc
package backend.jvm

import scala.tools.asm
import scala.annotation.switch
import scala.collection.{ immutable, mutable }
import collection.convert.Wrappers.JListWrapper

/*
 *  A high-level facade to the ASM API for bytecode generation.
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded
 *  @version 1.0
 *
 */
abstract class BCodeIdiomatic extends BCodeGlue {

  import global._

  val classfileVersion: Int = settings.target.value match {
    case "jvm-1.5"     => asm.Opcodes.V1_5
    case "jvm-1.6"     => asm.Opcodes.V1_6
    case "jvm-1.7"     => asm.Opcodes.V1_7
  }

  val majorVersion: Int = (classfileVersion & 0xFF)
  val emitStackMapFrame = (majorVersion >= 50)

  def mkFlags(args: Int*) = args.foldLeft(0)(_ | _)

  val extraProc: Int = mkFlags(
    asm.ClassWriter.COMPUTE_MAXS,
    if (emitStackMapFrame) asm.ClassWriter.COMPUTE_FRAMES else 0
  )

  val StringBuilderClassName = "scala/collection/mutable/StringBuilder"

  val CLASS_CONSTRUCTOR_NAME    = "<clinit>"
  val INSTANCE_CONSTRUCTOR_NAME = "<init>"

  val ObjectReference   = brefType("java/lang/Object")
  val AnyRefReference   = ObjectReference
  val objArrayReference = arrayOf(ObjectReference)

  val JAVA_LANG_OBJECT  = ObjectReference
  val JAVA_LANG_STRING  = brefType("java/lang/String")

  var StringBuilderReference: BType = null

  val EMPTY_STRING_ARRAY   = Array.empty[String]
  val EMPTY_INT_ARRAY      = Array.empty[Int]
  val EMPTY_LABEL_ARRAY    = Array.empty[asm.Label]
  val EMPTY_BTYPE_ARRAY    = Array.empty[BType]

  /* can-multi-thread */
  final def mkArray(xs: List[BType]): Array[BType] = {
    if (xs.isEmpty) { return EMPTY_BTYPE_ARRAY }
    val a = new Array[BType](xs.size); xs.copyToArray(a); a
  }
  /* can-multi-thread */
  final def mkArray(xs: List[String]): Array[String] = {
    if (xs.isEmpty) { return EMPTY_STRING_ARRAY }
    val a = new Array[String](xs.size); xs.copyToArray(a); a
  }
  /* can-multi-thread */
  final def mkArray(xs: List[asm.Label]): Array[asm.Label] = {
    if (xs.isEmpty) { return EMPTY_LABEL_ARRAY }
    val a = new Array[asm.Label](xs.size); xs.copyToArray(a); a
  }
  /* can-multi-thread */
  final def mkArray(xs: List[Int]): Array[Int] = {
    if (xs.isEmpty) { return EMPTY_INT_ARRAY }
    val a = new Array[Int](xs.size); xs.copyToArray(a); a
  }

  /*
   * can-multi-thread
   */
  final def mkArrayReverse(xs: List[String]): Array[String] = {
    val len = xs.size
    if (len == 0) { return EMPTY_STRING_ARRAY }
    val a = new Array[String](len)
    var i = len - 1
    var rest = xs
    while (!rest.isEmpty) {
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
    if (len == 0) { return EMPTY_INT_ARRAY }
    val a = new Array[Int](len)
    var i = len - 1
    var rest = xs
    while (!rest.isEmpty) {
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
    if (len == 0) { return EMPTY_LABEL_ARRAY }
    val a = new Array[asm.Label](len)
    var i = len - 1
    var rest = xs
    while (!rest.isEmpty) {
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
    assert(!(elem.isUnitType), s"The element type of an array can't be: $elem")
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
          if (kind.isIntSizedType) {
            emit(Opcodes.ICONST_M1)
            emit(Opcodes.IXOR)
          } else if (kind == LONG) {
            jmethod.visitLdcInsn(new java.lang.Long(-1))
            jmethod.visitInsn(Opcodes.LXOR)
          } else {
            abort(s"Impossible to negate an $kind")
          }

        case _ =>
          abort(s"Unknown arithmetic primitive $op")
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
        if (el.isArray || el.hasObjectSort) JAVA_LANG_OBJECT
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
            if (chosen != -1) { emit(chosen) }
          }

      if (from == to) { return }
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
            case _                => emit(L2I); emitT2T(INT, to)
          }

        case asm.Type.DOUBLE =>
          import asm.Opcodes.{ D2L, D2F, D2I }
          (to.sort: @switch) match {
            case asm.Type.FLOAT   => emit(D2F)
            case asm.Type.LONG    => emit(D2L)
            case _                => emit(D2I); emitT2T(INT, to)
          }
      }
    } // end of emitT2T()

    // can-multi-thread
    final def aconst(cst: AnyRef) {
      if (cst == null) { emit(Opcodes.ACONST_NULL) }
      else             { jmethod.visitLdcInsn(cst) }
    }

    // can-multi-thread
    final def boolconst(b: Boolean) { iconst(if (b) 1 else 0) }

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
      if (elem.isRefOrArrayType || elem.isPhantomType ) {
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
      val opc = (if (cond == icodes.EQ) Opcodes.IF_ACMPEQ else Opcodes.IF_ACMPNE)
      jmethod.visitJumpInsn(opc, label)
    }
    // can-multi-thread
    final def emitIFNONNULL(label: asm.Label) { jmethod.visitJumpInsn(Opcodes.IFNONNULL, label) }
    // can-multi-thread
    final def emitIFNULL   (label: asm.Label) { jmethod.visitJumpInsn(Opcodes.IFNULL,    label) }

    // can-multi-thread
    final def emitRETURN(tk: BType) {
      if (tk == UNIT) { emit(Opcodes.RETURN) }
      else            { emitTypeBased(JCodeMethodN.returnOpcodes, tk)      }
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
        if (keys(i-1) == keys(i)) {
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
        while (i < keyRange) {
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
        if (tk.isRefOrArrayType) {  opcs(0) }
        else if (tk.isIntSizedType) {
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
          case _               => opcs(0)
        }
      }
      emit(opc)
    }

    // can-multi-thread
    final def drop(tk: BType) { emit(if (tk.isWideType) Opcodes.POP2 else Opcodes.POP) }

    // can-multi-thread
    final def dup(tk: BType)  { emit(if (tk.isWideType) Opcodes.DUP2 else Opcodes.DUP) }

    // ---------------- type checks and casts ----------------

    // can-multi-thread
    final def isInstance(tk: BType) {
      jmethod.visitTypeInsn(Opcodes.INSTANCEOF, tk.getInternalName)
    }

    // can-multi-thread
    final def checkCast(tk: BType) {
      assert(tk.isRefOrArrayType, s"checkcast on primitive type: $tk")
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
      if (acc.isEmpty) {
        acc = saved
      } else {
        result += (tree -> acc)
        acc = acc ::: saved
      }
    }
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
      while (insnIter.hasNext) {
        f(insnIter.next())
      }
    }

    @inline final def toList: List[asm.tree.AbstractInsnNode] = {
      var result: List[asm.tree.AbstractInsnNode] = Nil
      lst foreachInsn { insn => if (insn != null) { result ::= insn }  }
      result.reverse
    }

  }

  /*
   *  Upon finding a name already seen among previous List elements, adds a numeric postfix to make it unique.
   */
  def uniquify(names: List[String]): List[String] = {
    val seen = mutable.Set.empty[String]

      @scala.annotation.tailrec def uniquified(current: String, attempt: Int): String = {
        if (seen contains current) {
          val currentBis = (current + "$" + attempt.toString)
          if (seen contains currentBis) {
            uniquified(current, attempt + 1)
          } else currentBis
        } else current
      }

    var rest = names
    var result: List[String] = Nil
    while (rest.nonEmpty) {
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
    while (iter.hasNext) {
      val nxt = iter.next()
      if (seen contains nxt) { return false }
      seen += nxt
    }
    true
  }

}