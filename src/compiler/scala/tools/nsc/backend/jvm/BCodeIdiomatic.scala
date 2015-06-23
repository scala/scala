/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package tools.nsc
package backend.jvm

import scala.tools.asm
import scala.annotation.switch
import scala.collection.mutable
import GenBCode._
import scala.tools.asm.tree.MethodInsnNode

/*
 *  A high-level facade to the ASM API for bytecode generation.
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded
 *  @version 1.0
 *
 */
abstract class BCodeIdiomatic extends SubComponent {
  val bTypes = new BTypesFromSymbols[global.type](global)

  import global._
  import bTypes._
  import coreBTypes._

  val classfileVersion: Int = settings.target.value match {
    case "jvm-1.5"     => asm.Opcodes.V1_5
    case "jvm-1.6"     => asm.Opcodes.V1_6
    case "jvm-1.7"     => asm.Opcodes.V1_7
    case "jvm-1.8"     => asm.Opcodes.V1_8
  }

  val majorVersion: Int = (classfileVersion & 0xFF)
  val emitStackMapFrame = (majorVersion >= 50)

  val extraProc: Int = GenBCode.mkFlags(
    asm.ClassWriter.COMPUTE_MAXS,
    if (emitStackMapFrame) asm.ClassWriter.COMPUTE_FRAMES else 0
  )

  val StringBuilderClassName = "scala/collection/mutable/StringBuilder"

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

  /* Just a namespace for utilities that encapsulate MethodVisitor idioms.
   *  In the ASM world, org.objectweb.asm.commons.InstructionAdapter plays a similar role,
   *  but the methods here allow choosing when to transition from ICode to ASM types
   *  (including not at all, e.g. for performance).
   */
  abstract class JCodeMethodN {

    def jmethod: asm.tree.MethodNode

    import asm.Opcodes;
    import icodes.opcodes.{ Static, Dynamic,  SuperCall }

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
    final def genStartConcat(pos: Position): Unit = {
      jmethod.visitTypeInsn(Opcodes.NEW, StringBuilderClassName)
      jmethod.visitInsn(Opcodes.DUP)
      invokespecial(
        StringBuilderClassName,
        INSTANCE_CONSTRUCTOR_NAME,
        "()V",
        pos
      )
    }

    /*
     * can-multi-thread
     */
    final def genStringConcat(el: BType, pos: Position): Unit = {

      val jtype =
        if (el.isArray || el.isClass) ObjectReference
        else el

      val bt = MethodBType(List(jtype), StringBuilderReference)

      invokevirtual(StringBuilderClassName, "append", bt.descriptor, pos)
    }

    /*
     * can-multi-thread
     */
    final def genEndConcat(pos: Position): Unit = {
      invokevirtual(StringBuilderClassName, "toString", "()Ljava/lang/String;", pos)
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
        from.isNonVoidPrimitiveType && to.isNonVoidPrimitiveType,
        s"Cannot emit primitive conversion from $from to $to - ${global.currentUnit}"
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
      (from: @unchecked) match {

        // using `asm.Type.SHORT` instead of `BType.SHORT` because otherwise "warning: could not emit switch for @switch annotated match"

        case BYTE  => pickOne(JCodeMethodN.fromByteT2T)
        case SHORT => pickOne(JCodeMethodN.fromShortT2T)
        case CHAR  => pickOne(JCodeMethodN.fromCharT2T)
        case INT   => pickOne(JCodeMethodN.fromIntT2T)

        case FLOAT  =>
          import asm.Opcodes.{ F2L, F2D, F2I }
          to match {
            case LONG    => emit(F2L)
            case DOUBLE  => emit(F2D)
            case _       => emit(F2I); emitT2T(INT, to)
          }

        case LONG   =>
          import asm.Opcodes.{ L2F, L2D, L2I }
          to match {
            case FLOAT   => emit(L2F)
            case DOUBLE  => emit(L2D)
            case _       => emit(L2I); emitT2T(INT, to)
          }

        case DOUBLE =>
          import asm.Opcodes.{ D2L, D2F, D2I }
          to match {
            case FLOAT   => emit(D2F)
            case LONG    => emit(D2L)
            case _       => emit(D2I); emitT2T(INT, to)
          }
      }
    } // end of emitT2T()

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
      elem match {
        case c: RefBType =>
          /* phantom type at play in `Array(null)`, SI-1513. On the other hand, Array(()) has element type `scala.runtime.BoxedUnit` which isObject. */
          jmethod.visitTypeInsn(Opcodes.ANEWARRAY, c.classOrArrayType)
        case _ =>
          assert(elem.isNonVoidPrimitiveType)
          val rand = {
            // using `asm.Type.SHORT` instead of `BType.SHORT` because otherwise "warning: could not emit switch for @switch annotated match"
            (elem: @unchecked) match {
              case BOOL   => Opcodes.T_BOOLEAN
              case BYTE   => Opcodes.T_BYTE
              case SHORT  => Opcodes.T_SHORT
              case CHAR   => Opcodes.T_CHAR
              case INT    => Opcodes.T_INT
              case LONG   => Opcodes.T_LONG
              case FLOAT  => Opcodes.T_FLOAT
              case DOUBLE => Opcodes.T_DOUBLE
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
    final def invokespecial(owner: String, name: String, desc: String, pos: Position) {
      addInvoke(Opcodes.INVOKESPECIAL, owner, name, desc, false, pos)
    }
    // can-multi-thread
    final def invokestatic(owner: String, name: String, desc: String, pos: Position) {
      addInvoke(Opcodes.INVOKESTATIC, owner, name, desc, false, pos)
    }
    // can-multi-thread
    final def invokeinterface(owner: String, name: String, desc: String, pos: Position) {
      addInvoke(Opcodes.INVOKEINTERFACE, owner, name, desc, true, pos)
    }
    // can-multi-thread
    final def invokevirtual(owner: String, name: String, desc: String, pos: Position) {
      addInvoke(Opcodes.INVOKEVIRTUAL, owner, name, desc, false, pos)
    }

    private def addInvoke(opcode: Int, owner: String, name: String, desc: String, itf: Boolean, pos: Position) = {
      val node = new MethodInsnNode(opcode, owner, name, desc, itf)
      jmethod.instructions.add(node)
      if (settings.YoptInlinerEnabled) callsitePositions(node) = pos
    }
    final def invokedynamic(owner: String, name: String, desc: String) {
      jmethod.visitMethodInsn(Opcodes.INVOKEDYNAMIC, owner, name, desc)
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

    /* Emits one of tableswitch or lookupswitch.
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
      jmethod.visitVarInsn(tk.typedOpcode(opc), idx)
    }

    // ---------------- array load and store ----------------

    // can-multi-thread
    final def emitTypeBased(opcs: Array[Int], tk: BType) {
      assert(tk != UNIT, tk)
      val opc = {
        if (tk.isRef) { opcs(0) }
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
        tk match {
          case LONG   => opcs(1)
          case FLOAT  => opcs(2)
          case DOUBLE => opcs(3)
          case _      => opcs(0)
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
    final def isInstance(tk: RefBType): Unit = {
      jmethod.visitTypeInsn(Opcodes.INSTANCEOF, tk.classOrArrayType)
    }

    // can-multi-thread
    final def checkCast(tk: RefBType): Unit = {
      // TODO ICode also requires: but that's too much, right? assert(!isBoxedType(tk),     "checkcast on boxed type: " + tk)
      jmethod.visitTypeInsn(Opcodes.CHECKCAST, tk.classOrArrayType)
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
    (code: @switch) match {
      case B2B | C2B | S2B | I2B | L2B | F2B | D2B => BYTE
      case B2C | C2C | S2C | I2C | L2C | F2C | D2C => CHAR
      case B2S | C2S | S2S | I2S | L2S | F2S | D2S => SHORT
      case B2I | C2I | S2I | I2I | L2I | F2I | D2I => INT
      case B2L | C2L | S2L | I2L | L2L | F2L | D2L => LONG
      case B2F | C2F | S2F | I2F | L2F | F2F | D2F => FLOAT
      case B2D | C2D | S2D | I2D | L2D | F2D | D2D => DOUBLE
    }
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

  implicit class InsnIterMethodNode(mnode: asm.tree.MethodNode) {
    @inline final def foreachInsn(f: (asm.tree.AbstractInsnNode) => Unit) { mnode.instructions.foreachInsn(f) }
  }

  implicit class InsnIterInsnList(lst: asm.tree.InsnList) {

    @inline final def foreachInsn(f: (asm.tree.AbstractInsnNode) => Unit) {
      val insnIter = lst.iterator()
      while (insnIter.hasNext) {
        f(insnIter.next())
      }
    }
  }
}
