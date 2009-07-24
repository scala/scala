/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author Iulian Dragos
 */
// $Id$

package scala.tools.nsc
package symtab
package classfile

import java.io.IOException

import scala.collection.mutable._
import scala.tools.nsc._
import scala.tools.nsc.backend.icode._
import scala.tools.nsc.io._
import scala.tools.nsc.util.{Position,NoPosition}

import ClassfileConstants._
import Flags._

/** ICode reader from Java bytecode.
 *
 *  @author Iulian Dragos
 *  @version 1.0
 */
abstract class ICodeReader extends ClassfileParser {
  val global: Global
  import global._
  import icodes._

  var instanceCode: IClass = null          // the ICode class for the current symbol
  var staticCode:   IClass = null          // the ICode class static members
  var method: IMethod = _                  // the current IMethod

  val OBJECT: TypeKind = REFERENCE(definitions.ObjectClass)
  val nothingName = newTermName("scala.runtime.Nothing$")
  val nullName    = newTermName("scala.runtime.Null$")
  var isScalaModule = false

  /** Read back bytecode for the given class symbol. It returns
   *  two IClass objects, one for static members and one
   *  for non-static members.
   */
  def readClass(cls: Symbol): (IClass, IClass) = {
    var classFile: AbstractFile = null;
    var sym = cls
    sym.info // ensure accurate type information

    isScalaModule = cls.isModule && !cls.hasFlag(JAVA)
    log("Reading class: " + cls + " isScalaModule?: " + isScalaModule)
    val name = cls.fullNameString(java.io.File.separatorChar) + (if (sym.hasFlag(MODULE)) "$" else "")
    val entry = classPath.root.find(name, false)
    if (entry ne null) {
      classFile = entry.classFile
//      if (isScalaModule)
//        sym = cls.linkedClassOfModule
      assert(classFile ne null, "No classfile for " + cls)

//    for (s <- cls.info.members)
//      Console.println("" + s + ": " + s.tpe)
      parse(classFile, sym)
    } else
      log("Could not find: " + cls)

    (staticCode, instanceCode)
  }

  /** If we're parsing a scala module, the owner of members is always
   *  the module symbol.
   */
  override def getOwner(jflags: Int): Symbol =
    if (isScalaModule) this.staticModule
    else super.getOwner(jflags)

  override def parseClass() {
    this.instanceCode = new IClass(clazz)
    this.staticCode   = new IClass(staticModule)
    val jflags = in.nextChar
    val isAttribute = (jflags & JAVA_ACC_ANNOTATION) != 0
    var sflags = transFlags(jflags, true)
    if ((sflags & DEFERRED) != 0) sflags = sflags & ~DEFERRED | ABSTRACT
    val c = pool.getClassSymbol(in.nextChar)

    parseInnerClasses()

    in.skip(2)               // super class
    in.skip(2 * in.nextChar) // interfaces
    val fieldCount = in.nextChar
    for (i <- 0 until fieldCount) parseField()
    val methodCount = in.nextChar
    for (i <- 0 until methodCount) parseMethod();
    instanceCode.methods = instanceCode.methods.reverse
    staticCode.methods = staticCode.methods.reverse
  }

  override def parseField() {
    val (jflags, sym) = parseMember(true)
    getCode(jflags).addField(new IField(sym))
    skipAttributes()
  }

  private def parseMember(field: Boolean): (Int, Symbol) = {
    val jflags = in.nextChar
    val name = pool.getName(in.nextChar)

    val owner = getOwner(jflags)
    val dummySym = owner.newMethod(owner.pos, name).setFlag(javaToScalaFlags(jflags))

    var tpe  = pool.getType(dummySym, in.nextChar)

    if ("<clinit>" == name.toString)
      (jflags, NoSymbol)
    else {
      val owner = getOwner(jflags)
      var sym = owner.info.member(name).suchThat(old => sameType(old.tpe, tpe));
      if (sym == NoSymbol)
        sym = owner.info.member(newTermName(name.toString + nme.LOCAL_SUFFIX)).suchThat(old => old.tpe =:= tpe);
      if (sym == NoSymbol) {
        log("Could not find symbol for " + name + ": " + tpe + " in " + owner.info.decls)
        log(owner.info.member(name).tpe + " : " + tpe)
        if (field)
          sym = owner.newValue(owner.pos, name).setInfo(tpe).setFlag(MUTABLE | javaToScalaFlags(jflags))
        else
          sym = dummySym.setInfo(tpe)
        owner.info.decls.enter(sym)
        log("added " + sym + ": " + sym.tpe)
      }
      (jflags, sym)
    }
  }

  private def javaToScalaFlags(flags: Int): Long = {
    import ch.epfl.lamp.fjbg.JAccessFlags._

    var res = 0L
    if ((flags & ACC_PRIVATE) != 0) res |= Flags.PRIVATE
    if ((flags & ACC_PROTECTED) != 0) res |= Flags.PROTECTED
    if ((flags & ACC_FINAL) != 0) res |= Flags.FINAL
    if ((flags & ACC_ABSTRACT) != 0) res |= Flags.DEFERRED
    if ((flags & ACC_SYNTHETIC) != 0) res |= Flags.SYNTHETIC

    res
  }

  /** Checks if tp1 is the same type as tp2, modulo implict methods.
   *  We don't care about the distinction between implcit and explicit
   *  methods as this point, and we can't get back the information from
   *  bytecode anyway.
   */
  private def sameType(tp1: Type, tp2: Type): Boolean = (tp1, tp2) match {
    case (MethodType(args1, resTpe1), MethodType(args2, resTpe2)) =>
      if (tp1.isInstanceOf[ImplicitMethodType] || tp2.isInstanceOf[ImplicitMethodType]) {
        MethodType(args1, resTpe1) =:= MethodType(args2, resTpe2)
      } else
        tp1 =:= tp2
    case _ => tp1 =:= tp2
  }

  override def parseMethod() {
    val (jflags, sym) = parseMember(false)
    if (sym != NoSymbol) {
      log("Parsing method " + sym.fullNameString + ": " + sym.tpe);
      this.method = new IMethod(sym);
      this.method.returnType = toTypeKind(sym.tpe.resultType)
      getCode(jflags).addMethod(this.method)
      if ((jflags & JAVA_ACC_NATIVE) != 0)
        this.method.native = true
      val attributeCount = in.nextChar
      for (i <- 0 until attributeCount) parseAttribute()
    } else {
      if (settings.debug.value) log("Skipping non-existent method.");
      skipAttributes();
    }
  }

  def parseAttribute() {
    val attrName = pool.getName(in.nextChar)
    val attrLen = in.nextInt
    attrName match {
      case nme.CodeATTR =>
        parseByteCode()
      case _ =>
        in.skip(attrLen)
    }
  }

  override def classNameToSymbol(name: Name) = {
    val sym = if (name == nothingName)
      definitions.NothingClass
    else if (name == nullName)
      definitions.NullClass
    else if (name.endsWith("$class")) {
      val iface = definitions.getClass(name.subName(0, name.length - "$class".length))
      log("forcing " + iface.owner + " at phase: " + phase + " impl: " + iface.implClass)
      iface.owner.info // force the mixin type-transformer
      definitions.getClass(name)
    } else if (name.endsWith("$")) {
      val sym = forceMangledName(name.subName(0, name.length -1), true)
      if (sym == NoSymbol)
        definitions.getModule(name.subName(0, name.length - 1))
      else sym
    } else {
      forceMangledName(name, false)
      definitions.getClass(name)
    }
    if (sym.isModule)
      sym.moduleClass
    else
      sym
  }


  var maxStack: Int = _
  var maxLocals: Int = _
  val JVM = ClassfileConstants // shorter, uppercase alias for use in case patterns

  def toUnsignedByte(b: Byte): Int = b.toInt & 0xff
  var pc = 0

  /** Parse java bytecode into ICode */
  def parseByteCode() {
    maxStack = in.nextChar
    maxLocals = in.nextChar
    val codeLength = in.nextInt
    val code = new LinearCode

    def parseInstruction {
      import opcodes._
      import code._
      var size = 1 // instruction size

      /** Parse 16 bit jump target. */
      def parseJumpTarget = {
        size = size + 2
        val offset = in.nextChar.asInstanceOf[Short]
        val target = pc + offset
        assert(target >= 0 && target < codeLength, "Illegal jump target: " + target)
        target
      }

      /** Parse 32 bit jump target. */
      def parseJumpTargetW: Int = {
        size += 4
        val offset = in.nextInt
        val target = pc + offset
        assert(target >= 0 && target < codeLength, "Illegal jump target: " + target + "pc: " + pc + " offset: " + offset)
        target
      }

      val instr = toUnsignedByte(in.nextByte)
      instr match {
        case JVM.nop => parseInstruction
        case JVM.aconst_null => code.emit(CONSTANT(Constant(null)))
        case JVM.iconst_m1   => code.emit(CONSTANT(Constant(-1)))
        case JVM.iconst_0    => code.emit(CONSTANT(Constant(0)))
        case JVM.iconst_1    => code.emit(CONSTANT(Constant(1)))
        case JVM.iconst_2    => code.emit(CONSTANT(Constant(2)))
        case JVM.iconst_3    => code.emit(CONSTANT(Constant(3)))
        case JVM.iconst_4    => code.emit(CONSTANT(Constant(4)))
        case JVM.iconst_5    => code.emit(CONSTANT(Constant(5)))

        case JVM.lconst_0    => code.emit(CONSTANT(Constant(0l)))
        case JVM.lconst_1    => code.emit(CONSTANT(Constant(1l)))
        case JVM.fconst_0    => code.emit(CONSTANT(Constant(0.0f)))
        case JVM.fconst_1    => code.emit(CONSTANT(Constant(1.0f)))
        case JVM.fconst_2    => code.emit(CONSTANT(Constant(2.0f)))
        case JVM.dconst_0    => code.emit(CONSTANT(Constant(0.0)))
        case JVM.dconst_1    => code.emit(CONSTANT(Constant(1.0)))

        case JVM.bipush      => code.emit(CONSTANT(Constant(in.nextByte))); size += 1
        case JVM.sipush      => code.emit(CONSTANT(Constant(in.nextChar))); size += 2
        case JVM.ldc         => code.emit(CONSTANT(pool.getConstant(toUnsignedByte(in.nextByte)))); size += 1
        case JVM.ldc_w       => code.emit(CONSTANT(pool.getConstant(in.nextChar))); size += 2
        case JVM.ldc2_w      => code.emit(CONSTANT(pool.getConstant(in.nextChar))); size += 2
        case JVM.iload       => code.emit(LOAD_LOCAL(code.getLocal(in.nextByte, INT)));    size += 1
        case JVM.lload       => code.emit(LOAD_LOCAL(code.getLocal(in.nextByte, LONG)));   size += 1
        case JVM.fload       => code.emit(LOAD_LOCAL(code.getLocal(in.nextByte, FLOAT)));  size += 1
        case JVM.dload       => code.emit(LOAD_LOCAL(code.getLocal(in.nextByte, DOUBLE))); size += 1
        case JVM.aload       =>
          val local = in.nextByte; size += 1
          if (local == 0 && !method.isStatic)
            code.emit(THIS(method.symbol.owner));
          else
            code.emit(LOAD_LOCAL(code.getLocal(local, OBJECT)));

        case JVM.iload_0     => code.emit(LOAD_LOCAL(code.getLocal(0, INT)))
        case JVM.iload_1     => code.emit(LOAD_LOCAL(code.getLocal(1, INT)))
        case JVM.iload_2     => code.emit(LOAD_LOCAL(code.getLocal(2, INT)))
        case JVM.iload_3     => code.emit(LOAD_LOCAL(code.getLocal(3, INT)))
        case JVM.lload_0     => code.emit(LOAD_LOCAL(code.getLocal(0, LONG)))
        case JVM.lload_1     => code.emit(LOAD_LOCAL(code.getLocal(1, LONG)))
        case JVM.lload_2     => code.emit(LOAD_LOCAL(code.getLocal(2, LONG)))
        case JVM.lload_3     => code.emit(LOAD_LOCAL(code.getLocal(3, LONG)))
        case JVM.fload_0     => code.emit(LOAD_LOCAL(code.getLocal(0, FLOAT)))
        case JVM.fload_1     => code.emit(LOAD_LOCAL(code.getLocal(1, FLOAT)))
        case JVM.fload_2     => code.emit(LOAD_LOCAL(code.getLocal(2, FLOAT)))
        case JVM.fload_3     => code.emit(LOAD_LOCAL(code.getLocal(3, FLOAT)))
        case JVM.dload_0     => code.emit(LOAD_LOCAL(code.getLocal(0, DOUBLE)))
        case JVM.dload_1     => code.emit(LOAD_LOCAL(code.getLocal(1, DOUBLE)))
        case JVM.dload_2     => code.emit(LOAD_LOCAL(code.getLocal(2, DOUBLE)))
        case JVM.dload_3     => code.emit(LOAD_LOCAL(code.getLocal(3, DOUBLE)))
        case JVM.aload_0     =>
          if (!method.isStatic)
            code.emit(THIS(method.symbol.owner));
          else
            code.emit(LOAD_LOCAL(code.getLocal(0, OBJECT)));
        case JVM.aload_1     => code.emit(LOAD_LOCAL(code.getLocal(1, OBJECT)))
        case JVM.aload_2     => code.emit(LOAD_LOCAL(code.getLocal(2, OBJECT)))
        case JVM.aload_3     => code.emit(LOAD_LOCAL(code.getLocal(3, OBJECT)))

        case JVM.iaload      => code.emit(LOAD_ARRAY_ITEM(INT))
        case JVM.laload      => code.emit(LOAD_ARRAY_ITEM(LONG))
        case JVM.faload      => code.emit(LOAD_ARRAY_ITEM(FLOAT))
        case JVM.daload      => code.emit(LOAD_ARRAY_ITEM(DOUBLE))
        case JVM.aaload      => code.emit(LOAD_ARRAY_ITEM(OBJECT))
        case JVM.baload      => code.emit(LOAD_ARRAY_ITEM(BYTE))
        case JVM.caload      => code.emit(LOAD_ARRAY_ITEM(CHAR))
        case JVM.saload      => code.emit(LOAD_ARRAY_ITEM(SHORT))

        case JVM.istore      => code.emit(STORE_LOCAL(code.getLocal(in.nextByte, INT)));    size += 1
        case JVM.lstore      => code.emit(STORE_LOCAL(code.getLocal(in.nextByte, LONG)));   size += 1
        case JVM.fstore      => code.emit(STORE_LOCAL(code.getLocal(in.nextByte, FLOAT)));  size += 1
        case JVM.dstore      => code.emit(STORE_LOCAL(code.getLocal(in.nextByte, DOUBLE))); size += 1
        case JVM.astore      => code.emit(STORE_LOCAL(code.getLocal(in.nextByte, OBJECT))); size += 1
        case JVM.istore_0    => code.emit(STORE_LOCAL(code.getLocal(0, INT)))
        case JVM.istore_1    => code.emit(STORE_LOCAL(code.getLocal(1, INT)))
        case JVM.istore_2    => code.emit(STORE_LOCAL(code.getLocal(2, INT)))
        case JVM.istore_3    => code.emit(STORE_LOCAL(code.getLocal(3, INT)))
        case JVM.lstore_0    => code.emit(STORE_LOCAL(code.getLocal(0, LONG)))
        case JVM.lstore_1    => code.emit(STORE_LOCAL(code.getLocal(1, LONG)))
        case JVM.lstore_2    => code.emit(STORE_LOCAL(code.getLocal(2, LONG)))
        case JVM.lstore_3    => code.emit(STORE_LOCAL(code.getLocal(3, LONG)))
        case JVM.fstore_0    => code.emit(STORE_LOCAL(code.getLocal(0, FLOAT)))
        case JVM.fstore_1    => code.emit(STORE_LOCAL(code.getLocal(1, FLOAT)))
        case JVM.fstore_2    => code.emit(STORE_LOCAL(code.getLocal(2, FLOAT)))
        case JVM.fstore_3    => code.emit(STORE_LOCAL(code.getLocal(3, FLOAT)))
        case JVM.dstore_0    => code.emit(STORE_LOCAL(code.getLocal(0, DOUBLE)))
        case JVM.dstore_1    => code.emit(STORE_LOCAL(code.getLocal(1, DOUBLE)))
        case JVM.dstore_2    => code.emit(STORE_LOCAL(code.getLocal(2, DOUBLE)))
        case JVM.dstore_3    => code.emit(STORE_LOCAL(code.getLocal(3, DOUBLE)))
        case JVM.astore_0    => code.emit(STORE_LOCAL(code.getLocal(0, OBJECT)))
        case JVM.astore_1    => code.emit(STORE_LOCAL(code.getLocal(1, OBJECT)))
        case JVM.astore_2    => code.emit(STORE_LOCAL(code.getLocal(2, OBJECT)))
        case JVM.astore_3    => code.emit(STORE_LOCAL(code.getLocal(3, OBJECT)))
        case JVM.iastore     => code.emit(STORE_ARRAY_ITEM(INT))
        case JVM.lastore     => code.emit(STORE_ARRAY_ITEM(LONG))
        case JVM.fastore     => code.emit(STORE_ARRAY_ITEM(FLOAT))
        case JVM.dastore     => code.emit(STORE_ARRAY_ITEM(DOUBLE))
        case JVM.aastore     => code.emit(STORE_ARRAY_ITEM(OBJECT))
        case JVM.bastore     => code.emit(STORE_ARRAY_ITEM(BYTE))
        case JVM.castore     => code.emit(STORE_ARRAY_ITEM(CHAR))
        case JVM.sastore     => code.emit(STORE_ARRAY_ITEM(SHORT))

        case JVM.pop         => code.emit(DROP(INT))   // any 1-word type would do
        case JVM.pop2        => code.emit(DROP(LONG))  // any 2-word type would do
        case JVM.dup         => code.emit(DUP(OBJECT)) // TODO: Is the kind inside DUP ever needed?
        case JVM.dup_x1      => code.emit(DUP_X1)      // Predef.error("Unsupported JVM bytecode: dup_x1")
        case JVM.dup_x2      => code.emit(DUP_X2)      // Predef.error("Unsupported JVM bytecode: dup_x2")
        case JVM.dup2        => code.emit(DUP(LONG))   // TODO: Is the kind inside DUP ever needed?
        case JVM.dup2_x1     => code.emit(DUP2_X1)     // Predef.error("Unsupported JVM bytecode: dup2_x1")
        case JVM.dup2_x2     => code.emit(DUP2_X2)     // Predef.error("Unsupported JVM bytecode: dup2_x2")
        case JVM.swap        => Predef.error("Unsupported JVM bytecode: swap")

        case JVM.iadd        => code.emit(CALL_PRIMITIVE(Arithmetic(ADD, INT)))
        case JVM.ladd        => code.emit(CALL_PRIMITIVE(Arithmetic(ADD, LONG)))
        case JVM.fadd        => code.emit(CALL_PRIMITIVE(Arithmetic(ADD, FLOAT)))
        case JVM.dadd        => code.emit(CALL_PRIMITIVE(Arithmetic(ADD, DOUBLE)))
        case JVM.isub        => code.emit(CALL_PRIMITIVE(Arithmetic(SUB, INT)))
        case JVM.lsub        => code.emit(CALL_PRIMITIVE(Arithmetic(SUB, LONG)))
        case JVM.fsub        => code.emit(CALL_PRIMITIVE(Arithmetic(SUB, FLOAT)))
        case JVM.dsub        => code.emit(CALL_PRIMITIVE(Arithmetic(SUB, DOUBLE)))
        case JVM.imul        => code.emit(CALL_PRIMITIVE(Arithmetic(MUL, INT)))
        case JVM.lmul        => code.emit(CALL_PRIMITIVE(Arithmetic(MUL, LONG)))
        case JVM.fmul        => code.emit(CALL_PRIMITIVE(Arithmetic(MUL, FLOAT)))
        case JVM.dmul        => code.emit(CALL_PRIMITIVE(Arithmetic(MUL, DOUBLE)))
        case JVM.idiv        => code.emit(CALL_PRIMITIVE(Arithmetic(DIV, INT)))
        case JVM.ldiv        => code.emit(CALL_PRIMITIVE(Arithmetic(DIV, LONG)))
        case JVM.fdiv        => code.emit(CALL_PRIMITIVE(Arithmetic(DIV, FLOAT)))
        case JVM.ddiv        => code.emit(CALL_PRIMITIVE(Arithmetic(DIV, DOUBLE)))
        case JVM.irem        => code.emit(CALL_PRIMITIVE(Arithmetic(REM, INT)))
        case JVM.lrem        => code.emit(CALL_PRIMITIVE(Arithmetic(REM, LONG)))
        case JVM.frem        => code.emit(CALL_PRIMITIVE(Arithmetic(REM, FLOAT)))
        case JVM.drem        => code.emit(CALL_PRIMITIVE(Arithmetic(REM, DOUBLE)))

        case JVM.ineg        => code.emit(CALL_PRIMITIVE(Negation(INT)))
        case JVM.lneg        => code.emit(CALL_PRIMITIVE(Negation(LONG)))
        case JVM.fneg        => code.emit(CALL_PRIMITIVE(Negation(FLOAT)))
        case JVM.dneg        => code.emit(CALL_PRIMITIVE(Negation(DOUBLE)))

        case JVM.ishl        => code.emit(CALL_PRIMITIVE(Shift(LSL, INT)))
        case JVM.lshl        => code.emit(CALL_PRIMITIVE(Shift(LSL, LONG)))
        case JVM.ishr        => code.emit(CALL_PRIMITIVE(Shift(LSR, INT)))
        case JVM.lshr        => code.emit(CALL_PRIMITIVE(Shift(LSR, LONG)))
        case JVM.iushr       => code.emit(CALL_PRIMITIVE(Shift(ASR, INT)))
        case JVM.lushr       => code.emit(CALL_PRIMITIVE(Shift(ASR, LONG)))
        case JVM.iand        => code.emit(CALL_PRIMITIVE(Logical(AND, INT)))
        case JVM.land        => code.emit(CALL_PRIMITIVE(Logical(AND, LONG)))
        case JVM.ior         => code.emit(CALL_PRIMITIVE(Logical(OR, INT)))
        case JVM.lor         => code.emit(CALL_PRIMITIVE(Logical(OR, LONG)))
        case JVM.ixor        => code.emit(CALL_PRIMITIVE(Logical(XOR, INT)))
        case JVM.lxor        => code.emit(CALL_PRIMITIVE(Logical(XOR, LONG)))
        case JVM.iinc        =>
          size += 2
          val local = code.getLocal(in.nextByte, INT)
          code.emit(LOAD_LOCAL(local))
          code.emit(CONSTANT(Constant(in.nextByte)))
          code.emit(CALL_PRIMITIVE(Arithmetic(ADD, INT)))
          code.emit(STORE_LOCAL(local))

        case JVM.i2l         => code.emit(CALL_PRIMITIVE(Conversion(INT, LONG)))
        case JVM.i2f         => code.emit(CALL_PRIMITIVE(Conversion(INT, FLOAT)))
        case JVM.i2d         => code.emit(CALL_PRIMITIVE(Conversion(INT, DOUBLE)))
        case JVM.l2i         => code.emit(CALL_PRIMITIVE(Conversion(LONG, INT)))
        case JVM.l2f         => code.emit(CALL_PRIMITIVE(Conversion(LONG, FLOAT)))
        case JVM.l2d         => code.emit(CALL_PRIMITIVE(Conversion(LONG, DOUBLE)))
        case JVM.f2i         => code.emit(CALL_PRIMITIVE(Conversion(FLOAT, INT)))
        case JVM.f2l         => code.emit(CALL_PRIMITIVE(Conversion(FLOAT, LONG)))
        case JVM.f2d         => code.emit(CALL_PRIMITIVE(Conversion(FLOAT, DOUBLE)))
        case JVM.d2i         => code.emit(CALL_PRIMITIVE(Conversion(DOUBLE, INT)))
        case JVM.d2l         => code.emit(CALL_PRIMITIVE(Conversion(DOUBLE, LONG)))
        case JVM.d2f         => code.emit(CALL_PRIMITIVE(Conversion(DOUBLE, FLOAT)))
        case JVM.i2b         => code.emit(CALL_PRIMITIVE(Conversion(INT, BYTE)))
        case JVM.i2c         => code.emit(CALL_PRIMITIVE(Conversion(INT, CHAR)))
        case JVM.i2s         => code.emit(CALL_PRIMITIVE(Conversion(INT, SHORT)))

        case JVM.lcmp        => code.emit(CALL_PRIMITIVE(Comparison(CMP, LONG)))
        case JVM.fcmpl       => code.emit(CALL_PRIMITIVE(Comparison(CMPL, FLOAT)))
        case JVM.fcmpg       => code.emit(CALL_PRIMITIVE(Comparison(CMPG, FLOAT)))
        case JVM.dcmpl       => code.emit(CALL_PRIMITIVE(Comparison(CMPL, DOUBLE)))
        case JVM.dcmpg       => code.emit(CALL_PRIMITIVE(Comparison(CMPG, DOUBLE)))

        case JVM.ifeq        => code.emit(LCZJUMP(parseJumpTarget, pc + size, EQ, INT))
        case JVM.ifne        => code.emit(LCZJUMP(parseJumpTarget, pc + size, NE, INT))
        case JVM.iflt        => code.emit(LCZJUMP(parseJumpTarget, pc + size, LT, INT))
        case JVM.ifge        => code.emit(LCZJUMP(parseJumpTarget, pc + size, GE, INT))
        case JVM.ifgt        => code.emit(LCZJUMP(parseJumpTarget, pc + size, GT, INT))
        case JVM.ifle        => code.emit(LCZJUMP(parseJumpTarget, pc + size, LE, INT))

        case JVM.if_icmpeq   => code.emit(LCJUMP(parseJumpTarget, pc + size, EQ, INT))
        case JVM.if_icmpne   => code.emit(LCJUMP(parseJumpTarget, pc + size, NE, INT))
        case JVM.if_icmplt   => code.emit(LCJUMP(parseJumpTarget, pc + size, LT, INT))
        case JVM.if_icmpge   => code.emit(LCJUMP(parseJumpTarget, pc + size, GE, INT))
        case JVM.if_icmpgt   => code.emit(LCJUMP(parseJumpTarget, pc + size, GT, INT))
        case JVM.if_icmple   => code.emit(LCJUMP(parseJumpTarget, pc + size, LE, INT))
        case JVM.if_acmpeq   => code.emit(LCJUMP(parseJumpTarget, pc + size, EQ, OBJECT))
        case JVM.if_acmpne   => code.emit(LCJUMP(parseJumpTarget, pc + size, NE, OBJECT))

        case JVM.goto        => emit(LJUMP(parseJumpTarget))
        case JVM.jsr         => Predef.error("Cannot handle jsr/ret")
        case JVM.ret         => Predef.error("Cannot handle jsr/ret")
        case JVM.tableswitch =>
          val padding = if ((pc + size) % 4 != 0) 4 - ((pc + size) % 4) else 0
          size += padding
          in.bp += padding
          assert((pc + size % 4) != 0)
/*          var byte1 = in.nextByte; size += 1;
          while (byte1 == 0) { byte1 = in.nextByte; size += 1; }
          val default = byte1 << 24 | in.nextByte << 16 | in.nextByte << 8 | in.nextByte;
          size = size + 3
       */
          val default = pc + in.nextInt; size += 4
          val low  = in.nextInt
          val high = in.nextInt
          size += 8
          assert(low <= high, "Value low not <= high for tableswitch.")

          val tags = List.tabulate(high - low + 1)(n => List(low + n))
          val targets = for (_ <- tags) yield parseJumpTargetW
          code.emit(LSWITCH(tags, targets ::: List(default)))

        case JVM.lookupswitch =>
          val padding = if ((pc + size) % 4 != 0) 4 - ((pc + size) % 4) else 0
          size += padding
          in.bp += padding
          assert((pc + size % 4) != 0)
          val default = pc + in.nextInt; size += 4
          val npairs = in.nextInt; size += 4
          var tags: List[List[Int]] = Nil
          var targets: List[Int] = Nil
          var i = 0
          while (i < npairs) {
            tags = List(in.nextInt) :: tags; size += 4
            targets = parseJumpTargetW :: targets; // parseJumpTargetW updates 'size' itself
            i += 1
          }
          targets = default :: targets
          code.emit(LSWITCH(tags.reverse, targets.reverse))

        case JVM.ireturn     => code.emit(RETURN(INT))
        case JVM.lreturn     => code.emit(RETURN(LONG))
        case JVM.freturn     => code.emit(RETURN(FLOAT))
        case JVM.dreturn     => code.emit(RETURN(DOUBLE))
        case JVM.areturn     => code.emit(RETURN(OBJECT))
        case JVM.return_     => code.emit(RETURN(UNIT))

        case JVM.getstatic    =>
          val field = pool.getMemberSymbol(in.nextChar, true); size += 2
          if (field.hasFlag(Flags.MODULE))
            code.emit(LOAD_MODULE(field))
          else
            code.emit(LOAD_FIELD(field, true))
        case JVM.putstatic   =>
          val field = pool.getMemberSymbol(in.nextChar, true); size += 2
          code.emit(STORE_FIELD(field, true))
        case JVM.getfield    =>
          val field = pool.getMemberSymbol(in.nextChar, false); size += 2
          code.emit(LOAD_FIELD(field, false))
        case JVM.putfield    =>
          val field = pool.getMemberSymbol(in.nextChar, false); size += 2
          code.emit(STORE_FIELD(field, false))

        case JVM.invokevirtual =>
          val m = pool.getMemberSymbol(in.nextChar, false); size += 2
          code.emit(CALL_METHOD(m, Dynamic))
        case JVM.invokeinterface  =>
          val m = pool.getMemberSymbol(in.nextChar, false); size += 4
          in.skip(2)
          code.emit(CALL_METHOD(m, Dynamic))
        case JVM.invokespecial   =>
          val m = pool.getMemberSymbol(in.nextChar, false); size += 2
          val style = if (m.name == nme.CONSTRUCTOR || m.hasFlag(Flags.PRIVATE)) Static(true)
                      else SuperCall(m.owner.name);
          code.emit(CALL_METHOD(m, style))
        case JVM.invokestatic    =>
          val m = pool.getMemberSymbol(in.nextChar, true); size += 2
          if (isBox(m))
            code.emit(BOX(toTypeKind(m.info.paramTypes.head)))
          else if (isUnbox(m))
            code.emit(UNBOX(toTypeKind(m.info.resultType)))
          else
            code.emit(CALL_METHOD(m, Static(false)))

        case JVM.new_          =>
          code.emit(NEW(REFERENCE(pool.getClassSymbol(in.nextChar))))
          size += 2
        case JVM.newarray      =>
          val kind = in.nextByte match {
            case T_BOOLEAN => BOOL
            case T_CHAR    => CHAR
            case T_FLOAT   => FLOAT
            case T_DOUBLE  => DOUBLE
            case T_BYTE    => BYTE
            case T_SHORT   => SHORT
            case T_INT     => INT
            case T_LONG    => LONG
          }
          size += 1
          code.emit(CREATE_ARRAY(kind, 1))

        case JVM.anewarray     =>
          val tpe = pool.getClassOrArrayType(in.nextChar); size += 2
          code.emit(CREATE_ARRAY(toTypeKind(tpe), 1))

        case JVM.arraylength   => code.emit(CALL_PRIMITIVE(ArrayLength(OBJECT))); // the kind does not matter
        case JVM.athrow        => code.emit(THROW());
        case JVM.checkcast     =>
          code.emit(CHECK_CAST(toTypeKind(pool.getClassOrArrayType(in.nextChar)))); size += 2
        case JVM.instanceof    =>
          code.emit(IS_INSTANCE(toTypeKind(pool.getClassOrArrayType(in.nextChar)))); size += 2
        case JVM.monitorenter  => code.emit(MONITOR_ENTER())
        case JVM.monitorexit   => code.emit(MONITOR_EXIT())
        case JVM.wide          =>
          size += 1
          toUnsignedByte(in.nextByte) match {
            case JVM.iload  => code.emit(LOAD_LOCAL(code.getLocal(in.nextChar, INT)));    size += 2
            case JVM.lload  => code.emit(LOAD_LOCAL(code.getLocal(in.nextChar, LONG)));   size += 2
            case JVM.fload  => code.emit(LOAD_LOCAL(code.getLocal(in.nextChar, FLOAT)));  size += 2
            case JVM.dload  => code.emit(LOAD_LOCAL(code.getLocal(in.nextChar, DOUBLE))); size += 2
            case JVM.aload  => code.emit(LOAD_LOCAL(code.getLocal(in.nextChar, OBJECT))); size += 2
            case JVM.istore => code.emit(STORE_LOCAL(code.getLocal(in.nextChar, INT)));    size += 2
            case JVM.lstore => code.emit(STORE_LOCAL(code.getLocal(in.nextChar, LONG)));   size += 2
            case JVM.fstore => code.emit(STORE_LOCAL(code.getLocal(in.nextChar, FLOAT)));  size += 2
            case JVM.dstore => code.emit(STORE_LOCAL(code.getLocal(in.nextChar, DOUBLE))); size += 2
            case JVM.astore => code.emit(STORE_LOCAL(code.getLocal(in.nextChar, OBJECT))); size += 2
            case JVM.ret => Predef.error("Cannot handle jsr/ret")
            case JVM.iinc =>
              size += 4
              val local = code.getLocal(in.nextChar, INT)
              code.emit(CONSTANT(Constant(in.nextChar)))
              code.emit(CALL_PRIMITIVE(Arithmetic(ADD, INT)))
              code.emit(STORE_LOCAL(local))
            case _ => Predef.error("Invalid 'wide' operand")
          }

        case JVM.multianewarray =>
          size += 3
          val tpe = toTypeKind(pool.getClassOrArrayType(in.nextChar))
          val dim = in.nextByte
//          assert(dim == 1, "Cannot handle multidimensional arrays yet.")
          code.emit(CREATE_ARRAY(tpe, dim))

        case JVM.ifnull    => code.emit(LCZJUMP(parseJumpTarget, pc + size, EQ, OBJECT))
        case JVM.ifnonnull => code.emit(LCZJUMP(parseJumpTarget, pc + size, NE, OBJECT))
        case JVM.goto_w    => code.emit(LJUMP(parseJumpTargetW))
        case JVM.jsr_w     => Predef.error("Cannot handle jsr/ret")

//        case _ => Predef.error("Unknown bytecode")
      }
      pc += size
    }

    // add parameters
    var idx = if (method.isStatic) 0 else 1
    for (t <- method.symbol.tpe.paramTypes) {
      this.method.addParam(code.enterParam(idx, toTypeKind(t)))
      idx += 1
    }

    pc = 0
    while (pc < codeLength) parseInstruction

    val exceptionEntries = in.nextChar.toInt
    var i = 0
    while (i < exceptionEntries) {
      // skip start end PC
      in.skip(4)
      // read the handler PC
      code.jmpTargets += in.nextChar
      // skip the exception type
      in.skip(2)
      i += 1
    }
    skipAttributes()

    code.toBasicBlock
    assert(method.code ne null)
    // reverse parameters, as they were prepended during code generation
    method.params = method.params.reverse
    if (code.containsDUPX) {
      code.resolveDups
    }
    if (code.containsNEW) code.resolveNEWs
  }

  /** TODO: move in Definitions and remove obsolete isBox/isUnbox found there. */
  def isBox(m: Symbol): Boolean =
    (m.owner == definitions.BoxesRunTimeClass.moduleClass
        && m.name.startsWith("boxTo"))

  def isUnbox(m: Symbol): Boolean =
    (m.owner == definitions.BoxesRunTimeClass.moduleClass
        && m.name.startsWith("unboxTo"))

  /** Return the icode class that should include members with the given flags.
   *  There are two possible classes, the static part and the instance part.
   */
  def getCode(flags: Int): IClass =
    if (isScalaModule) staticCode
    else if ((flags & JAVA_ACC_STATIC) != 0) staticCode
    else instanceCode

  class LinearCode {
    var instrs: ListBuffer[(Int, Instruction)] = new ListBuffer
    var jmpTargets: Set[Int] = new HashSet[Int]
    var locals: Map[Int, List[(Local, TypeKind)]] = new HashMap()

    var containsDUPX = false
    var containsNEW  = false

    def emit(i: Instruction) {
      instrs += ((pc, i))
      if (i.isInstanceOf[DupX])
        containsDUPX = true
      if (i.isInstanceOf[opcodes.NEW])
        containsNEW = true
    }

    /** Break this linear code in basic block representation
     *  As a side effect, it sets the 'code' field of the current
     */
    def toBasicBlock: Code = {
      import opcodes._

      val code = new Code(method.symbol.name.toString, method);
      method.setCode(code)
      var bb = code.startBlock

      def makeBasicBlocks: Map[Int, BasicBlock] = {
        val block: Map[Int, BasicBlock] = new HashMap
        for (pc <- jmpTargets) block += (pc -> code.newBlock)
        block
      }

      val blocks = makeBasicBlocks
      var otherBlock: BasicBlock = null
      var disableJmpTarget = false

      for ((pc, instr) <- instrs.iterator) {
//        Console.println("> " + pc + ": " + instr);
        if (jmpTargets contains pc) {
          otherBlock = blocks(pc)
          if (!bb.closed && otherBlock != bb) {
            bb.emit(JUMP(otherBlock))
            bb.close
//            Console.println("\t> closing bb: " + bb)
          }
          bb = otherBlock
//          Console.println("\t> entering bb: " + bb)
        }
        instr match {
          case LJUMP(target) =>
            otherBlock = blocks(target)
            bb.emit(JUMP(otherBlock))
            bb.close

          case LCJUMP(success, failure, cond, kind) =>
            otherBlock = blocks(success)
            val failBlock = blocks(failure)
            bb.emit(CJUMP(otherBlock, failBlock, cond, kind))
            bb.close

          case LCZJUMP(success, failure, cond, kind) =>
            otherBlock = blocks(success)
            val failBlock = blocks(failure)
            bb.emit(CZJUMP(otherBlock, failBlock, cond, kind))
            bb.close

          case LSWITCH(tags, targets) =>
            bb.emit(SWITCH(tags, targets map blocks))
            bb.close

          case RETURN(_) =>
            bb.emit(instr)
            bb.close

          case THROW() =>
            bb.emit(instr)
            bb.close

          case _ =>
            bb.emit(instr)
        }
      }

      code
    }

    def resolveDups {
      import opcodes._

      val tfa = new analysis.MethodTFA() {
        import analysis._
        import analysis.typeFlowLattice.IState

        /** Abstract interpretation for one instruction. */
        override def interpret(in: typeFlowLattice.Elem, i: Instruction): typeFlowLattice.Elem = {
          var out = IState(new VarBinding(in.vars), new TypeStack(in.stack));
          val bindings = out.vars;
          val stack = out.stack;
          import stack.push
          i match {
            case DUP_X1 =>
              val (one, two) = stack.pop2
              push(one); push(two); push(one);
              out = IState(bindings, stack)

            case DUP_X2 =>
              val (one, two, three) = stack.pop3
              push(one); push(three); push(two); push(one);
              out = IState(bindings, stack)

            case DUP2_X1 =>
              val (one, two) = stack.pop2
              if (one.isWideType) {
                push(one); push(two); push(one);
              } else {
                val three = stack.pop
                push(two); push(one); push(three); push(two); push(one);
              }
              out = IState(bindings, stack)

            case DUP2_X2 =>
              val (one, two) = stack.pop2
              if (one.isWideType && two.isWideType) {
                push(one); push(two); push(one);
              } else if (one.isWideType) {
                val three = stack.pop
                assert(!three.isWideType, "Impossible")
                push(one); push(three); push(two); push(one);
              } else {
                val three = stack.pop
                if (three.isWideType) {
                  push(two); push(one); push(one); push(three); push(two); push(one);
                } else {
                  val four = stack.pop
                  push(two); push(one); push(four); push(one); push(three); push(two); push(one);
                }
              }
              out = IState(bindings, stack)

            case _ =>
              out = super.interpret(in, i)
          }
          out
        }
      }

//      method.dump
      tfa.init(method)
      tfa.run
      for (bb <- linearizer.linearize(method)) {
        var info = tfa.in(bb)
        for (i <- bb.toList) {
          i match {
            case DUP_X1 =>
              val one = info.stack.types(0)
              val two = info.stack.types(1)
              assert(!one.isWideType, "DUP_X1 expects values of size 1 on top of stack " + info.stack);
              val tmp1 = freshLocal(one);
              val tmp2 = freshLocal(two);
              bb.replaceInstruction(i, List(STORE_LOCAL(tmp1),
                  STORE_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1),
                  LOAD_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1)));

            case DUP_X2 =>
              val one = info.stack.types(0)
              val two = info.stack.types(1)
              assert (!one.isWideType, "DUP_X2 expects values of size 1 on top of stack " + info.stack);
              val tmp1 = freshLocal(one);
              val tmp2 = freshLocal(two);
              if (two.isWideType)
                bb.replaceInstruction(i, List(STORE_LOCAL(tmp1),
                  STORE_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1),
                  LOAD_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1)));
              else {
                val tmp3 = freshLocal(info.stack.types(2));
                bb.replaceInstruction(i, List(STORE_LOCAL(tmp1),
                  STORE_LOCAL(tmp2),
                  STORE_LOCAL(tmp3),
                  LOAD_LOCAL(tmp1),
                  LOAD_LOCAL(tmp3),
                  LOAD_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1)));
              }

            case DUP2_X1 =>
              val one = info.stack.types(0)
              val two = info.stack.types(1)
              val tmp1 = freshLocal(one);
              val tmp2 = freshLocal(two);
              if (one.isWideType) {
                assert(!two.isWideType, "Impossible")
                bb.replaceInstruction(i, List(STORE_LOCAL(tmp1),
                  STORE_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1),
                  LOAD_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1)));
              } else {
                val tmp3 = freshLocal(info.stack.types(2));
                bb.replaceInstruction(i, List(STORE_LOCAL(tmp1),
                  STORE_LOCAL(tmp2),
                  STORE_LOCAL(tmp3),
                  LOAD_LOCAL(tmp1),
                  LOAD_LOCAL(tmp3),
                  LOAD_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1)));
              }

            case DUP2_X2 =>
              val one = info.stack.types(0)
              val two = info.stack.types(1)
              val tmp1 = freshLocal(one);
              val tmp2 = freshLocal(two);
              if (one.isWideType && two.isWideType) {
                bb.replaceInstruction(i, List(STORE_LOCAL(tmp1),
                  STORE_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1),
                  LOAD_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1)));
              } else if (one.isWideType) {
                val three = info.stack.types(2)
                assert(!two.isWideType && !three.isWideType, "Impossible")
                val tmp3 = freshLocal(three);
                bb.replaceInstruction(i, List(STORE_LOCAL(tmp1),
                  STORE_LOCAL(tmp2),
                  STORE_LOCAL(tmp3),
                  LOAD_LOCAL(tmp1),
                  LOAD_LOCAL(tmp3),
                  LOAD_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1)));
              } else {
                val three = info.stack.types(2)
                val tmp3 = freshLocal(three);
                if (three.isWideType) {
                  bb.replaceInstruction(i, List(STORE_LOCAL(tmp1),
                      STORE_LOCAL(tmp2),
                      STORE_LOCAL(tmp3),
                      LOAD_LOCAL(tmp2),
                      LOAD_LOCAL(tmp1),
                      LOAD_LOCAL(tmp3),
                      LOAD_LOCAL(tmp2),
                      LOAD_LOCAL(tmp1)));
                } else {
                  val four = info.stack.types(3)
                  val tmp4 = freshLocal(three);
                  assert(!four.isWideType, "Impossible")
                  bb.replaceInstruction(i, List(STORE_LOCAL(tmp1),
                      STORE_LOCAL(tmp2),
                      STORE_LOCAL(tmp3),
                      STORE_LOCAL(tmp4),
                      LOAD_LOCAL(tmp2),
                      LOAD_LOCAL(tmp1),
                      LOAD_LOCAL(tmp4),
                      LOAD_LOCAL(tmp3),
                      LOAD_LOCAL(tmp2),
                      LOAD_LOCAL(tmp1)));
                }
              }
            case _ =>
          }
          info = tfa.interpret(info, i)
        }
      }
    }

    /** Recover def-use chains for NEW and initializers. */
    def resolveNEWs {
      import opcodes._

      val rdef = new reachingDefinitions.ReachingDefinitionsAnalysis;
      rdef.init(method)
      rdef.run

      for (bb <- method.code.blocks) {
        var info = rdef.in(bb)
        for ((i, idx) <- bb.toList.zipWithIndex) i match {
          case CALL_METHOD(m, Static(true)) if m.isClassConstructor =>
            val defs = rdef.findDefs(bb, idx, 1, m.info.paramTypes.length)
            if (settings.debug.value) log("ctor: " + i + " found defs: " + defs)
            assert(defs.length == 1, "wrong defs at bb " + bb + "\n" + method.dump + rdef)
            val (bb1, idx1) = defs.head
            var producer = bb1(idx1)
            while (producer.isInstanceOf[DUP]) {
              val (bb2, idx2) = rdef.findDefs(bb1, idx1, 1).head
              producer = bb2(idx2)
            }
            producer match {
              case nw: NEW => nw.init = i.asInstanceOf[CALL_METHOD]
              case _: THIS => () // super constructor call
              case _ => assert(false, producer + "\n" + method.dump)
            }
          case _ =>
        }
      }

    }

    /** Return the local at given index, with the given type. */
    def getLocal(idx: Int, kind: TypeKind): Local = {
      assert(idx < maxLocals, "Index too large for local variable.");

      def checkValidIndex {
        locals.get(idx - 1) match {
          case Some(others) if ((others find { x => x._1 == LONG || x._1 == DOUBLE}) != None) =>
            error("Illegal index: " + idx + " points in the middle of another local")
          case _ => ()
        }
        kind match {
          case LONG | DOUBLE if (locals.isDefinedAt(idx + 1)) =>
            error("Illegal index: " + idx + " overlaps " + locals(idx + 1))
          case _ => ()
        }
      }

      locals.get(idx) match {
        case Some(ls) =>
          val l = ls find { loc => loc._2 <:< kind }
          l match {
            case Some((loc, _)) => loc
            case None =>
              val l = freshLocal(kind)
              locals(idx) = (l, kind) :: locals(idx)
              log("Expected kind " + kind + " for local " + idx +
                " but only " + ls + " found. Added new local.")
              l
          }
        case None =>
          checkValidIndex
          val l = freshLocal(idx, kind, false)
          log("Added new local for idx " + idx + ": " + kind)
          locals += (idx -> List((l, kind)))
          l
      }
    }

    override def toString(): String = instrs.toList.mkString("", "\n", "")

    /** Return a fresh Local variable for the given index.
     */
    private def freshLocal(idx: Int, kind: TypeKind, isArg: Boolean) = {
      val sym = method.symbol.newVariable(NoPosition, "loc" + idx).setInfo(kind.toType);
      val l = new Local(sym, kind, isArg)
      method.addLocal(l)
      l
    }

    private var count = 0

    /** Invent a new local, with a new index value outside the range of
     *  the original method. */
    def freshLocal(kind: TypeKind): Local = {
      count += 1
      freshLocal(maxLocals + count, kind, false)
    }

    /** add a method param with the given index. */
    def enterParam(idx: Int, kind: TypeKind) = {
      val sym = method.symbol.newVariable(NoPosition, "par" + idx).setInfo(kind.toType);
      val l = new Local(sym, kind, true)
      assert(!locals.isDefinedAt(idx))
      locals += (idx -> List((l, kind)))
      l
    }

    /** Base class for branch instructions that take addresses. */
    abstract class LazyJump(pc: Int) extends Instruction {
      override def toString() = "LazyJump " + pc
      jmpTargets += pc
    }

    case class LJUMP(pc: Int) extends LazyJump(pc);
    case class LCJUMP(success: Int, failure: Int, cond: TestOp, kind: TypeKind)
      extends LazyJump(success) {
      override def toString(): String ="LCJUMP (" + kind + ") " + success + " : " + failure;

      jmpTargets += failure
    }

    case class LCZJUMP(success: Int, failure: Int, cond: TestOp, kind: TypeKind)
      extends LazyJump(success) {
      override def toString(): String ="LCZJUMP (" + kind + ") " + success + " : " + failure;

      jmpTargets += failure
    }

    case class LSWITCH(tags: List[List[Int]], targets: List[Int]) extends LazyJump(targets.head) {
      override def toString(): String ="LSWITCH (tags: " + tags + ") targets: " + targets;

      targets.tail.foreach(t => jmpTargets += t)
    }

    /** Duplicate and exchange pseudo-instruction. Should be later
     *  replaced by proper ICode */
    abstract class DupX extends Instruction

    case object DUP_X1 extends DupX
    case object DUP_X2 extends DupX
    case object DUP2_X1 extends DupX
    case object DUP2_X2 extends DupX
  }
}
