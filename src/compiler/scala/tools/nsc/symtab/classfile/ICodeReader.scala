/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Iulian Dragos
 */
// $Id$

package scala.tools.nsc.symtab.classfile

import java.io.IOException

import scala.collection.mutable._
import scala.tools.nsc._
import scala.tools.nsc.backend.icode._
import scala.tools.nsc.io._
import scala.tools.nsc.util.Position

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
    isScalaModule = cls.isModule && !cls.hasFlag(JAVA)
    val name = cls.fullNameString(java.io.File.separatorChar) + (if (isScalaModule) "$" else "")
    classFile = classPath.root.find(name, false).classFile
    if (cls.isModule && !cls.hasFlag(JAVA))
      sym = cls.linkedClassOfModule
    assert(classFile ne null, "No classfile for " + cls)

//    for (val s <- cls.info.members)
//      Console.println("" + s + ": " + s.tpe)
    this.instanceCode = new IClass(sym)
    this.staticCode   = new IClass(sym.linkedClassOfClass)
    parse(classFile, sym)

    (staticCode, instanceCode)
  }

  /** If we're parsing a scala module, the owner of members is always
   *  the module symbol.
   */
  override def getOwner(jflags: Int): Symbol = {
    if (isScalaModule) this.staticModule else super.getOwner(jflags)
  }

  override def parseClass(): Unit = {
    val jflags = in.nextChar
    val isAttribute = (jflags & JAVA_ACC_ANNOTATION) != 0
    var sflags = transFlags(jflags)
    if ((sflags & DEFERRED) != 0) sflags = sflags & ~DEFERRED | ABSTRACT
    val c = pool.getClassSymbol(in.nextChar)
//    if (c != clazz)
//      throw new IOException("class file '" + in.file + "' contains " + c + "instead of " + clazz)

    in.skip(2)               // super class
    in.skip(2 * in.nextChar) // interfaces
    val fieldCount = in.nextChar
    for (val i <- 0 until fieldCount) parseField();
    val methodCount = in.nextChar
    for (val i <- 0 until methodCount) parseMethod();
    instanceCode.methods = instanceCode.methods.reverse
    staticCode.methods = staticCode.methods.reverse
  }

  override def parseField(): Unit = {
    val (jflags, sym) = parseMember()
    getCode(jflags).addField(new IField(sym))
    skipAttributes()
  }

  private def parseMember(): (Int, Symbol) = {
    val jflags = in.nextChar
    val name = pool.getName(in.nextChar)
    var tpe  = pool.getType(in.nextChar)
    if (name == nme.CONSTRUCTOR)
      tpe match {
        case MethodType(formals, restpe) =>
          assert(restpe.symbol == definitions.UnitClass)
          tpe = MethodType(formals, getOwner(jflags).tpe)
      }

    if ("<clinit>" == name.toString)
      (jflags, NoSymbol)
    else {
      var sym = getOwner(jflags).info.member(name).suchThat(old => old.tpe =:= tpe);
      if (sym == NoSymbol)
        sym = getOwner(jflags).info.member(newTermName(name.toString + nme.LOCAL_SUFFIX)).suchThat(old => old.tpe =:= tpe);
      if (sym == NoSymbol)
        Console.println("Could not find symbol for " + name + ": " + tpe);
      (jflags, sym)
    }
  }

  override def parseMethod(): Unit = {
    val (jflags, sym) = parseMember();
    if (sym != NoSymbol) {
      Console.println("Parsing method " + sym.fullNameString);
      this.method = new IMethod(sym);
      getCode(jflags).addMethod(this.method);
      if ((jflags & JAVA_ACC_NATIVE) != 0)
        this.method.native = true
      val attributeCount = in.nextChar;
      for (val i <- 0 until attributeCount)
        parseAttribute();
    } else {
      if (settings.debug.value) log("Skipping non-existent method.");
      skipAttributes();
    }
  }

  def parseAttribute(): Unit = {
    val attrName = pool.getName(in.nextChar)
    val attrLen = in.nextInt
    attrName match {
      case nme.CodeATTR =>
        parseByteCode()
      case _ =>
        in.skip(attrLen)
    }
  }

  override def classNameToSymbol(name: Name) =
    if (name == nothingName)
      definitions.AllClass
    else if (name == nullName)
      definitions.AllRefClass
    else if (name.endsWith("$"))
      definitions.getModule(name.subName(0, name.length - 1))
    else
      definitions.getClass(name)


  var maxStack: Int = _
  var maxLocals: Int = _
  val JVM = ClassfileConstants // shorter, uppercase alias for use in case patterns

  def toUnsignedByte(b: Byte): Int = b.toInt & 0xff
  var pc = 0

  /** Parse java bytecode into ICode */
  def parseByteCode(): Unit = {
    maxStack = in.nextChar
    maxLocals = in.nextChar
    val codeLength = in.nextInt
    val code = new LinearCode

    def parseInstruction: Unit = {
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
      def parseJumpTargetW = {
        size = size + 4
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

        case JVM.bipush      => code.emit(CONSTANT(Constant(in.nextByte))); size = size + 1;
        case JVM.sipush      => code.emit(CONSTANT(Constant(in.nextChar))); size = size + 2;
        case JVM.ldc         => code.emit(CONSTANT(pool.getConstant(in.nextByte))); size = size + 1;
        case JVM.ldc_w       => code.emit(CONSTANT(pool.getConstant(in.nextChar))); size = size + 2;
        case JVM.ldc2_w      => code.emit(CONSTANT(pool.getConstant(in.nextChar))); size = size + 2;
        case JVM.iload       => code.emit(LOAD_LOCAL(code.getLocal(in.nextByte, INT)));    size = size + 1;
        case JVM.lload       => code.emit(LOAD_LOCAL(code.getLocal(in.nextByte, LONG)));   size = size + 1;
        case JVM.fload       => code.emit(LOAD_LOCAL(code.getLocal(in.nextByte, FLOAT)));  size = size + 1;
        case JVM.dload       => code.emit(LOAD_LOCAL(code.getLocal(in.nextByte, DOUBLE))); size = size + 1;
        case JVM.aload       =>
          val local = in.nextByte; size = size + 1;
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

        case JVM.istore      => code.emit(STORE_LOCAL(code.getLocal(in.nextByte, INT)));    size = size + 1;
        case JVM.lstore      => code.emit(STORE_LOCAL(code.getLocal(in.nextByte, LONG)));   size = size + 1;
        case JVM.fstore      => code.emit(STORE_LOCAL(code.getLocal(in.nextByte, FLOAT)));  size = size + 1;
        case JVM.dstore      => code.emit(STORE_LOCAL(code.getLocal(in.nextByte, DOUBLE))); size = size + 1;
        case JVM.astore      => code.emit(STORE_LOCAL(code.getLocal(in.nextByte, OBJECT))); size = size + 1;
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

        case JVM.pop         => code.emit(DROP(INT)); // any 1-word type would do
        case JVM.pop2        => code.emit(DROP(LONG)); // any 2-word type would do
        case JVM.dup         => code.emit(DUP(OBJECT)); // TODO: Is the kind inside DUP ever needed?
        case JVM.dup_x1      => code.emit(DUP_X1);  // Predef.error("Unsupported JVM bytecode: dup_x1")
        case JVM.dup_x2      => code.emit(DUP_X2);  // Predef.error("Unsupported JVM bytecode: dup_x2")
        case JVM.dup2        => code.emit(DUP(LONG)); // TODO: Is the kind inside DUP ever needed?
        case JVM.dup2_x1     => code.emit(DUP2_X1);  // Predef.error("Unsupported JVM bytecode: dup2_x1")
        case JVM.dup2_x2     => code.emit(DUP2_X2);  // Predef.error("Unsupported JVM bytecode: dup2_x2")
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
          size = size + 2
          val local = code.getLocal(in.nextByte, INT)
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
          var byte1 = in.nextByte; size = size + 1;
          while (byte1 == 0) { byte1 = in.nextByte; size = size + 1; }
          val default = byte1 << 24 | in.nextByte << 16 | in.nextByte << 8 | in.nextByte;
          size = size + 3
          val low  = in.nextInt
          val high = in.nextInt
          size = size + 8
          assert(low <= high, "Value low not <= high for tableswitch.")

          val tags = List.tabulate(high - low + 1, n => List(low + n))
          val targets = for (val _ <- tags) yield parseJumpTargetW
          code.emit(LSWITCH(tags, targets ::: List(default)))

        case JVM.lookupswitch =>
          val padding = if ((pc + size) % 4 != 0) 4 - ((pc + size) % 4) else 0
          size = size + padding;
          in.bp = in.bp + padding
          assert((pc + size % 4) != 0)
          val default = pc + in.nextInt; size = size + 4
          val npairs = in.nextInt; size = size + 4
          var tags: List[List[Int]] = Nil
          var targets: List[Int] = Nil
          var i = 0
          while (i < npairs) {
            tags = List(in.nextInt) :: tags; size = size + 4;
            targets = parseJumpTargetW :: targets; // parseJumpTargetW updates 'size' itself
            i = i + 1;
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
          val field = pool.getMemberSymbol(in.nextChar, true); size = size + 2;
          if (field.hasFlag(Flags.MODULE))
            code.emit(LOAD_MODULE(field))
          else
            code.emit(LOAD_FIELD(field, true))
        case JVM.putstatic   =>
          val field = pool.getMemberSymbol(in.nextChar, true); size = size + 2;
          code.emit(STORE_FIELD(field, true))
        case JVM.getfield    =>
          val field = pool.getMemberSymbol(in.nextChar, false); size = size + 2;
          code.emit(LOAD_FIELD(field, false))
        case JVM.putfield    =>
          val field = pool.getMemberSymbol(in.nextChar, false); size = size + 2;
          code.emit(STORE_FIELD(field, false))

        case JVM.invokevirtual =>
          val m = pool.getMemberSymbol(in.nextChar, false); size = size + 2;
          code.emit(CALL_METHOD(m, Dynamic))
        case JVM.invokeinterface  =>
          val m = pool.getMemberSymbol(in.nextChar, false); size = size + 4;
          in.skip(2)
          code.emit(CALL_METHOD(m, Dynamic));
        case JVM.invokespecial   =>
          val m = pool.getMemberSymbol(in.nextChar, false); size = size + 2;
          val style = if (m.name == nme.CONSTRUCTOR || m.hasFlag(Flags.PRIVATE)) Static(true)
                      else SuperCall(m.owner.name);
          code.emit(CALL_METHOD(m, style))
        case JVM.invokestatic    =>
          val m = pool.getMemberSymbol(in.nextChar, true); size = size + 2;
          code.emit(CALL_METHOD(m, Static(false)))

        case JVM.new_          =>
          code.emit(NEW(REFERENCE(pool.getClassSymbol(in.nextChar))))
          size = size + 2
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
          size = size + 1
          code.emit(CREATE_ARRAY(kind))

        case JVM.anewarray     =>
          val tpe = REFERENCE(pool.getClassSymbol(in.nextChar)); size = size + 2;
          code.emit(CREATE_ARRAY(tpe))

        case JVM.arraylength   => code.emit(CALL_PRIMITIVE(ArrayLength(OBJECT))); // the kind does not matter
        case JVM.athrow        => code.emit(THROW());
        case JVM.checkcast     => code.emit(CHECK_CAST(toTypeKind(pool.getClassOrArrayType(in.nextChar)))); size = size + 2;
        case JVM.instanceof    => code.emit(IS_INSTANCE(toTypeKind(pool.getClassOrArrayType(in.nextChar)))); size = size + 2;
        case JVM.monitorenter  => code.emit(MONITOR_ENTER());
        case JVM.monitorexit   => code.emit(MONITOR_EXIT());
        case JVM.wide          =>
          size = size + 1;
          toUnsignedByte(in.nextByte) match {
            case JVM.iload  => code.emit(LOAD_LOCAL(code.getLocal(in.nextChar, INT)));    size = size + 2;
            case JVM.lload  => code.emit(LOAD_LOCAL(code.getLocal(in.nextChar, LONG)));   size = size + 2;
            case JVM.fload  => code.emit(LOAD_LOCAL(code.getLocal(in.nextChar, FLOAT)));  size = size + 2;
            case JVM.dload  => code.emit(LOAD_LOCAL(code.getLocal(in.nextChar, DOUBLE))); size = size + 2;
            case JVM.aload  => code.emit(LOAD_LOCAL(code.getLocal(in.nextChar, OBJECT))); size = size + 2;
            case JVM.istore => code.emit(STORE_LOCAL(code.getLocal(in.nextChar, INT)));    size = size + 2;
            case JVM.lstore => code.emit(STORE_LOCAL(code.getLocal(in.nextChar, LONG)));   size = size + 2;
            case JVM.fstore => code.emit(STORE_LOCAL(code.getLocal(in.nextChar, FLOAT)));  size = size + 2;
            case JVM.dstore => code.emit(STORE_LOCAL(code.getLocal(in.nextChar, DOUBLE))); size = size + 2;
            case JVM.astore => code.emit(STORE_LOCAL(code.getLocal(in.nextChar, OBJECT))); size = size + 2;
            case JVM.ret => Predef.error("Cannot handle jsr/ret")
            case JVM.iinc =>
              size = size + 4
              val local = code.getLocal(in.nextChar, INT)
              code.emit(CONSTANT(Constant(in.nextChar)))
              code.emit(CALL_PRIMITIVE(Arithmetic(ADD, INT)))
              code.emit(STORE_LOCAL(local))
            case _ => Predef.error("Invalid 'wide' operand")
          }

        case JVM.multianewarray =>
          size = size + 3
          val tpe = toTypeKind(pool.getClassOrArrayType(in.nextChar))
          val dim = in.nextByte
          assert(dim == 1, "Cannot handle multidimensional arrays yet.")
          code.emit(CREATE_ARRAY(tpe))

        case JVM.ifnull    => code.emit(LCZJUMP(parseJumpTarget, pc + size, EQ, OBJECT))
        case JVM.ifnonnull => code.emit(LCZJUMP(parseJumpTarget, pc + size, NE, OBJECT))
        case JVM.goto_w    => code.emit(LJUMP(parseJumpTargetW))
        case JVM.jsr_w     => Predef.error("Cannot handle jsr/ret")

//        case _ => Predef.error("Unknown bytecode")
      }
      pc = pc + size
    }

    // add parameters
    var idx = if (method.isStatic) 0 else 1
    for (val t <- method.symbol.tpe.paramTypes) {
      this.method.addParam(code.freshLocal(idx, toTypeKind(t), true))
      idx = idx + 1
    }

    pc = 0
    while (pc < codeLength) {
      parseInstruction
    }

    val exceptionEntries = in.nextChar.toInt
    var i = 0
    while (i < exceptionEntries) {
      // skip start end PC
      in.skip(4)
      // read the handler PC
      code.jmpTargets += in.nextChar
      // skip the exception type
      in.skip(2)
      i = i + 1
    }
    skipAttributes()

    code.toBasicBlock
    assert(method.code ne null)
    if (code.containsDUPX) {

      code.resolveDups
    }
  }

  /** Return the icode class that should include members with the given flags.
   *  There are two possible classes, the static part and the instance part.
   */
  def getCode(flags: Int): IClass =
    if ((flags & JAVA_ACC_STATIC) != 0) staticCode else instanceCode

  class LinearCode {
    var instrs: ListBuffer[(Int, Instruction)] = new ListBuffer
    var jmpTargets: Set[Int] = new HashSet[Int]
    var locals: Map[Int, List[(Local, TypeKind)]] = new HashMap()

    var containsDUPX = false

    def emit(i: Instruction) = {
//      Console.println(i);
      instrs += (pc, i)
      if (i.isInstanceOf[DupX])
        containsDUPX = true
    }

    /** Break this linear code in basic block representation
     *  As a side effect, it sets the 'code' field of the current
     */
    def toBasicBlock: Code = {
      import opcodes._

      val code = new Code(method.symbol.name.toString);
      method.setCode(code)
      var bb = code.startBlock

      def makeBasicBlocks: Map[Int, BasicBlock] = {
        val block: Map[Int, BasicBlock] = new HashMap;
        for (val pc <- jmpTargets) block += pc -> code.newBlock
        block
      }

      val blocks = makeBasicBlocks
      var otherBlock: BasicBlock = null
      var disableJmpTarget = false

      for (val (pc, instr) <- instrs.elements) {
//        Console.println("> " + pc + ": " + instr);
        if (jmpTargets contains pc) {
          otherBlock = blocks(pc)
          if (!bb.isClosed && otherBlock != bb) {
            bb.emit(JUMP(otherBlock))
            bb.close
          }
          bb = otherBlock
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

          case _ =>
            bb.emit(instr)
        }
      }

      code
    }

    def resolveDups: Unit = {
      import opcodes._

      val tfa = new analysis.MethodTFA() {
        import analysis._
        /** Abstract interpretation for one instruction. */
        override def interpret(in: typeFlowLattice.Elem, i: Instruction): typeFlowLattice.Elem = {
          var out = (new VarBinding(in._1), new TypeStack(in._2));
          val bindings = out._1;
          val stack = out._2;
          import stack.push
          i match {
            case DUP_X1 =>
              val (one, two) = stack.pop2
              push(one); push(two); push(one);

            case DUP_X2 =>
              val (one, two, three) = stack.pop3
              push(one); push(three); push(two); push(one);

            case DUP2_X1 =>
              val (one, two) = stack.pop2
              if (one.isWideType) {
                push(one); push(two); push(one);
              } else {
                val three = stack.pop
                push(two); push(one); push(three); push(two); push(one);
              }

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

            case _ =>
              out = super.interpret(in, i)
          }
          out
        }
      }

      method.dump
      tfa.init(method)
      tfa.run
      for (val bb <- linearizer.linearize(method)) {
        var info = tfa.in(bb);
        for (val i <- bb.toList) {
          i match {
            case DUP_X1 =>
              val one = info._2.types(0)
              val two = info._2.types(1)
              assert(!one.isWideType, "DUP_X1 expects values of size 1 on top of stack " + info._2);
              val tmp1 = freshLocal(one);
              val tmp2 = freshLocal(two);
              bb.replaceInstruction(i, List(STORE_LOCAL(tmp1),
                  STORE_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1),
                  LOAD_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1)));

            case DUP_X2 =>
              val one = info._2.types(0)
              val two = info._2.types(1)
              assert (!one.isWideType, "DUP_X2 expects values of size 1 on top of stack " + info._2);
              val tmp1 = freshLocal(one);
              val tmp2 = freshLocal(two);
              if (two.isWideType)
                bb.replaceInstruction(i, List(STORE_LOCAL(tmp1),
                  STORE_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1),
                  LOAD_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1)));
              else {
                val tmp3 = freshLocal(info._2.types(2));
                bb.replaceInstruction(i, List(STORE_LOCAL(tmp1),
                  STORE_LOCAL(tmp2),
                  STORE_LOCAL(tmp3),
                  LOAD_LOCAL(tmp1),
                  LOAD_LOCAL(tmp3),
                  LOAD_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1)));
              }

            case DUP2_X1 =>
              val one = info._2.types(0)
              val two = info._2.types(1)
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
                val tmp3 = freshLocal(info._2.types(2));
                bb.replaceInstruction(i, List(STORE_LOCAL(tmp1),
                  STORE_LOCAL(tmp2),
                  STORE_LOCAL(tmp3),
                  LOAD_LOCAL(tmp1),
                  LOAD_LOCAL(tmp3),
                  LOAD_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1)));
              }

            case DUP2_X2 =>
              val one = info._2.types(0)
              val two = info._2.types(1)
              val tmp1 = freshLocal(one);
              val tmp2 = freshLocal(two);
              if (one.isWideType && two.isWideType) {
                bb.replaceInstruction(i, List(STORE_LOCAL(tmp1),
                  STORE_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1),
                  LOAD_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1)));
              } else if (one.isWideType) {
                val three = info._2.types(2)
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
                val three = info._2.types(2)
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
                  val four = info._2.types(3)
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

    /** Return the local at given index, with the given type. */
    def getLocal(idx: Int, kind: TypeKind): Local = {
      assert(idx < maxLocals, "Index too large for local variable.");

      def checkValidIndex: Unit = {
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
          val l = ls find { loc => loc._2 == kind }
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
          locals += idx -> List((l, kind))
          l
      }
    }

    override def toString(): String = instrs.toList.mkString("", "\n", "")

    /** Return a fresh Local variable for the given index.
     */
    def freshLocal(idx: Int, kind: TypeKind, isArg: Boolean) = {
      val sym = method.symbol.newVariable(NoPos, "loc" + idx).setInfo(kind.toType);
      val l = new Local(sym, kind, isArg)
      method.addLocal(l)
      l
    }

    var count = 0;

    /** Invent a new local, with a new index value outside the range of
     *  the original method. */
    def freshLocal(kind: TypeKind): Local = {
      count = count + 1
      freshLocal(maxLocals + count, kind, false)
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
    abstract class DupX extends Instruction;

    case object DUP_X1 extends DupX;
    case object DUP_X2 extends DupX;
    case object DUP2_X1 extends DupX;
    case object DUP2_X2 extends DupX;
  }
}
