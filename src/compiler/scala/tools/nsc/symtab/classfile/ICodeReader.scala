/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Iulian Dragos
 */

package scala
package tools.nsc
package symtab
package classfile

import scala.collection.{ mutable, immutable }
import mutable.ListBuffer
import ClassfileConstants._
import scala.reflect.internal.JavaAccFlags

/** ICode reader from Java bytecode.
 *
 *  @author Iulian Dragos
 *  @version 1.0
 */
abstract class ICodeReader extends ClassfileParser {
  val global: Global
  val symbolTable: global.type
  val loaders: global.loaders.type
  import global._
  import icodes._

  var instanceCode: IClass = null          // the ICode class for the current symbol
  var staticCode:   IClass = null          // the ICode class static members
  var method: IMethod = NoIMethod          // the current IMethod
  var isScalaModule = false

  override protected type ThisConstantPool = ICodeConstantPool
  override protected def newConstantPool = new ICodeConstantPool

  /** Try to force the chain of enclosing classes for the given name. Otherwise
   *  flatten would not lift classes that were not referenced in the source code.
   */
  def forceMangledName(name: Name, module: Boolean): Symbol = {
    val parts = name.decode.toString.split(Array('.', '$'))
    var sym: Symbol = rootMirror.RootClass

    // was "at flatten.prev"
    enteringFlatten {
      for (part0 <- parts; if !(part0 == ""); part = newTermName(part0)) {
        val sym1 = enteringIcode {
          sym.linkedClassOfClass.info
          sym.info.decl(part.encode)
        }//.suchThat(module == _.isModule)

        sym = sym1 orElse sym.info.decl(part.encode.toTypeName)
      }
    }
    sym
  }

  protected class ICodeConstantPool extends ConstantPool {
    /** Return the symbol of the class member at `index`.
     *  The following special cases exist:
     *   - If the member refers to special `MODULE$` static field, return
     *  the symbol of the corresponding module.
     *   - If the member is a field, and is not found with the given name,
     *     another try is made by appending `nme.LOCAL_SUFFIX_STRING`
     *   - If no symbol is found in the right tpe, a new try is made in the
     *     companion class, in case the owner is an implementation class.
     */
    def getMemberSymbol(index: Int, static: Boolean): Symbol = {
      if (index <= 0 || len <= index) errorBadIndex(index)
      var f = values(index).asInstanceOf[Symbol]
      if (f eq null) {
        val start = starts(index)
        val first = in.buf(start).toInt
        if (first != CONSTANT_FIELDREF &&
            first != CONSTANT_METHODREF &&
            first != CONSTANT_INTFMETHODREF) errorBadTag(start)
        val ownerTpe = getClassOrArrayType(in.getChar(start + 1).toInt)
        debuglog("getMemberSymbol(static: " + static + "): owner type: " + ownerTpe + " " + ownerTpe.typeSymbol.unexpandedName)
        val (name0, tpe0) = getNameAndType(in.getChar(start + 3).toInt, ownerTpe)
        debuglog("getMemberSymbol: name and tpe: " + name0 + ": " + tpe0)

        forceMangledName(tpe0.typeSymbol.name, module = false)
        val (name, tpe) = getNameAndType(in.getChar(start + 3).toInt, ownerTpe)
        if (name == nme.MODULE_INSTANCE_FIELD) {
          val index = in.getChar(start + 1).toInt
          val name = getExternalName(in.getChar(starts(index).toInt + 1).toInt)
          //assert(name.endsWith("$"), "Not a module class: " + name)
          f = forceMangledName(name dropRight 1, module = true)
          if (f == NoSymbol)
            f = rootMirror.getModuleByName(name dropRight 1)
        } else {
          val origName = nme.unexpandedName(name)
          val owner = if (static) ownerTpe.typeSymbol.linkedClassOfClass else ownerTpe.typeSymbol
          f = owner.info.findMember(origName, 0, 0, stableOnly = false).suchThat(_.tpe.widen =:= tpe)
          if (f == NoSymbol)
            f = owner.info.findMember(newTermName(origName + nme.LOCAL_SUFFIX_STRING), 0, 0, stableOnly = false).suchThat(_.tpe =:= tpe)
          if (f == NoSymbol) {
            // if it's an impl class, try to find it's static member inside the class
            if (ownerTpe.typeSymbol.isImplClass) {
              f = ownerTpe.findMember(origName, 0, 0, stableOnly = false).suchThat(_.tpe =:= tpe)
            } else {
              log("Couldn't find " + name + ": " + tpe + " inside: \n" + ownerTpe)
              f = tpe match {
                case MethodType(_, _) => owner.newMethod(name.toTermName, owner.pos)
                case _                => owner.newVariable(name.toTermName, owner.pos)
              }
              f setInfo tpe
              log("created fake member " + f.fullName)
            }
          }
        }
        assert(f != NoSymbol,
          s"could not find $name: $tpe in $ownerTpe" + (
            if (settings.debug.value) ownerTpe.members.mkString(", members are:\n  ", "\n  ", "") else ""
          )
        )
        values(index) = f
      }
      f
    }
  }

  /** Read back bytecode for the given class symbol. It returns
   *  two IClass objects, one for static members and one
   *  for non-static members.
   */
  def readClass(cls: Symbol): (IClass, IClass) = {
    cls.info // ensure accurate type information

    isScalaModule = cls.isModule && !cls.isJavaDefined
    log("ICodeReader reading " + cls)
    val name = cls.javaClassName

    classFileLookup.findClassFile(name) match {
      case Some(classFile) => parse(classFile, cls)
      case _               => MissingRequirementError.notFound("Could not find bytecode for " + cls)
    }

    (staticCode, instanceCode)
  }

  override def parseClass() {
    this.instanceCode = new IClass(clazz)
    this.staticCode   = new IClass(staticModule)

    u2
    pool getClassSymbol u2
    parseInnerClasses()

    in.skip(2)               // super class
    in.skip(2 * u2) // interfaces
    val fieldCount = u2
    for (i <- 0 until fieldCount) parseField()
    val methodCount = u2
    for (i <- 0 until methodCount) parseMethod()
    instanceCode.methods = instanceCode.methods.reverse
    staticCode.methods = staticCode.methods.reverse
  }

  override def parseField() {
    val (jflags, sym) = parseMember(field = true)
    getCode(jflags) addField new IField(sym)
    skipAttributes()
  }

  private def parseMember(field: Boolean): (JavaAccFlags, Symbol) = {
    val jflags   = JavaAccFlags(u2)
    val name     = pool getName u2
    /*  If we're parsing a scala module, the owner of members is always
     *  the module symbol.
     */
    val owner = (
      if (isScalaModule) staticModule
      else if (jflags.isStatic) moduleClass
      else clazz
    )
    val dummySym = owner.newMethod(name.toTermName, owner.pos, jflags.toScalaFlags)

    try {
      val ch  = u2
      val tpe = pool.getType(dummySym, ch)

      if ("<clinit>" == name.toString)
        (jflags, NoSymbol)
      else {
        var sym = owner.info.findMember(name, 0, 0, stableOnly = false).suchThat(old => sameType(old.tpe, tpe))
        if (sym == NoSymbol)
          sym = owner.info.findMember(newTermName(name + nme.LOCAL_SUFFIX_STRING), 0, 0, stableOnly = false).suchThat(_.tpe =:= tpe)
        if (sym == NoSymbol) {
          sym = if (field) owner.newValue(name.toTermName, owner.pos, jflags.toScalaFlags) else dummySym
          sym setInfoAndEnter tpe
          log(s"ICodeReader could not locate ${name.decode} in $owner.  Created ${sym.defString}.")
        }
        (jflags, sym)
      }
    } catch {
      case e: MissingRequirementError =>
        (jflags, NoSymbol)
    }
  }

  /** Checks if `tp1` is the same type as `tp2`, modulo implicit methods.
   *  We don't care about the distinction between implicit and explicit
   *  methods as this point, and we can't get back the information from
   *  bytecode anyway.
   */
  private def sameType(tp1: Type, tp2: Type): Boolean = (tp1, tp2) match {
    case (mt1 @ MethodType(args1, resTpe1), mt2 @ MethodType(args2, resTpe2)) if mt1.isImplicit || mt2.isImplicit =>
      MethodType(args1, resTpe1) =:= MethodType(args2, resTpe2)
    case _ =>
      tp1 =:= tp2
  }

  override def parseMethod() {
    val (jflags, sym) = parseMember(field = false)
    val beginning = in.bp
    try {
      if (sym != NoSymbol) {
        this.method = new IMethod(sym)
        this.method.returnType = toTypeKind(sym.tpe.resultType)
        getCode(jflags).addMethod(this.method)
        if (jflags.isNative)
          this.method.native = true
        val attributeCount = u2
        for (i <- 0 until attributeCount) parseAttribute()
      } else {
        debuglog("Skipping non-existent method.")
        skipAttributes()
      }
    } catch {
      case e: MissingRequirementError =>
        in.bp = beginning; skipAttributes()
        debuglog("Skipping non-existent method. " + e.msg)
    }
  }

  def parseAttribute() {
    val attrName = pool.getName(u2).toTypeName
    val attrLen = u4
    attrName match {
      case tpnme.CodeATTR =>
        parseByteCode()
      case _ =>
        in.skip(attrLen)
    }
  }

  override def classNameToSymbol(name: Name) = {
    val sym = if (name == fulltpnme.RuntimeNothing)
      definitions.NothingClass
    else if (name == fulltpnme.RuntimeNull)
      definitions.NullClass
    else if (nme.isImplClassName(name)) {
      val iface = rootMirror.getClassByName(tpnme.interfaceName(name))
      log("forcing " + iface.owner + " at phase: " + phase + " impl: " + iface.implClass)
      iface.owner.info // force the mixin type-transformer
      rootMirror.getClassByName(name)
    }
    else if (nme.isModuleName(name)) {
      val strippedName = name.dropModule
      forceMangledName(newTermName(strippedName.decode), module = true) orElse rootMirror.getModuleByName(strippedName)
    }
    else {
      forceMangledName(name, module = false)
      exitingFlatten(rootMirror.getClassByName(name.toTypeName))
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
    maxStack = u2
    maxLocals = u2
    val codeLength = u4
    val code = new LinearCode

    def parseInstruction() {
      import opcodes._
      import code._
      var size = 1 // instruction size

      /* Parse 16 bit jump target. */
      def parseJumpTarget = {
        size += 2
        val offset = u2.toShort
        val target = pc + offset
        assert(target >= 0 && target < codeLength, "Illegal jump target: " + target)
        target
      }

      /* Parse 32 bit jump target. */
      def parseJumpTargetW: Int = {
        size += 4
        val offset = u4
        val target = pc + offset
        assert(target >= 0 && target < codeLength, "Illegal jump target: " + target + "pc: " + pc + " offset: " + offset)
        target
      }

      u1 match {
        case JVM.nop => parseInstruction()
        case JVM.aconst_null => code emit CONSTANT(Constant(null))
        case JVM.iconst_m1   => code emit CONSTANT(Constant(-1))
        case JVM.iconst_0    => code emit CONSTANT(Constant(0))
        case JVM.iconst_1    => code emit CONSTANT(Constant(1))
        case JVM.iconst_2    => code emit CONSTANT(Constant(2))
        case JVM.iconst_3    => code emit CONSTANT(Constant(3))
        case JVM.iconst_4    => code emit CONSTANT(Constant(4))
        case JVM.iconst_5    => code emit CONSTANT(Constant(5))

        case JVM.lconst_0    => code emit CONSTANT(Constant(0l))
        case JVM.lconst_1    => code emit CONSTANT(Constant(1l))
        case JVM.fconst_0    => code emit CONSTANT(Constant(0.0f))
        case JVM.fconst_1    => code emit CONSTANT(Constant(1.0f))
        case JVM.fconst_2    => code emit CONSTANT(Constant(2.0f))
        case JVM.dconst_0    => code emit CONSTANT(Constant(0.0))
        case JVM.dconst_1    => code emit CONSTANT(Constant(1.0))

        case JVM.bipush      => code.emit(CONSTANT(Constant(s1))); size += 1
        case JVM.sipush      => code.emit(CONSTANT(Constant(s2))); size += 2
        case JVM.ldc         => code.emit(CONSTANT(pool.getConstant(u1))); size += 1
        case JVM.ldc_w       => code.emit(CONSTANT(pool.getConstant(u2))); size += 2
        case JVM.ldc2_w      => code.emit(CONSTANT(pool.getConstant(u2))); size += 2
        case JVM.iload       => code.emit(LOAD_LOCAL(code.getLocal(u1, INT)));    size += 1
        case JVM.lload       => code.emit(LOAD_LOCAL(code.getLocal(u1, LONG)));   size += 1
        case JVM.fload       => code.emit(LOAD_LOCAL(code.getLocal(u1, FLOAT)));  size += 1
        case JVM.dload       => code.emit(LOAD_LOCAL(code.getLocal(u1, DOUBLE))); size += 1
        case JVM.aload       =>
          val local = u1.toInt; size += 1
          if (local == 0 && !method.isStatic)
            code.emit(THIS(method.symbol.owner))
          else
            code.emit(LOAD_LOCAL(code.getLocal(local, ObjectReference)))

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
            code.emit(THIS(method.symbol.owner))
          else
            code.emit(LOAD_LOCAL(code.getLocal(0, ObjectReference)))
        case JVM.aload_1     => code.emit(LOAD_LOCAL(code.getLocal(1, ObjectReference)))
        case JVM.aload_2     => code.emit(LOAD_LOCAL(code.getLocal(2, ObjectReference)))
        case JVM.aload_3     => code.emit(LOAD_LOCAL(code.getLocal(3, ObjectReference)))

        case JVM.iaload      => code.emit(LOAD_ARRAY_ITEM(INT))
        case JVM.laload      => code.emit(LOAD_ARRAY_ITEM(LONG))
        case JVM.faload      => code.emit(LOAD_ARRAY_ITEM(FLOAT))
        case JVM.daload      => code.emit(LOAD_ARRAY_ITEM(DOUBLE))
        case JVM.aaload      => code.emit(LOAD_ARRAY_ITEM(ObjectReference))
        case JVM.baload      => code.emit(LOAD_ARRAY_ITEM(BYTE))
        case JVM.caload      => code.emit(LOAD_ARRAY_ITEM(CHAR))
        case JVM.saload      => code.emit(LOAD_ARRAY_ITEM(SHORT))

        case JVM.istore      => code.emit(STORE_LOCAL(code.getLocal(u1, INT)));    size += 1
        case JVM.lstore      => code.emit(STORE_LOCAL(code.getLocal(u1, LONG)));   size += 1
        case JVM.fstore      => code.emit(STORE_LOCAL(code.getLocal(u1, FLOAT)));  size += 1
        case JVM.dstore      => code.emit(STORE_LOCAL(code.getLocal(u1, DOUBLE))); size += 1
        case JVM.astore      => code.emit(STORE_LOCAL(code.getLocal(u1, ObjectReference))); size += 1
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
        case JVM.astore_0    =>
          if (method.isStatic)
            code.emit(STORE_LOCAL(code.getLocal(0, ObjectReference)))
          else
            code.emit(STORE_THIS(ObjectReference))
        case JVM.astore_1    => code.emit(STORE_LOCAL(code.getLocal(1, ObjectReference)))
        case JVM.astore_2    => code.emit(STORE_LOCAL(code.getLocal(2, ObjectReference)))
        case JVM.astore_3    => code.emit(STORE_LOCAL(code.getLocal(3, ObjectReference)))
        case JVM.iastore     => code.emit(STORE_ARRAY_ITEM(INT))
        case JVM.lastore     => code.emit(STORE_ARRAY_ITEM(LONG))
        case JVM.fastore     => code.emit(STORE_ARRAY_ITEM(FLOAT))
        case JVM.dastore     => code.emit(STORE_ARRAY_ITEM(DOUBLE))
        case JVM.aastore     => code.emit(STORE_ARRAY_ITEM(ObjectReference))
        case JVM.bastore     => code.emit(STORE_ARRAY_ITEM(BYTE))
        case JVM.castore     => code.emit(STORE_ARRAY_ITEM(CHAR))
        case JVM.sastore     => code.emit(STORE_ARRAY_ITEM(SHORT))

        case JVM.pop         => code.emit(DROP(INT))   // any 1-word type would do
        case JVM.pop2        => code.emit(DROP(LONG))  // any 2-word type would do
        case JVM.dup         => code.emit(DUP(ObjectReference)) // TODO: Is the kind inside DUP ever needed?
        case JVM.dup_x1      => code.emit(DUP_X1)      // sys.error("Unsupported JVM bytecode: dup_x1")
        case JVM.dup_x2      => code.emit(DUP_X2)      // sys.error("Unsupported JVM bytecode: dup_x2")
        case JVM.dup2        => code.emit(DUP(LONG))   // TODO: Is the kind inside DUP ever needed?
        case JVM.dup2_x1     => code.emit(DUP2_X1)     // sys.error("Unsupported JVM bytecode: dup2_x1")
        case JVM.dup2_x2     => code.emit(DUP2_X2)     // sys.error("Unsupported JVM bytecode: dup2_x2")
        case JVM.swap        => sys.error("Unsupported JVM bytecode: swap")

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
        case JVM.ishr        => code.emit(CALL_PRIMITIVE(Shift(ASR, INT)))
        case JVM.lshr        => code.emit(CALL_PRIMITIVE(Shift(ASR, LONG)))
        case JVM.iushr       => code.emit(CALL_PRIMITIVE(Shift(LSR, INT)))
        case JVM.lushr       => code.emit(CALL_PRIMITIVE(Shift(LSR, LONG)))
        case JVM.iand        => code.emit(CALL_PRIMITIVE(Logical(AND, INT)))
        case JVM.land        => code.emit(CALL_PRIMITIVE(Logical(AND, LONG)))
        case JVM.ior         => code.emit(CALL_PRIMITIVE(Logical(OR, INT)))
        case JVM.lor         => code.emit(CALL_PRIMITIVE(Logical(OR, LONG)))
        case JVM.ixor        => code.emit(CALL_PRIMITIVE(Logical(XOR, INT)))
        case JVM.lxor        => code.emit(CALL_PRIMITIVE(Logical(XOR, LONG)))
        case JVM.iinc        =>
          size += 2
          val local = code.getLocal(u1, INT)
          code.emit(LOAD_LOCAL(local))
          code.emit(CONSTANT(Constant(s1)))
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
        case JVM.if_acmpeq   => code.emit(LCJUMP(parseJumpTarget, pc + size, EQ, ObjectReference))
        case JVM.if_acmpne   => code.emit(LCJUMP(parseJumpTarget, pc + size, NE, ObjectReference))

        case JVM.goto        => emit(LJUMP(parseJumpTarget))
        case JVM.jsr         => sys.error("Cannot handle jsr/ret")
        case JVM.ret         => sys.error("Cannot handle jsr/ret")
        case JVM.tableswitch =>
          val padding = if ((pc + size) % 4 != 0) 4 - ((pc + size) % 4) else 0
          size += padding
          in.bp += padding
          assert((pc + size % 4) != 0, pc)
/*          var byte1 = u1; size += 1;
          while (byte1 == 0) { byte1 = u1; size += 1; }
          val default = byte1 << 24 | u1 << 16 | u1 << 8 | u1;
          size = size + 3
       */
          val default = pc + u4; size += 4
          val low  = u4
          val high = u4
          size += 8
          assert(low <= high, "Value low not <= high for tableswitch.")

          val tags = List.tabulate(high - low + 1)(n => List(low + n))
          val targets = for (_ <- tags) yield parseJumpTargetW
          code.emit(LSWITCH(tags, targets ::: List(default)))

        case JVM.lookupswitch =>
          val padding = if ((pc + size) % 4 != 0) 4 - ((pc + size) % 4) else 0
          size += padding
          in.bp += padding
          assert((pc + size % 4) != 0, pc)
          val default = pc + u4; size += 4
          val npairs = u4; size += 4
          var tags: List[List[Int]] = Nil
          var targets: List[Int] = Nil
          var i = 0
          while (i < npairs) {
            tags = List(u4) :: tags; size += 4
            targets = parseJumpTargetW :: targets; // parseJumpTargetW updates 'size' itself
            i += 1
          }
          targets = default :: targets
          code.emit(LSWITCH(tags.reverse, targets.reverse))

        case JVM.ireturn     => code.emit(RETURN(INT))
        case JVM.lreturn     => code.emit(RETURN(LONG))
        case JVM.freturn     => code.emit(RETURN(FLOAT))
        case JVM.dreturn     => code.emit(RETURN(DOUBLE))
        case JVM.areturn     => code.emit(RETURN(ObjectReference))
        case JVM.return_     => code.emit(RETURN(UNIT))

        case JVM.getstatic    =>
          val field = pool.getMemberSymbol(u2, static = true); size += 2
          if (field.hasModuleFlag)
            code emit LOAD_MODULE(field)
          else
            code emit LOAD_FIELD(field, isStatic = true)
        case JVM.putstatic   =>
          val field = pool.getMemberSymbol(u2, static = true); size += 2
          code.emit(STORE_FIELD(field, isStatic = true))
        case JVM.getfield    =>
          val field = pool.getMemberSymbol(u2, static = false); size += 2
          code.emit(LOAD_FIELD(field, isStatic = false))
        case JVM.putfield    =>
          val field = pool.getMemberSymbol(u2, static = false); size += 2
          code.emit(STORE_FIELD(field, isStatic = false))

        case JVM.invokevirtual =>
          val m = pool.getMemberSymbol(u2, static = false); size += 2
          code.emit(CALL_METHOD(m, Dynamic))
          method.updateRecursive(m)
        case JVM.invokeinterface  =>
          val m = pool.getMemberSymbol(u2, static = false); size += 4
          in.skip(2)
          code.emit(CALL_METHOD(m, Dynamic))
          // invokeinterface can't be recursive
        case JVM.invokespecial   =>
          val m = pool.getMemberSymbol(u2, static = false); size += 2
          val style = if (m.name == nme.CONSTRUCTOR || m.isPrivate) Static(onInstance = true)
                      else SuperCall(m.owner.name)
          code.emit(CALL_METHOD(m, style))
          method.updateRecursive(m)
        case JVM.invokestatic    =>
          val m = pool.getMemberSymbol(u2, static = true); size += 2
          if (isBox(m))
            code.emit(BOX(toTypeKind(m.info.paramTypes.head)))
          else if (isUnbox(m))
            code.emit(UNBOX(toTypeKind(m.info.resultType)))
          else {
            code.emit(CALL_METHOD(m, Static(onInstance = false)))
            method.updateRecursive(m)
          }
        case JVM.invokedynamic  =>
          // TODO, this is just a place holder. A real implementation must parse the class constant entry
          debuglog("Found JVM invokedynamic instruction, inserting place holder ICode INVOKE_DYNAMIC.")
          containsInvokeDynamic = true
          val poolEntry = in.nextChar.toInt
          in.skip(2)
          code.emit(INVOKE_DYNAMIC(poolEntry))

        case JVM.new_          =>
          code.emit(NEW(REFERENCE(pool.getClassSymbol(u2))))
          size += 2
        case JVM.newarray      =>
          val kind = u1 match {
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
          val tpe = pool.getClassOrArrayType(u2); size += 2
          code.emit(CREATE_ARRAY(toTypeKind(tpe), 1))

        case JVM.arraylength   => code.emit(CALL_PRIMITIVE(ArrayLength(ObjectReference))); // the kind does not matter
        case JVM.athrow        => code.emit(THROW(definitions.ThrowableClass))
        case JVM.checkcast     =>
          code.emit(CHECK_CAST(toTypeKind(pool.getClassOrArrayType(u2)))); size += 2
        case JVM.instanceof    =>
          code.emit(IS_INSTANCE(toTypeKind(pool.getClassOrArrayType(u2)))); size += 2
        case JVM.monitorenter  => code.emit(MONITOR_ENTER())
        case JVM.monitorexit   => code.emit(MONITOR_EXIT())
        case JVM.wide          =>
          size += 1
          u1 match {
            case JVM.iload  => code.emit(LOAD_LOCAL(code.getLocal(u2, INT)));    size += 2
            case JVM.lload  => code.emit(LOAD_LOCAL(code.getLocal(u2, LONG)));   size += 2
            case JVM.fload  => code.emit(LOAD_LOCAL(code.getLocal(u2, FLOAT)));  size += 2
            case JVM.dload  => code.emit(LOAD_LOCAL(code.getLocal(u2, DOUBLE))); size += 2
            case JVM.aload  => code.emit(LOAD_LOCAL(code.getLocal(u2, ObjectReference))); size += 2
            case JVM.istore => code.emit(STORE_LOCAL(code.getLocal(u2, INT)));    size += 2
            case JVM.lstore => code.emit(STORE_LOCAL(code.getLocal(u2, LONG)));   size += 2
            case JVM.fstore => code.emit(STORE_LOCAL(code.getLocal(u2, FLOAT)));  size += 2
            case JVM.dstore => code.emit(STORE_LOCAL(code.getLocal(u2, DOUBLE))); size += 2
            case JVM.astore => code.emit(STORE_LOCAL(code.getLocal(u2, ObjectReference))); size += 2
            case JVM.ret => sys.error("Cannot handle jsr/ret")
            case JVM.iinc =>
              size += 4
              val local = code.getLocal(u2, INT)
              code.emit(CONSTANT(Constant(u2)))
              code.emit(CALL_PRIMITIVE(Arithmetic(ADD, INT)))
              code.emit(STORE_LOCAL(local))
            case _ => sys.error("Invalid 'wide' operand")
          }

        case JVM.multianewarray =>
          size += 3
          val tpe = toTypeKind(pool getClassOrArrayType u2)
          val dim = u1
//          assert(dim == 1, "Cannot handle multidimensional arrays yet.")
          code emit CREATE_ARRAY(tpe, dim)

        case JVM.ifnull    => code emit LCZJUMP(parseJumpTarget, pc + size, EQ, ObjectReference)
        case JVM.ifnonnull => code emit LCZJUMP(parseJumpTarget, pc + size, NE, ObjectReference)
        case JVM.goto_w    => code emit LJUMP(parseJumpTargetW)
        case JVM.jsr_w     => sys.error("Cannot handle jsr/ret")

//        case _ => sys.error("Unknown bytecode")
      }
      pc += size
    }

    // add parameters
    var idx = if (method.isStatic) 0 else 1
    for (t <- method.symbol.tpe.paramTypes) {
      val kind = toTypeKind(t)
      this.method addParam code.enterParam(idx, kind)
      val width = if (kind.isWideType) 2 else 1
      idx += width
    }

    pc = 0
    while (pc < codeLength) parseInstruction()

    val exceptionEntries = u2.toInt
    code.containsEHs = (exceptionEntries != 0)
    var i = 0
    while (i < exceptionEntries) {
      // skip start end PC
      in.skip(4)
      // read the handler PC
      code.jmpTargets += u2
      // skip the exception type
      in.skip(2)
      i += 1
    }
    skipAttributes()

    code.toBasicBlock
    assert(method.hasCode, method)
    // reverse parameters, as they were prepended during code generation
    method.params = method.params.reverse

    if (code.containsDUPX)
      code.resolveDups()

    if (code.containsNEW)
      code.resolveNEWs()
  }

  /** Note: these methods are different from the methods of the same name found
   *  in Definitions.  These test whether a symbol represents one of the boxTo/unboxTo
   *  methods found in BoxesRunTime.  The others test whether a symbol represents a
   *  synthetic method from one of the fake companion classes of the primitive types,
   *  such as Int.box(5).
   */
  def isBox(m: Symbol): Boolean =
    (m.owner == definitions.BoxesRunTimeClass
        && m.name.startsWith("boxTo"))

  def isUnbox(m: Symbol): Boolean =
    (m.owner == definitions.BoxesRunTimeClass
        && m.name.startsWith("unboxTo"))

  /** Return the icode class that should include members with the given flags.
   *  There are two possible classes, the static part and the instance part.
   */
  def getCode(flags: JavaAccFlags): IClass =
    if (isScalaModule || flags.isStatic) staticCode else instanceCode

  class LinearCode {
    val instrs: ListBuffer[(Int, Instruction)] = new ListBuffer
    val jmpTargets: mutable.Set[Int] = perRunCaches.newSet[Int]()
    val locals: mutable.Map[Int, List[(Local, TypeKind)]] = perRunCaches.newMap()

    var containsDUPX = false
    var containsNEW  = false
    var containsEHs  = false
    var containsInvokeDynamic = false

    def emit(i: Instruction) {
      instrs += ((pc, i))
      if (i.isInstanceOf[DupX])
        containsDUPX = true
      if (i.isInstanceOf[opcodes.NEW])
        containsNEW = true
    }

    /** Break this linear code in basic block representation
     *  As a side effect, it sets the `code` field of the current
     */
    def toBasicBlock: Code = {
      import opcodes._

      val code = new Code(method)
      method.setCode(code)
      method.bytecodeHasEHs = containsEHs
      method.bytecodeHasInvokeDynamic = containsInvokeDynamic
      var bb = code.startBlock

      def makeBasicBlocks: mutable.Map[Int, BasicBlock] =
        mutable.Map(jmpTargets.toSeq map (_ -> code.newBlock): _*)

      val blocks = makeBasicBlocks
      var otherBlock: BasicBlock = NoBasicBlock

      for ((pc, instr) <- instrs.iterator) {
//        Console.println("> " + pc + ": " + instr);
        if (jmpTargets(pc)) {
          otherBlock = blocks(pc)
          if (!bb.closed && otherBlock != bb) {
            bb.emit(JUMP(otherBlock))
            bb.close()
//            Console.println("\t> closing bb: " + bb)
          }
          bb = otherBlock
//          Console.println("\t> entering bb: " + bb)
        }

        if (bb.closed) {
          // the basic block is closed, i.e. the previous instruction was a jump, return or throw,
          // but the next instruction is not a jump target. this means that the next instruction is
          // dead code. we can therefore advance until the next jump target.
          debuglog(s"ICode reader skipping dead instruction $instr in classfile $instanceCode")
        } else {
          instr match {
            case LJUMP(target) =>
              otherBlock = blocks(target)
              bb.emitOnly(JUMP(otherBlock))

            case LCJUMP(success, failure, cond, kind) =>
              otherBlock = blocks(success)
              val failBlock = blocks(failure)
              bb.emitOnly(CJUMP(otherBlock, failBlock, cond, kind))

            case LCZJUMP(success, failure, cond, kind) =>
              otherBlock = blocks(success)
              val failBlock = blocks(failure)
              bb.emitOnly(CZJUMP(otherBlock, failBlock, cond, kind))

            case LSWITCH(tags, targets) =>
              bb.emitOnly(SWITCH(tags, targets map blocks))

            case RETURN(_) =>
              bb emitOnly instr

            case THROW(clasz) =>
              bb emitOnly instr

            case _ =>
              bb emit instr
          }
        }
      }

      method.code
    }

    def resolveDups() {
      import opcodes._

      val tfa = new analysis.MethodTFA() {
        import analysis._

        /** Abstract interpretation for one instruction. */
        override def mutatingInterpret(out: typeFlowLattice.Elem, i: Instruction): typeFlowLattice.Elem = {
          val stack = out.stack
          import stack.push
          i match {
            case DUP_X1 =>
              val (one, two) = stack.pop2
              push(one); push(two); push(one)

            case DUP_X2 =>
              val (one, two, three) = stack.pop3
              push(one); push(three); push(two); push(one)

            case DUP2_X1 =>
              val (one, two) = stack.pop2
              if (one.isWideType) {
                push(one); push(two); push(one)
              } else {
                val three = stack.pop
                push(two); push(one); push(three); push(two); push(one)
              }

            case DUP2_X2 =>
              val (one, two) = stack.pop2
              if (one.isWideType && two.isWideType) {
                push(one); push(two); push(one)
              } else if (one.isWideType) {
                val three = stack.pop
                assert(!three.isWideType, "Impossible")
                push(one); push(three); push(two); push(one)
              } else {
                val three = stack.pop
                if (three.isWideType) {
                  push(two); push(one); push(one); push(three); push(two); push(one)
                } else {
                  val four = stack.pop
                  push(two); push(one); push(four); push(one); push(three); push(two); push(one)
                }
              }

            case _ =>
              super.mutatingInterpret(out, i)
          }
          out
        }
      }

//      method.dump
      tfa.init(method)
      tfa.run()
      for (bb <- linearizer.linearize(method)) {
        var info = tfa.in(bb)
        for (i <- bb.toList) {
          i match {
            case DUP_X1 =>
              val one = info.stack.types(0)
              val two = info.stack.types(1)
              assert(!one.isWideType, "DUP_X1 expects values of size 1 on top of stack " + info.stack)
              val tmp1 = freshLocal(one)
              val tmp2 = freshLocal(two)
              bb.replaceInstruction(i, List(STORE_LOCAL(tmp1),
                  STORE_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1),
                  LOAD_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1)))

            case DUP_X2 =>
              val one = info.stack.types(0)
              val two = info.stack.types(1)
              assert (!one.isWideType, "DUP_X2 expects values of size 1 on top of stack " + info.stack)
              val tmp1 = freshLocal(one)
              val tmp2 = freshLocal(two)
              if (two.isWideType)
                bb.replaceInstruction(i, List(STORE_LOCAL(tmp1),
                  STORE_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1),
                  LOAD_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1)))
              else {
                val tmp3 = freshLocal(info.stack.types(2))
                bb.replaceInstruction(i, List(STORE_LOCAL(tmp1),
                  STORE_LOCAL(tmp2),
                  STORE_LOCAL(tmp3),
                  LOAD_LOCAL(tmp1),
                  LOAD_LOCAL(tmp3),
                  LOAD_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1)))
              }

            case DUP2_X1 =>
              val one = info.stack.types(0)
              val two = info.stack.types(1)
              val tmp1 = freshLocal(one)
              val tmp2 = freshLocal(two)
              if (one.isWideType) {
                assert(!two.isWideType, "Impossible")
                bb.replaceInstruction(i, List(STORE_LOCAL(tmp1),
                  STORE_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1),
                  LOAD_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1)))
              } else {
                val tmp3 = freshLocal(info.stack.types(2))
                bb.replaceInstruction(i, List(STORE_LOCAL(tmp1),
                  STORE_LOCAL(tmp2),
                  STORE_LOCAL(tmp3),
                  LOAD_LOCAL(tmp1),
                  LOAD_LOCAL(tmp3),
                  LOAD_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1)))
              }

            case DUP2_X2 =>
              val one = info.stack.types(0)
              val two = info.stack.types(1)
              val tmp1 = freshLocal(one)
              val tmp2 = freshLocal(two)
              if (one.isWideType && two.isWideType) {
                bb.replaceInstruction(i, List(STORE_LOCAL(tmp1),
                  STORE_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1),
                  LOAD_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1)))
              } else if (one.isWideType) {
                val three = info.stack.types(2)
                assert(!two.isWideType && !three.isWideType, "Impossible")
                val tmp3 = freshLocal(three)
                bb.replaceInstruction(i, List(STORE_LOCAL(tmp1),
                  STORE_LOCAL(tmp2),
                  STORE_LOCAL(tmp3),
                  LOAD_LOCAL(tmp1),
                  LOAD_LOCAL(tmp3),
                  LOAD_LOCAL(tmp2),
                  LOAD_LOCAL(tmp1)))
              } else {
                val three = info.stack.types(2)
                val tmp3 = freshLocal(three)
                if (three.isWideType) {
                  bb.replaceInstruction(i, List(STORE_LOCAL(tmp1),
                      STORE_LOCAL(tmp2),
                      STORE_LOCAL(tmp3),
                      LOAD_LOCAL(tmp2),
                      LOAD_LOCAL(tmp1),
                      LOAD_LOCAL(tmp3),
                      LOAD_LOCAL(tmp2),
                      LOAD_LOCAL(tmp1)))
                } else {
                  val four = info.stack.types(3)
                  val tmp4 = freshLocal(three)
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
                      LOAD_LOCAL(tmp1)))
                }
              }
            case _ =>
          }
          info = tfa.interpret(info, i)
        }
      }
    }

    /** Recover def-use chains for NEW and initializers. */
    def resolveNEWs() {
      import opcodes._
      val rdef = new reachingDefinitions.ReachingDefinitionsAnalysis
      rdef.init(method)
      rdef.run()

      for (bb <- method.code.blocks ; (i, idx) <- bb.toList.zipWithIndex) i match {
        case cm @ CALL_METHOD(m, Static(true)) if m.isClassConstructor =>
          def loop(bb0: BasicBlock, idx0: Int, depth: Int): Unit = {
            rdef.findDefs(bb0, idx0, 1, depth) match {
              case ((bb1, idx1)) :: _ =>
                bb1(idx1) match {
                  case _: DUP   => loop(bb1, idx1, 0)
                  case x: NEW   => x.init = cm
                  case _: THIS  => () // super constructor call
                  case producer => dumpMethodAndAbort(method, "producer: " + producer)
                }
              case _ => ()
            }
          }
          loop(bb, idx, m.info.paramTypes.length)

        case _ => ()
      }
    }

    /** Return the local at given index, with the given type. */
    def getLocal(idx: Char, kind: TypeKind): Local = getLocal(idx.toInt, kind)
    def getLocal(idx: Int, kind: TypeKind): Local = {
      assert(idx < maxLocals, "Index too large for local variable.")

      def checkValidIndex() {
        locals.get(idx - 1) match {
          case Some(others) if others exists (_._2.isWideType) =>
            global.globalError("Illegal index: " + idx + " points in the middle of another local")
          case _ => ()
        }
        kind match {
          case LONG | DOUBLE if (locals.isDefinedAt(idx + 1)) =>
            global.globalError("Illegal index: " + idx + " overlaps " + locals(idx + 1) + "\nlocals: " + locals)
          case _ => ()
        }
      }

      locals.get(idx) match {
        case Some(ls) =>
          val l = ls find { loc => loc._2 isAssignabledTo kind }
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
          checkValidIndex()
          val l = freshLocal(idx, kind, isArg = false)
          debuglog("Added new local for idx " + idx + ": " + kind)
          locals += (idx -> List((l, kind)))
          l
      }
    }

    override def toString(): String = instrs.toList.mkString("", "\n", "")

    /** Return a fresh Local variable for the given index.
     */
    private def freshLocal(idx: Int, kind: TypeKind, isArg: Boolean) = {
      val sym = method.symbol.newVariable(newTermName("loc" + idx)).setInfo(kind.toType)
      val l = new Local(sym, kind, isArg)
      method.addLocal(l)
      l
    }

    private var count = 0

    /** Invent a new local, with a new index value outside the range of
     *  the original method. */
    def freshLocal(kind: TypeKind): Local = {
      count += 1
      freshLocal(maxLocals + count, kind, isArg = false)
    }

    /** add a method param with the given index. */
    def enterParam(idx: Int, kind: TypeKind) = {
      val sym = method.symbol.newVariable(newTermName("par" + idx)).setInfo(kind.toType)
      val l = new Local(sym, kind, true)
      assert(!locals.isDefinedAt(idx), locals(idx))
      locals += (idx -> List((l, kind)))
      l
    }

    /** Base class for branch instructions that take addresses. */
    abstract class LazyJump(pc: Int) extends Instruction {
      override def toString() = "LazyJump " + pc
      jmpTargets += pc
    }

    case class LJUMP(pc: Int) extends LazyJump(pc)

    case class LCJUMP(success: Int, failure: Int, cond: TestOp, kind: TypeKind)
      extends LazyJump(success) {
      override def toString(): String = "LCJUMP (" + kind + ") " + success + " : " + failure

      jmpTargets += failure
    }

    case class LCZJUMP(success: Int, failure: Int, cond: TestOp, kind: TypeKind)
      extends LazyJump(success) {
      override def toString(): String = "LCZJUMP (" + kind + ") " + success + " : " + failure

      jmpTargets += failure
    }

    case class LSWITCH(tags: List[List[Int]], targets: List[Int]) extends LazyJump(targets.head) {
      override def toString(): String = "LSWITCH (tags: " + tags + ") targets: " + targets

      jmpTargets ++= targets.tail
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
