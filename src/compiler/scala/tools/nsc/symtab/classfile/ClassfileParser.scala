/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

/* Ideas to extend this to an icode reader:
 *
 * 1. Parse classfile a second time, creating a hashmap `code' that
 *    associates method symbols with code.
 * 2. For every method symbol `meth' in the new scope:
 *
 *    new = oldclass.info.decl(meth.name).suchThat(old => old.tpe =:= meth.tpe)
 *
 *   code(new) = code(meth)
 */
package scala.tools.nsc.symtab.classfile

import scala.tools.nsc.util.Position
import scala.tools.nsc.io.AbstractFile
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import scala.collection.immutable.{Map, ListMap}

import java.lang.Integer.toHexString
import java.io.IOException

/** This abstract class implements a class file parser.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
abstract class ClassfileParser {
  def sourcePath : AbstractFile = null

  val global: Global
  import global._

  import ClassfileConstants._
  import Flags._

  protected var in: AbstractFileReader = _  // the class file reader
  protected var clazz: Symbol = _           // the class symbol containing dynamic members
  protected var staticModule: Symbol = _    // the module symbol containing static members
  protected var instanceDefs: Scope = _     // the scope of all instance definitions
  protected var staticDefs: Scope = _       // the scope of all static definitions
  protected var pool: ConstantPool = _      // the classfile's constant pool
  protected var isScala: boolean = _        // does class file describe a scala class?
  protected var hasMeta: boolean = _        // does class file contain jaco meta attribute?s
  protected var busy: boolean = false       // lock to detect recursive reads
  protected var classTParams = Map[Name,Symbol]()
  protected val fresh = new scala.tools.nsc.util.FreshNameCreator

  private object metaParser extends MetaParser {
    val global: ClassfileParser.this.global.type = ClassfileParser.this.global
  }

  private object unpickler extends UnPickler {
    val global: ClassfileParser.this.global.type = ClassfileParser.this.global
  }

  def parse(file: AbstractFile, root: Symbol): unit = {
    def handleError(e: Exception) = {
      if (settings.debug.value) e.printStackTrace();//debug
      throw new IOException("class file '" + in.file + "' is broken\n(" + e.getMessage() + ")")
    }
    assert(!busy)
    busy = true
    root match {
      case cs : ClassSymbol => cs.classFile = file;
      case ms : ModuleSymbol => ms.moduleClass.asInstanceOf[ClassSymbol].classFile = file;
      case _ =>
        Console.println("Skipping class: " + root + ": " + root.getClass);
    }

    this.in = new AbstractFileReader(file)
    if (root.isModule) {
      this.clazz = root.linkedClassOfModule
      this.staticModule = root
    } else {
      this.clazz = root
      this.staticModule = root.linkedModuleOfClass
    }
    this.isScala = false
    this.hasMeta = false
    try {
      parseHeader
      this.pool = new ConstantPool
      parseClass()
    } catch {
      case e: FatalError => handleError(e)
      case e: RuntimeException => handleError(e)
    }
    busy = false
  }

  protected def statics: Symbol = staticModule.moduleClass

  private def parseHeader: unit = {
    val magic = in.nextInt
    if (magic != JAVA_MAGIC)
      throw new IOException("class file '" + in.file + "' "
                            + "has wrong magic number 0x" + toHexString(magic)
                            + ", should be 0x" + toHexString(JAVA_MAGIC))
    val minorVersion = in.nextChar
    val majorVersion = in.nextChar
    if ((majorVersion < JAVA_MAJOR_VERSION) ||
        ((majorVersion == JAVA_MAJOR_VERSION) &&
         (minorVersion < JAVA_MINOR_VERSION)))
      throw new IOException("class file '" + in.file + "' "
                            + "has unknown version "
                            + majorVersion + "." + minorVersion
                            + ", should be at least "
                            + JAVA_MAJOR_VERSION + "." + JAVA_MINOR_VERSION)
  }

  class ConstantPool {
    private val len = in.nextChar
    private val starts = new Array[int](len)
    private val values = new Array[AnyRef](len)
    private val internalized = new Array[Name](len);
    { var i = 1
      while (i < starts.length) {
        starts(i) = in.bp
        i = i + 1
        in.nextByte match {
          case CONSTANT_UTF8 | CONSTANT_UNICODE =>
            in.skip(in.nextChar)
          case CONSTANT_CLASS | CONSTANT_STRING =>
            in.skip(2)
          case CONSTANT_FIELDREF | CONSTANT_METHODREF | CONSTANT_INTFMETHODREF
             | CONSTANT_NAMEANDTYPE | CONSTANT_INTEGER | CONSTANT_FLOAT =>
            in.skip(4)
          case CONSTANT_LONG | CONSTANT_DOUBLE =>
            in.skip(8)
            i = i + 1
          case _ =>
            errorBadTag(in.bp - 1)
        }
      }
    }

    def getName(index: int): Name = {
      if (index <= 0 || len <= index) errorBadIndex(index)
      var name = values(index).asInstanceOf[Name]
      if (name eq null) {
        val start = starts(index)
        if (in.buf(start) != CONSTANT_UTF8) errorBadTag(start)
        name = newTermName(in.buf, start + 3, in.getChar(start + 1))
        values(index) = name
      }
      name
    }

    def getExternalName(index: int): Name = {
      if (index <= 0 || len <= index) errorBadIndex(index)
      if (internalized(index) eq null) {
        internalized(index) = getName(index).replace('/', '.')
      }
      internalized(index)
    }

    def getClassSymbol(index: int): Symbol = {
      if (index <= 0 || len <= index) errorBadIndex(index)
      var c = values(index).asInstanceOf[Symbol]
      if (c eq null) {
        val start = starts(index)
        if (in.buf(start) != CONSTANT_CLASS) errorBadTag(start)
        val name = getExternalName(in.getChar(start + 1))
        if (name.endsWith("$"))
          c = definitions.getModule(name.subName(0, name.length - 1))
        else
          c = classNameToSymbol(name)
        values(index) = c
      }
      c
    }

    /** Return the symbol of the class member at <code>index</code>.
     *  The following special cases exist:
     *   - If the member refers to special MODULE$ static field, return
     *  the symbol of the corresponding module.
     *   - If the member is a field, and is not found with the given name,
     *     another try is made by appending nme.LOCAL_SUFFIX
     *   - If no symbol is found in the right tpe, a new try is made in the
     *     companion class, in case the owner is an implementation class.
     */
    def getMemberSymbol(index: Int, static: Boolean): Symbol = {
      if (index <= 0 || len <= index) errorBadIndex(index)
      var f = values(index).asInstanceOf[Symbol]
      if (f eq null) {
        val start = starts(index)
        if (in.buf(start) != CONSTANT_FIELDREF &&
            in.buf(start) != CONSTANT_METHODREF &&
            in.buf(start) != CONSTANT_INTFMETHODREF) errorBadTag(start)
        val ownerTpe = getClassOrArrayType(in.getChar(start + 1))
        val (name, tpe) = getNameAndType(in.getChar(start + 3), ownerTpe)
        if (name == nme.MODULE_INSTANCE_FIELD) {
          val index = in.getChar(start + 1)
          val name = getExternalName(in.getChar(starts(index) + 1))
          assert(name.endsWith("$"), "Not a module class: " + name)
          f = definitions.getModule(name.subName(0, name.length - 1))
        } else {
          val owner = if (static) ownerTpe.symbol.linkedClassOfClass else ownerTpe.symbol
//          Console.println("" + owner.info.decl(name).tpe + " =:= " + tpe)
          f = owner.info.member(name).suchThat(.tpe.=:=(tpe))
          if (f == NoSymbol)
            f = owner.info.member(newTermName(name.toString + nme.LOCAL_SUFFIX)).suchThat(.tpe.=:=(tpe))
          if (f == NoSymbol) {
            // if it's an impl class, try to find it's static member inside the class
            assert(ownerTpe.symbol.isImplClass, "Not an implementation class: " + owner + " couldn't find " + name + ": " + tpe);
            f = ownerTpe.member(name).suchThat(.tpe.=:=(tpe))
          }
        }
        assert(f != NoSymbol)
        values(index) = f
      }
      f
    }

    def getNameAndType(index: Int, ownerTpe: Type): (Name, Type) = {
      if (index <= 0 || len <= index) errorBadIndex(index)
      var p = values(index).asInstanceOf[(Name, Type)]
      if (p eq null) {
        val start = starts(index)
        if (in.buf(start) != CONSTANT_NAMEANDTYPE) errorBadTag(start)
        val name = getName(in.getChar(start + 1))
        var tpe  = getType(in.getChar(start + 3))
        if (name == nme.CONSTRUCTOR)
          tpe match {
            case MethodType(formals, restpe) =>
              assert(restpe.symbol == definitions.UnitClass)
              tpe = MethodType(formals, ownerTpe)
          }

        p = (name, tpe)
      }
      p
    }
    /** Return the type of a class constant entry. Since
     *  arrays are considered to be class types, they might
     *  appear as entries in 'newarray' or 'cast' opcodes.
     */
    def getClassOrArrayType(index: Int): Type = {
      if (index <= 0 || len <= index) errorBadIndex(index)
      val value = values(index)
      var c: Type = null
      if (value eq null) {
        val start = starts(index)
        if (in.buf(start) != CONSTANT_CLASS) errorBadTag(start)
        val name = getExternalName(in.getChar(start + 1))
        if (name(0) == ARRAY_TAG) {
          c = sigToType(name)
          values(index) = c
        } else {
          values(index) = definitions.getClass(name)
          c = definitions.getClass(name).tpe
        }
      } else c = value match {
          case tp: Type => tp
          case cls: Symbol => cls.tpe
      }
      c
    }

    def getType(index: int): Type =
      sigToType(getExternalName(index))

    def getSuperClass(index: int): Symbol =
      if (index == 0) definitions.AnyClass else getClassSymbol(index)

    def getConstant(index: int): Constant = {
      if (index <= 0 || len <= index) errorBadIndex(index)
      var value = values(index)
      if (value eq null) {
        val start = starts(index)
        value = in.buf(start) match {
          case CONSTANT_STRING =>
            Constant(getName(in.getChar(start + 1)).toString())
          case CONSTANT_INTEGER =>
            Constant(in.getInt(start + 1))
          case CONSTANT_FLOAT =>
            Constant(in.getFloat(start + 1))
          case CONSTANT_LONG =>
            Constant(in.getLong(start + 1))
          case CONSTANT_DOUBLE =>
            Constant(in.getDouble(start + 1))
          case CONSTANT_CLASS =>
            getClassSymbol(index)
          case _ =>
            errorBadTag(start)
        }
        values(index) = value
      }
      value match {
        case  ct: Constant => ct
        case cls: Symbol   => Constant(cls.tpe)
      }
    }

    /** Throws an exception signaling a bad constant index. */
    private def errorBadIndex(index: int) =
      throw new RuntimeException("bad constant pool index: " + index + " at pos: " + in.bp)

    /** Throws an exception signaling a bad tag at given address. */
    private def errorBadTag(start: int) =
      throw new RuntimeException("bad constant pool tag " + in.buf(start) + " at byte " + start)
  }

  def sigToType(name: Name): Type = {
    var index = 0
    val end = name.length
    def objToAny(tp: Type): Type =
      if (!global.phase.erasedTypes && tp.symbol == definitions.ObjectClass) definitions.AnyClass.tpe
      else tp
    def paramsigs2types: List[Type] =
      if (name(index) == ')') { index = index + 1; List() }
      else objToAny(sig2type) :: paramsigs2types
    def sig2type: Type = {
      val tag = name(index); index = index + 1
      tag match {
        case BYTE_TAG   => definitions.ByteClass.tpe
        case CHAR_TAG   => definitions.CharClass.tpe
        case DOUBLE_TAG => definitions.DoubleClass.tpe
        case FLOAT_TAG  => definitions.FloatClass.tpe
        case INT_TAG    => definitions.IntClass.tpe
        case LONG_TAG   => definitions.LongClass.tpe
        case SHORT_TAG  => definitions.ShortClass.tpe
        case VOID_TAG   => definitions.UnitClass.tpe
        case BOOL_TAG   => definitions.BooleanClass.tpe
        case 'L' =>
          val start = index
          while (name(index) != ';') { index = index + 1 }
          val end = index
          index = index + 1
          classNameToSymbol(name.subName(start, end)).tpe
        case ARRAY_TAG =>
          while ('0' <= name(index) && name(index) <= '9') index = index + 1
          appliedType(definitions.ArrayClass.tpe, List(sig2type))
        case '(' =>
          JavaMethodType(paramsigs2types, sig2type)
      }
    }
    sig2type
  }

  /** Return the class symbol of the given name. */
  def classNameToSymbol(name: Name) =
    if (name.pos('.') == name.length)
      definitions.getMember(definitions.EmptyPackageClass, name.toTypeName)
    else
      definitions.getClass(name)


  var sawPrivateConstructor = false

  def parseClass(): unit = {
    val jflags = in.nextChar
    val isAnnotation = (jflags & JAVA_ACC_ANNOTATION) != 0
    var sflags = transFlags(jflags)
    if ((sflags & DEFERRED) != 0) sflags = sflags & ~DEFERRED | ABSTRACT
    val c = pool.getClassSymbol(in.nextChar)
    if (c != clazz)
      throw new IOException("class file '" + in.file + "' contains wrong " + c)
    val superType = if (isAnnotation) { in.nextChar; definitions.AnnotationClass.tpe }
                    else pool.getSuperClass(in.nextChar).tpe
    val ifaceCount = in.nextChar
    var ifaces = for (val i <- List.range(0, ifaceCount)) yield pool.getSuperClass(in.nextChar).tpe
    if (isAnnotation) ifaces = definitions.ClassfileAnnotationClass.tpe :: ifaces
    val parents = superType :: ifaces

    instanceDefs = newScope
    staticDefs = newScope
    val classInfo = ClassInfoType(parents, instanceDefs, clazz)
    val staticInfo = ClassInfoType(List(), staticDefs, statics)

    val curbp = in.bp
    skipMembers() // fields
    skipMembers() // methods
    parseAttributes(clazz, classInfo)
    if (!isScala) {
      clazz.setFlag(sflags)
      setPrivateWithin(clazz, jflags)
      if (!hasMeta) {
        clazz.setInfo(classInfo)
      }
      statics.setInfo(staticInfo)
      staticModule.setInfo(statics.tpe)
      staticModule.setFlag(JAVA)
      staticModule.moduleClass.setFlag(JAVA)
      in.bp = curbp
      val fieldCount = in.nextChar
      for (val i <- 0 until fieldCount) parseField()
      sawPrivateConstructor = false
      val methodCount = in.nextChar
      for (val i <- 0 until methodCount) parseMethod()
      if (!sawPrivateConstructor &&
          (instanceDefs.lookup(nme.CONSTRUCTOR) == NoSymbol &&
           (sflags & INTERFACE) == 0))
        {
          //Console.println("adding constructor to " + clazz);//DEBUG
          instanceDefs.enter(
            clazz.newConstructor(NoPos)
            .setFlag(clazz.flags & ConstrFlags)
            .setInfo(MethodType(List(), clazz.tpe)))

          // If the annotation has an attribute with name 'value'
          // add a constructor for it
          if (isAnnotation) {
            val value = instanceDefs.lookup(nme.value)
            if (value != NoSymbol) {
              instanceDefs.enter(
                clazz.newConstructor(NoPos)
                .setFlag(clazz.flags & ConstrFlags)
                .setInfo(MethodType(List(value.tpe.resultType), clazz.tpe)))
            }
          }
        }
    }
  }

  def parseField(): unit = {
    val jflags = in.nextChar
    var sflags = transFlags(jflags)
    if ((sflags & FINAL) == 0) sflags = sflags | MUTABLE
    if ((sflags & PRIVATE) != 0 && !global.settings.XbytecodeRead.value) {
      in.skip(4); skipAttributes()
    } else {
      val name = pool.getName(in.nextChar)
      val info = pool.getType(in.nextChar)
      val sym = getOwner(jflags)
        .newValue(NoPos, name).setFlag(sflags)
      sym.setInfo(if ((jflags & JAVA_ACC_ENUM) == 0) info else mkConstantType(Constant(sym)))
      setPrivateWithin(sym, jflags)
      parseAttributes(sym, info)
      getScope(jflags).enter(sym)
    }
  }

  def parseMethod(): unit = {
    val jflags = in.nextChar
    var sflags = transFlags(jflags)
    if ((jflags & JAVA_ACC_PRIVATE) != 0 && !global.settings.XbytecodeRead.value) {
      val name = pool.getName(in.nextChar)
      if (name == nme.CONSTRUCTOR)
        sawPrivateConstructor = true
      in.skip(2); skipAttributes()
    } else {
      if ((jflags & JAVA_ACC_BRIDGE) != 0) sflags = sflags | PRIVATE
      if ((sflags & PRIVATE) != 0 && !global.settings.XbytecodeRead.value) {
        in.skip(4); skipAttributes()
      } else {
        val name = pool.getName(in.nextChar)
        var info = pool.getType(in.nextChar)
        if (name == nme.CONSTRUCTOR)
          info match {
            case MethodType(formals, restpe) =>
              assert(restpe.symbol == definitions.UnitClass)
              info = MethodType(formals, clazz.tpe)
          }
        val sym = getOwner(jflags)
          .newMethod(NoPos, name).setFlag(sflags).setInfo(info)
        setPrivateWithin(sym, jflags)
        parseAttributes(sym, info)
        getScope(jflags).enter(sym)
      }
    }
  }


  // returns the generic type represented by the signature
  private def polySigToType(sym: Symbol, sig: Name): Type =
    try { polySigToType0(sym, sig) }
    catch {
      case e: Throwable =>
        Console.err.println("" + sym + " - " + sig);
        throw e
    }
  private def polySigToType0(sym: Symbol, sig: Name): Type = {
    var index = 0
    val end = sig.length
    val newTParams = new ListBuffer[Symbol]()
    def objToAny(tp: Type): Type =
      if (tp.symbol == definitions.ObjectClass) definitions.AnyClass.tpe
      else tp
    def subName(isDelimiter: Char => Boolean): Name = {
      val start = index
      while (!isDelimiter(sig(index))) { index = index + 1; }
      sig.subName(start, index)
    }
    def typeParams(tparams: Map[Name,Symbol], covariant: Boolean): List[Type] = {
      assert(sig(index) == '<')
      index = index + 1
      val xs = new ListBuffer[Type]()
      while (sig(index) != '>') {
        sig(index) match {
          case variance @ ('+' | '-' | '*') =>
            index = index + 1
            val bounds = variance match {
              case '+' => mkTypeBounds(definitions.AllRefClass.typeConstructor,
                                       sig2type(tparams, covariant))
              case '-' => mkTypeBounds(sig2type(tparams, covariant),
                                       definitions.AnyRefClass.typeConstructor)
              case '*' => mkTypeBounds(definitions.AllRefClass.typeConstructor,
                                       definitions.AnyRefClass.typeConstructor)
            }
            val name = fresh.newName("T_" + sym.name)
            val newtparam =
              if (covariant) clazz.newAbstractType(NoPos, name)
              else {
                val s = sym.newTypeParameter(NoPos, name)
                newTParams += s
                s
              }
            newtparam.setInfo(bounds)
            xs += newtparam.tpe
          case _ => xs += sig2type(tparams, covariant)
        }
      }
      index = index + 1
      xs.toList
    }
    def sig2type(tparams: Map[Name,Symbol], covariant: Boolean): Type = {
      val tag = sig(index); index = index + 1
      tag match {
        case BYTE_TAG   => definitions.ByteClass.tpe
        case CHAR_TAG   => definitions.CharClass.tpe
        case DOUBLE_TAG => definitions.DoubleClass.tpe
        case FLOAT_TAG  => definitions.FloatClass.tpe
        case INT_TAG    => definitions.IntClass.tpe
        case LONG_TAG   => definitions.LongClass.tpe
        case SHORT_TAG  => definitions.ShortClass.tpe
        case VOID_TAG   => definitions.UnitClass.tpe
        case BOOL_TAG   => definitions.BooleanClass.tpe
        case 'L' =>
          var tpe = definitions.getClass(subName(c => ((c == ';') || (c == '<')))).tpe
          if (sig(index) == '<')
            tpe = appliedType(tpe, typeParams(tparams, covariant))
          index = index + 1
          tpe
        case ARRAY_TAG =>
          while ('0' <= sig(index) && sig(index) <= '9') index = index + 1
          appliedType(definitions.ArrayClass.tpe, List(sig2type(tparams, covariant)))
        case '(' =>
          val paramtypes = new ListBuffer[Type]()
          while (sig(index) != ')') {
            paramtypes += objToAny(sig2type(tparams, false))
          }
          index = index + 1
          val restype = if (sym.isConstructor) {
            assert(sig(index) == 'V')
            index = index + 1
            clazz.tpe
          } else
            sig2type(tparams, true)
          MethodType(paramtypes.toList, restype)
        case 'T' =>
          val n = subName(';'.==).toTypeName
          index = index + 1
          tparams(n).typeConstructor
      }
    }
    var tparams = classTParams
    if (sig(index) == '<') {
      index = index + 1
      while (sig(index) != '>') {
        val tpname = subName(':'.==).toTypeName
        val s = sym.newTypeParameter(NoPos, tpname)
        tparams = tparams + tpname -> s
        val ts = new ListBuffer[Type]
        while (sig(index) == ':') {
          index = index + 1
          if (sig(index) != ':') // guard against empty class bound
            ts += sig2type(tparams, false)
        }
        s.setInfo(mkTypeBounds(definitions.AllRefClass.typeConstructor,
                               intersectionType(ts.toList, sym)))
        newTParams += s
      }
      index = index + 1
    }
    val tpe =
      if (sym.isClass) {
        classTParams = tparams
        val parents = new ListBuffer[Type]()
        while (index < end) {
          parents += sig2type(tparams, true);  // here the variance doesnt'matter
        }
        ClassInfoType(parents.toList, instanceDefs, sym)
      }
      else
        sig2type(tparams, true)
    if (newTParams.length == 0) tpe
    else PolyType(newTParams.toList, tpe)
  }

  def parseAttributes(sym: Symbol, symtype: Type): unit = {
    def convertTo(c: Constant, pt: Type): Constant = {
      if (pt.symbol == definitions.BooleanClass && c.tag == IntTag)
        Constant(c.value != 0)
      else
        c convertTo pt
    }
    def parseAttribute(): unit = {
      val attrName = pool.getName(in.nextChar)
      val attrLen = in.nextInt
      val oldpb = in.bp
      attrName match {
        case nme.SignatureATTR =>
          if (global.settings.Xgenerics.value) {
            val sig = pool.getExternalName(in.nextChar)
            val newType = polySigToType(sym, sig)
            sym.setInfo(newType)
            if (settings.debug.value)
              global.inform("" + sym + "; signatire = " + sig + " type = " + newType)
            hasMeta = true
          } else
            in.skip(attrLen)
        case nme.SyntheticATTR =>
          sym.setFlag(SYNTHETIC)
          in.skip(attrLen)
        case nme.BridgeATTR =>
          sym.setFlag(BRIDGE)
          in.skip(attrLen)
        case nme.DeprecatedATTR =>
          sym.setFlag(DEPRECATED)
          in.skip(attrLen)
        case nme.ConstantValueATTR =>
          val c = pool.getConstant(in.nextChar)
          val c1 = convertTo(c, symtype)
          if (c1 ne null) sym.setInfo(mkConstantType(c1))
          else Console.println("failure to convert " + c + " to " + symtype); //debug
        case nme.InnerClassesATTR =>
          if (!isScala) parseInnerClasses() else in.skip(attrLen)
        case nme.ScalaSignatureATTR =>
          unpickler.unpickle(in.buf, in.bp, clazz, staticModule, in.file.toString())
          in.skip(attrLen)
          this.isScala = true
        case nme.JacoMetaATTR =>
          val meta = pool.getName(in.nextChar).toString().trim()
          metaParser.parse(meta, sym, symtype)
          this.hasMeta = true
        case nme.SourceFileATTR =>
          assert(attrLen == 2)
          val source = pool.getName(in.nextChar)
          if (sourcePath ne null) {
            val sourceFile0 = sourcePath.lookupPath(source.toString(), false)
            if ((sourceFile0 ne null) && (clazz.sourceFile eq null)) {
              clazz.sourceFile = sourceFile0
            }
            staticModule.moduleClass.sourceFile = clazz.sourceFile
          }
        case nme.AnnotationDefaultATTR =>
          sym.attributes =
            AnnotationInfo(definitions.AnnotationDefaultAttr.tpe, List(), List()) :: sym.attributes
          in.skip(attrLen)
        case nme.RuntimeAnnotationATTR =>
          parseAnnotations(attrLen)
          if (settings.debug.value)
            global.inform("" + sym + "; attributes = " + sym.attributes)
        case _ =>
          in.skip(attrLen)
      }
    }
    def parseTaggedConstant: Constant = {
      val tag = in.nextByte
      val index = in.nextChar
      tag match {
        case STRING_TAG => Constant(pool.getName(index).toString())
        case BOOL_TAG   => pool.getConstant(index)
        case BYTE_TAG   => pool.getConstant(index)
        case CHAR_TAG   => pool.getConstant(index)
        case SHORT_TAG  => pool.getConstant(index)
        case INT_TAG    => pool.getConstant(index)
        case LONG_TAG   => pool.getConstant(index)
        case FLOAT_TAG  => pool.getConstant(index)
        case DOUBLE_TAG => pool.getConstant(index)
        case CLASS_TAG  => Constant(pool.getType(index))
        case ENUM_TAG   =>
          val t = pool.getType(index)
          val n = pool.getName(in.nextChar)
          val s = t.symbol.linkedModuleOfClass.info.decls.lookup(n)
          //assert (s != NoSymbol, "while processing " + in.file + ": " + t + "." + n + ": " + t.decls)
          assert(s != NoSymbol, t) // avoid string concatenation!
          Constant(s)
        case ARRAY_TAG  =>
          val arr = new ArrayBuffer[Constant]()
          for (val i <- 0 until index) {
            arr += parseTaggedConstant
          }
          new ArrayConstant(arr.toArray,
              appliedType(definitions.ArrayClass.typeConstructor, List(arr(0).tpe)))
      }
    }
    def parseAnnotations(len: Int): Unit = {
      val nAttr = in.nextChar
      for (val n <- 0 until nAttr) {
        val attrNameIndex = in.nextChar
        val attrType = pool.getType(attrNameIndex)
        val nargs = in.nextChar
        val nvpairs = new ListBuffer[(Name,Constant)]
        for (val i <- 0 until nargs) {
          val name = pool.getName(in.nextChar)
          nvpairs += (name, parseTaggedConstant)
        }
        sym.attributes = AnnotationInfo(attrType, List(), nvpairs.toList) :: sym.attributes
      }
    }

    def parseInnerClasses(): unit = {
      for (val i <- 0 until in.nextChar) {
        val innerIndex = in.nextChar
        val outerIndex = in.nextChar
        val nameIndex = in.nextChar
        val jflags = in.nextChar
        if (innerIndex != 0 && outerIndex != 0 && nameIndex != 0 &&
            (jflags & (JAVA_ACC_PUBLIC | JAVA_ACC_PROTECTED)) != 0 &&
            pool.getClassSymbol(outerIndex) == sym) {
          val innerAlias = getOwner(jflags)
            .newAliasType(NoPos, pool.getName(nameIndex).toTypeName)
            .setInfo(pool.getClassSymbol(innerIndex).tpe)
          getScope(jflags).enter(innerAlias)

          if ((jflags & JAVA_ACC_STATIC) != 0) {
            val innerVal = staticModule.newValue(NoPos, pool.getName(nameIndex).toTermName)
              .setInfo(pool.getClassSymbol(innerIndex).linkedModuleOfClass.moduleClass.thisType)
            staticDefs.enter(innerVal)
          }
        }
      }
    }
    val attrCount = in.nextChar
    for (val i <- 0 until attrCount) parseAttribute()
  }

  def skipAttributes(): unit = {
    val attrCount = in.nextChar
    for (val i <- 0 until attrCount) {
      in.skip(2); in.skip(in.nextInt)
    }
  }

  def skipMembers(): unit = {
    val memberCount = in.nextChar
    for (val i <- 0 until memberCount) {
      in.skip(6); skipAttributes()
    }
  }

  protected def getOwner(flags: int): Symbol =
    if ((flags & JAVA_ACC_STATIC) != 0) statics else clazz

  protected def getScope(flags: int): Scope =
    if ((flags & JAVA_ACC_STATIC) != 0) staticDefs else instanceDefs

  protected def transFlags(flags: int): long = {
    var res = 0l
    if ((flags & JAVA_ACC_PRIVATE) != 0)
      res = res | PRIVATE
    else if ((flags & JAVA_ACC_PROTECTED) != 0)
      res = res | PROTECTED
    if ((flags & JAVA_ACC_ABSTRACT) != 0 && (flags & JAVA_ACC_ANNOTATION) == 0)
      res = res | DEFERRED
    if ((flags & JAVA_ACC_FINAL) != 0)
      res = res | FINAL
    if (((flags & JAVA_ACC_INTERFACE) != 0) &&
        ((flags & JAVA_ACC_ANNOTATION) == 0))
      res = res | TRAIT | INTERFACE | ABSTRACT
    if ((flags & JAVA_ACC_SYNTHETIC) != 0)
      res = res | SYNTHETIC
    if ((flags & JAVA_ACC_STATIC) != 0)
      res = res | STATIC
    res | JAVA
  }

  private def setPrivateWithin(sym: Symbol, jflags: int): unit =
    if ((jflags & (JAVA_ACC_PRIVATE | JAVA_ACC_PROTECTED | JAVA_ACC_PUBLIC)) == 0)
      sym.privateWithin = sym.toplevelClass.owner
}
