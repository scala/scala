/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

/* Ideas to extend this to an icode reader:

  1. Parse classfile a second time, creating a hashmap `code' that associates method symbols with code.
  2. For every method symbol `meth' in the new scope:

    new = oldclass.info.decl(meth.name).suchThat(old => old.tpe =:= meth.tpe)

    code(new) = code(meth)
*/

package scala.tools.nsc.symtab.classfile;

import scala.tools.nsc.util.Position;
import scala.tools.nsc.io.{AbstractFile, AbstractFileReader};
import scala.collection.mutable.ListBuffer;
import scala.collection.immutable.{Map, ListMap};

import java.io.IOException;

abstract class ClassfileParser {
  def sourcePath : AbstractFile = null;

  val global: Global;
  import global._;

  import ClassfileConstants._;
  import Flags._;

  private var in: AbstractFileReader = _;  // the class file
  private var clazz: Symbol = _;           // the class symbol containing dynamic members
  private var staticModule: Symbol = _;    // the module symbol containing static members
  private var instanceDefs: Scope = _;     // the scope of all instance definitions
  private var staticDefs: Scope = _;       // the scope of all static definitions
  private var pool: ConstantPool = _;      // the classfile's constant pool
  private var isScala: boolean = _;        // does class file describe a scala class?
  private var hasMeta: boolean = _;        // does class file contain jaco meta attribute?s
  private var busy: boolean = false;       // lock to detect recursive reads
  private var classTParams: Map[Name,Symbol] =
    collection.immutable.ListMap.Empty[Name,Symbol];
  private val fresh = new scala.tools.nsc.util.FreshNameCreator;

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
    assert(!busy);
    busy = true;
    this.in = new AbstractFileReader(file);
    if (root.isModule) {
      this.clazz = root.linkedClass;
      this.staticModule = root
    } else {
      this.clazz = root;
      this.staticModule = root.linkedModule
    }
    this.isScala = false;
    this.hasMeta = false;
    try {
      parseHeader;
      this.pool = new ConstantPool;
      parseClass()
    } catch {
      case e: FatalError => handleError(e)
      case e: RuntimeException => handleError(e)
    }
    busy = false
  }

  private def statics: Symbol = staticModule.moduleClass;

  private def parseHeader: unit = {
    val magic = in.nextInt;
    if (magic != JAVA_MAGIC)
      throw new IOException("class file '" + in.file + "' "
                            + "has wrong magic number 0x" + Integer.toHexString(magic)
                            + ", should be 0x" + Integer.toHexString(JAVA_MAGIC));
    val minorVersion = in.nextChar;
    val majorVersion = in.nextChar;
    if ((majorVersion < JAVA_MAJOR_VERSION) ||
        ((majorVersion == JAVA_MAJOR_VERSION) &&
         (minorVersion < JAVA_MINOR_VERSION)))
      throw new IOException("class file '" + in.file + "' "
                            + "has unknown version "
                            + majorVersion + "." + minorVersion
                            + ", should be at least "
                            + JAVA_MAJOR_VERSION + "." + JAVA_MINOR_VERSION);

  }

  class ConstantPool {
    private val len = in.nextChar;
    private val starts = new Array[int](len);
    private val values = new Array[Object](len);
    private val internalized = new Array[Name](len);
    { var i = 1;
      while (i < starts.length) {
        starts(i) = in.bp;
	i = i + 1;
        in.nextByte match {
          case CONSTANT_UTF8 | CONSTANT_UNICODE =>
            in.skip(in.nextChar);
          case CONSTANT_CLASS | CONSTANT_STRING =>
            in.skip(2);
          case CONSTANT_FIELDREF | CONSTANT_METHODREF | CONSTANT_INTFMETHODREF | CONSTANT_NAMEANDTYPE | CONSTANT_INTEGER | CONSTANT_FLOAT =>
            in.skip(4);
          case CONSTANT_LONG | CONSTANT_DOUBLE =>
            in.skip(8);
            i = i + 1
          case _ =>
            errorBadTag(in.bp - 1);
        }
      }
    }

    def getName(index: int): Name = {
      if (index <= 0 || len <= index) errorBadIndex(index);
      var name = values(index).asInstanceOf[Name];
      if (name == null) {
        val start = starts(index);
        if (in.buf(start) != CONSTANT_UTF8) errorBadTag(start);
        name = newTermName(in.buf, start + 3, in.getChar(start + 1));
        values(index) = name;
      }
      name
    }

    def getExternalName(index: int): Name = {
      if (index <= 0 || len <= index) errorBadIndex(index);
      if (internalized(index) == null) {
        internalized(index) = getName(index).replace('/', '.')
      }
      internalized(index)
    }

    def getClassSymbol(index: int): Symbol = {
      if (index <= 0 || len <= index) errorBadIndex(index);
      var c = values(index).asInstanceOf[Symbol];
      if (c == null) {
        val start = starts(index);
        if (in.buf(start) != CONSTANT_CLASS) errorBadTag(start);
        val name = getExternalName(in.getChar(start + 1));
        if (name.pos('.') == name.length)
          c = definitions.getMember(definitions.EmptyPackageClass, name.toTypeName)
        else
          c = definitions.getClass(name);
        values(index) = c;
      }
      c
    }

    def getType(index: int): Type =
      sigToType(getExternalName(index));

    def getSuperClass(index: int): Symbol =
      if (index == 0) definitions.AnyClass else getClassSymbol(index);

    def getConstant(index: int): Constant = {
      if (index <= 0 || len <= index) errorBadIndex(index);
      var value = values(index);
      if (value == null) {
        val start = starts(index);
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
          case _ =>
            errorBadTag(start);
        }
        values(index) = value;
      }
      value.asInstanceOf[Constant]
    }

    /** Throws an exception signaling a bad constant index. */
    private def errorBadIndex(index: int) =
      throw new RuntimeException("bad constant pool index: " + index);

    /** Throws an exception signaling a bad tag at given address. */
    private def errorBadTag(start: int) =
      throw new RuntimeException("bad constant pool tag " + in.buf(start) + " at byte " + start);
  }

  private def sigToType(name: Name): Type = {
    var index = 0;
    val end = name.length;
    def objToAny(tp: Type): Type =
      if (tp.symbol == definitions.ObjectClass) definitions.AnyClass.tpe
      else tp;
    def paramsigs2types: List[Type] =
      if (name(index) == ')') { index = index + 1; List() }
      else objToAny(sig2type) :: paramsigs2types;
    def sig2type: Type = {
      val tag = name(index); index = index + 1;
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
          val start = index;
          while (name(index) != ';') { index = index + 1 }
	  val end = index;
          index = index + 1;
          definitions.getClass(name.subName(start, end)).tpe
        case ARRAY_TAG =>
          while ('0' <= name(index) && name(index) <= '9') index = index + 1;
          appliedType(definitions.ArrayClass.tpe, List(sig2type))
        case '(' =>
          JavaMethodType(paramsigs2types, sig2type)
      }
    }
    sig2type
  }

  def parseClass(): unit = {
    val jflags = in.nextChar;
    val isAttribute = (jflags & JAVA_ACC_ANNOTATION) != 0;
    var sflags = transFlags(jflags);
    if ((sflags & DEFERRED) != 0) sflags = sflags & ~DEFERRED | ABSTRACT;
    val c = pool.getClassSymbol(in.nextChar);
    if (c != clazz)
      throw new IOException("class file '" + in.file + "' contains wrong " + clazz);
    val superType = if (isAttribute) { in.nextChar; definitions.AttributeClass.tpe }
                    else pool.getSuperClass(in.nextChar).tpe;
    val ifaceCount = in.nextChar;
    val parents = (superType ::
      (for (val i <- List.range(0, ifaceCount))
       yield pool.getSuperClass(in.nextChar).tpe));
    instanceDefs = new Scope();
    staticDefs = new Scope();
    val classInfo = ClassInfoType(parents, instanceDefs, clazz);
    val staticInfo = ClassInfoType(List(), staticDefs, statics);

    val curbp = in.bp;
    skipMembers(); // fields
    skipMembers(); // methods
    parseAttributes(clazz, classInfo);
    if (!isScala) {
      clazz.setFlag(sflags);
      setPrivateWithin(clazz, jflags);
      if (!hasMeta) {
	clazz.setInfo(classInfo);
      }
      statics.setInfo(staticInfo);
      staticModule.setInfo(statics.tpe);
      staticModule.setFlag(JAVA);
      staticModule.moduleClass.setFlag(JAVA);
      in.bp = curbp;
      val fieldCount = in.nextChar;
      for (val i <- Iterator.range(0, fieldCount)) parseField();
      val methodCount = in.nextChar;
      for (val i <- Iterator.range(0, methodCount)) parseMethod();
      if ((instanceDefs.lookup(nme.CONSTRUCTOR) == NoSymbol
           && (sflags & INTERFACE) == 0) ||
          isAttribute)
        {
          //System.out.println("adding constructor to " + clazz);//DEBUG
          val constrParamTypes =
            if (isAttribute) {
              val value = instanceDefs.lookup(nme.value)
              if (value == NoSymbol) List ()
              else List(value.tpe.resultType)
              //System.out.println("" + value + " : " + value.tpe)
            }
            else List()
          instanceDefs.enter(
            clazz.newConstructor(Position.NOPOS)
            .setFlag(clazz.flags & ConstrFlags)
            .setInfo(MethodType(constrParamTypes, clazz.tpe)));
        }
    }
  }

  def parseField(): unit = {
    val jflags = in.nextChar;
    var sflags = transFlags(jflags);
    if ((sflags & FINAL) == 0) sflags = sflags | MUTABLE;
    if ((sflags & PRIVATE) != 0) {
      in.skip(4); skipAttributes();
    } else {
      val name = pool.getName(in.nextChar);
      val info = pool.getType(in.nextChar);
      val sym = getOwner(jflags)
        .newValue(Position.NOPOS, name).setFlag(sflags).setInfo(info);
      setPrivateWithin(sym, jflags);
      parseAttributes(sym, info);
      getScope(jflags).enter(sym);
    }
  }

  def parseMethod(): unit = {
    val jflags = in.nextChar;
    var sflags = transFlags(jflags);
    if ((jflags & JAVA_ACC_BRIDGE) != 0) sflags = sflags | PRIVATE;
    if ((sflags & PRIVATE) != 0) {
      in.skip(4); skipAttributes();
    } else {
      val name = pool.getName(in.nextChar);
      var info = pool.getType(in.nextChar);
      if (name == nme.CONSTRUCTOR)
	info match {
	  case MethodType(formals, restpe) =>
	    assert(restpe.symbol == definitions.UnitClass);
	    info = MethodType(formals, clazz.tpe)
	}
      val sym = getOwner(jflags)
        .newMethod(Position.NOPOS, name).setFlag(sflags).setInfo(info);
      setPrivateWithin(sym, jflags);
      parseAttributes(sym, info);
      getScope(jflags).enter(sym);
    }
  }


  // returns the generic type represented by the signature
  private def polySigToType(sym: Symbol, sig: Name): Type =
    try { polySigToType0(sym, sig) }
    catch {
      case e: Throwable => System.err.println("" + sym + " - " + sig); throw e;
    }
  private def polySigToType0(sym: Symbol, sig: Name): Type = {
    var index = 0;
    val end = sig.length;
    val newTParams = new ListBuffer[Symbol]();
    def objToAny(tp: Type): Type =
      if (tp.symbol == definitions.ObjectClass) definitions.AnyClass.tpe
      else tp;
    def subName(isDelimiter: Char => Boolean): Name = {
      val start = index;
      while (!isDelimiter(sig(index))) { index = index + 1; }
      sig.subName(start, index)
    }
    def typeParams(tparams: Map[Name,Symbol], covariant: Boolean): List[Type] = {
      assert(sig(index) == '<');
      index = index + 1;
      val xs = new ListBuffer[Type]();
      while (sig(index) != '>') {
        sig(index) match {
          case variance @ ('+' | '-' | '*') =>
            index = index + 1;
            val bounds = variance match {
              case '+' => TypeBounds(definitions.AllRefClass.typeConstructor,
                                     sig2type(tparams, covariant));
              case '-' => TypeBounds(sig2type(tparams, covariant),
                                     definitions.AnyRefClass.typeConstructor);
              case '*' => TypeBounds(definitions.AllRefClass.typeConstructor,
                                     definitions.AnyRefClass.typeConstructor)
            }
            val name = fresh.newName("T_" + sym.name);
            val newtparam =
              if (covariant) clazz.newAbstractType(Position.NOPOS, name);
              else {
                val s = sym.newTypeParameter(Position.NOPOS, name);
                newTParams += s;
                s
              }
            newtparam.setInfo(bounds);
            xs += newtparam.tpe
          case _ => xs += sig2type(tparams, covariant);
        }
      }
      index = index + 1;
      xs.toList
    }
    def sig2type(tparams: Map[Name,Symbol], covariant: Boolean): Type = {
      val tag = sig(index); index = index + 1;
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
          var tpe = definitions.getClass(subName(c => ((c == ';') || (c == '<')))).tpe;
          if (sig(index) == '<')
            tpe = appliedType(tpe, typeParams(tparams, covariant));
          index = index + 1;
          tpe
        case ARRAY_TAG =>
          while ('0' <= sig(index) && sig(index) <= '9') index = index + 1;
          appliedType(definitions.ArrayClass.tpe, List(sig2type(tparams, covariant)))
        case '(' =>
          val paramtypes = new ListBuffer[Type]();
          while (sig(index) != ')') {
            paramtypes += objToAny(sig2type(tparams, false));
          }
          index = index + 1;
          val restype = if (sym.isConstructor) {
            assert(sig(index) == 'V');
            index = index + 1;
            clazz.tpe
          } else
            sig2type(tparams, true)
          MethodType(paramtypes.toList, restype)
        case 'T' =>
          val n = subName(';'.==).toTypeName;
          index = index + 1;
          tparams(n).typeConstructor
      }
    }
    var tparams = classTParams;
    if (sig(index) == '<') {
      index = index + 1;
      while (sig(index) != '>') {
        val tpname = subName(':'.==).toTypeName;
        val s = sym.newTypeParameter(Position.NOPOS, tpname);
        tparams = tparams + tpname -> s;
        val ts = new ListBuffer[Type];
        while (sig(index) == ':') {
          index = index + 1;
          if (sig(index) != ':') // guard against empty class bound
            ts += sig2type(tparams, false);
        }
        s.setInfo(TypeBounds(definitions.AllRefClass.typeConstructor,
                             intersectionType(ts.toList, sym)));
        newTParams += s;
      }
      index = index + 1;
    }
    val tpe =
      if (sym.isClass) {
        classTParams = tparams;
        val parents = new ListBuffer[Type]();
        while (index < end) {
          parents += sig2type(tparams, true);  // here the variance doesnt'matter
        }
        ClassInfoType(parents.toList, instanceDefs, sym);
      }
      else
        sig2type(tparams, true);
    if (newTParams.length == 0) tpe
    else PolyType(newTParams.toList, tpe);
  }

  def parseAttributes(sym: Symbol, symtype: Type): unit = {
    def convertTo(c: Constant, pt: Type): Constant = {
      if (pt.symbol == definitions.BooleanClass && c.tag == IntTag)
        Constant(c.value != 0)
      else
        c convertTo pt
    }
    def parseAttribute(): unit = {
      val attrName = pool.getName(in.nextChar);
      val attrLen = in.nextInt;
      attrName match {
        case nme.SignatureATTR =>
          if (global.settings.Xgenerics.value) {
            val sig = pool.getExternalName(in.nextChar);
            val newType = polySigToType(sym, sig);
            sym.setInfo(newType)
            if (settings.debug.value)
              global.inform("" + sym + "; signatire = " + sig + " type = " + newType);
            hasMeta = true;
          } else
            in.skip(attrLen);
        case nme.SyntheticATTR =>
          sym.setFlag(SYNTHETIC);
          in.skip(attrLen)
        case nme.BridgeATTR =>
          sym.setFlag(BRIDGE);
          in.skip(attrLen)
        case nme.DeprecatedATTR =>
          sym.setFlag(DEPRECATED);
          in.skip(attrLen)
        case nme.ConstantValueATTR =>
          val c = pool.getConstant(in.nextChar);
          val c1 = convertTo(c, symtype);
          if (c1 != null) sym.setInfo(ConstantType(c1));
          else System.out.println("failure to convert "+c+" to "+symtype);//debug
        case nme.InnerClassesATTR =>
          parseInnerClasses()
        case nme.ScalaSignatureATTR =>
          unpickler.unpickle(in.buf, in.bp, clazz, staticModule, in.file.toString());
          this.isScala = true;
        case nme.JacoMetaATTR =>
          val meta = pool.getName(in.nextChar).toString().trim();
          metaParser.parse(meta, sym, symtype);
          this.hasMeta = true;
        case nme.SourceFileATTR =>
          assert(attrLen == 2);
          val source = pool.getName(in.nextChar);
          if (sourcePath != null) {
            val sourceFile0 = sourcePath.lookupPath(source.toString(), false);
            if (sourceFile0 != null && clazz.sourceFile == null) {
              clazz.sourceFile = sourceFile0;
            }
            staticModule.moduleClass.sourceFile = clazz.sourceFile
          }
        case nme.RuntimeAnnotationATTR =>
          //parseAnnotations(attrLen);
          in.skip(attrLen)
        case _ =>
          in.skip(attrLen)
      }
    }
    def parseTaggedConstant(): Any = {
      val tag = in.nextByte;
      val index = in.nextChar;
      tag match {
        case STRING_TAG => pool.getName(index).toString();
        case BOOL_TAG   => pool.getConstant(index).intValue != 0;
        case BYTE_TAG   => pool.getConstant(index).byteValue;
        case CHAR_TAG   => pool.getConstant(index).charValue;
        case SHORT_TAG  => pool.getConstant(index).shortValue;
        case INT_TAG    => pool.getConstant(index).intValue
        case LONG_TAG   => pool.getConstant(index).longValue;
        case FLOAT_TAG  => pool.getConstant(index).floatValue;
        case DOUBLE_TAG => pool.getConstant(index).doubleValue;
        case CLASS_TAG  => pool.getType(index).toString() + ".class";
        case ENUM_TAG   =>
          pool.getType(index).toString() + "." + pool.getName(in.nextChar);
        case ARRAY_TAG  =>
          val arr = new ListBuffer[Any]();
          for (val i <- Iterator.range(0, index)) {
            arr += parseTaggedConstant()
          }
        arr.toList.mkString("{", ",", "}");
      }
    }
    def parseAnnotations(len: Int): Unit = {
      val buf = new StringBuffer();
      val nAttr = in.nextChar;
      for (val n <- Iterator.range(0,nAttr)) {
        val attrNameIndex = in.nextChar;
        val attrType = pool.getType(attrNameIndex);
        buf.append("@").append(attrType.toString()).append("(");
        val nargs = in.nextChar
        for (val i <- Iterator.range(0, nargs)) {
          if (i > 0) buf.append(", ");
          val name = pool.getName(in.nextChar);
          buf.append(name).append(" = ");
          val value = parseTaggedConstant();
          buf.append(value);
        }
        buf.append(")");
      }
      global.informProgress("parsed attribute " + buf);
    }
    def parseInnerClasses(): unit = {
      for (val i <- Iterator.range(0, in.nextChar)) {
	val innerIndex = in.nextChar;
	val outerIndex = in.nextChar;
	val nameIndex = in.nextChar;
	val jflags = in.nextChar;
	if (innerIndex != 0 && outerIndex != 0 && nameIndex != 0 &&
	    (jflags & (JAVA_ACC_PUBLIC | JAVA_ACC_PROTECTED)) != 0 &&
	    pool.getClassSymbol(outerIndex) == sym) {
	  val innerAlias = getOwner(jflags)
	    .newAliasType(Position.NOPOS, pool.getName(nameIndex).toTypeName)
	    .setInfo(pool.getClassSymbol(innerIndex).tpe);
	  getScope(jflags).enter(innerAlias);
	}
      }
    }
    val attrCount = in.nextChar;
    for (val i <- Iterator.range(0, attrCount)) parseAttribute()
  }

  def skipAttributes(): unit = {
    val attrCount = in.nextChar;
    for (val i <- Iterator.range(0, attrCount)) {
      in.skip(2); in.skip(in.nextInt)
    }
  }

  def skipMembers(): unit = {
    val memberCount = in.nextChar;
    for (val i <- Iterator.range(0, memberCount)) {
      in.skip(6); skipAttributes()
    }
  }

  private def getOwner(flags: int): Symbol =
    if ((flags & JAVA_ACC_STATIC) != 0) statics else clazz;

  private def getScope(flags: int): Scope =
    if ((flags & JAVA_ACC_STATIC) != 0) staticDefs else instanceDefs;

  private def transFlags(flags: int): long = {
    var res = 0l;
    if ((flags & JAVA_ACC_PRIVATE) != 0)
      res = res | PRIVATE
    else if ((flags & JAVA_ACC_PROTECTED) != 0)
      res = res | PROTECTED
    if ((flags & JAVA_ACC_ABSTRACT) != 0 && (flags & JAVA_ACC_ANNOTATION) == 0)
      res = res | DEFERRED;
    if ((flags & JAVA_ACC_FINAL) != 0)
      res = res | FINAL;
    if (((flags & JAVA_ACC_INTERFACE) != 0) &&
        ((flags & JAVA_ACC_ANNOTATION) == 0))
      res = res | TRAIT | INTERFACE | ABSTRACT;
    if ((flags & JAVA_ACC_SYNTHETIC) != 0)
      res = res | SYNTHETIC;
    if ((flags & JAVA_ACC_STATIC) != 0)
      res = res | STATIC;
    res | JAVA;
  }

  private def setPrivateWithin(sym: Symbol, jflags: int): unit =
    if ((jflags & (JAVA_ACC_PRIVATE | JAVA_ACC_PROTECTED | JAVA_ACC_PUBLIC)) == 0)
      sym.privateWithin = sym.toplevelClass.owner
}
