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

import scala.tools.util._;
import java.io.IOException;

abstract class ClassfileParser {

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

  private object metaParser extends MetaParser {
    val global: ClassfileParser.this.global.type = ClassfileParser.this.global
  }

  def parse(file: AbstractFile, root: Symbol): unit = {
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
      case e: RuntimeException =>
        if (settings.debug.value) e.printStackTrace();
        throw new IOException("class file '" + in.file + "' is broken")
    }
    busy = false
  }

  private def statics: Symbol = staticModule.moduleClass;

  private def parseHeader: unit = {
    val magic = in.nextInt();
    if (magic != JAVA_MAGIC)
      throw new IOException("class file '" + in.file + "' "
                            + "has wrong magic number 0x" + Integer.toHexString(magic)
                            + ", should be 0x" + Integer.toHexString(JAVA_MAGIC));
    val minorVersion = in.nextChar();
    val majorVersion = in.nextChar();
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
    private val len = in.nextChar();
    private val starts = new Array[int](len);
    private val values = new Array[Object](len);
    private val internalized = new Array[Name](len);
    { var i = 1;
      while (i < starts.length) {
        starts(i) = in.bp;
	i = i + 1;
        in.nextByte() match {
          case CONSTANT_UTF8 | CONSTANT_UNICODE =>
            in.skip(in.nextChar());
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
        c = definitions.getClass(name);
        values(index) = c;
      }
      c
    }

    def getType(index: int): Type =
      sigToType(getExternalName(index));

    def getSuperClass(index: int): Symbol =
      if (index == 0) definitions.AnyClass else getClassSymbol(index);

    def getLiteral(index: int): Any = {
      if (index <= 0 || len <= index) errorBadIndex(index);
      var value = values(index);
      if (value == null) {
        val start = starts(index);
        value = in.buf(start) match {
          case CONSTANT_STRING =>
            getName(in.getChar(start + 1)).toString()
          case CONSTANT_INTEGER =>
            in.getInt(start + 1)
          case CONSTANT_FLOAT =>
            in.getFloat(start + 1)
          case CONSTANT_LONG =>
            in.getLong(start + 1)
          case CONSTANT_DOUBLE =>
            in.getDouble(start + 1)
          case _ =>
            errorBadTag(start);
        }
        values(index) = value;
      }
      value
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
        case 'B' => definitions.ByteClass.tpe
        case 'C' => definitions.CharClass.tpe
        case 'D' => definitions.DoubleClass.tpe
        case 'F' => definitions.FloatClass.tpe
        case 'I' => definitions.IntClass.tpe
        case 'J' => definitions.LongClass.tpe
        case 'S' => definitions.ShortClass.tpe
        case 'V' => definitions.UnitClass.tpe
        case 'Z' => definitions.BooleanClass.tpe
        case 'L' =>
          val start = index;
          while (name(index) != ';') { index = index + 1 }
	  val end = index;
          index = index + 1;
          definitions.getClass(name.subName(start, end)).tpe
        case '[' =>
          while ('0' <= name(index) && name(index) <= '9') index = index + 1;
          appliedType(definitions.ArrayClass.tpe, List(sig2type))
        case '(' =>
          MethodType(paramsigs2types, sig2type)
      }
    }
    sig2type
  }

  def parseClass(): unit = {
    val jflags = in.nextChar();
    var sflags = transFlags(jflags);
    if ((sflags & DEFERRED) != 0) sflags = sflags & ~DEFERRED | ABSTRACT;
    val c = pool.getClassSymbol(in.nextChar());
    if (c != clazz)
      throw new IOException("class file '" + in.file + "' contains wrong " + clazz);
    val superType = pool.getSuperClass(in.nextChar()).tpe;
    val ifaceCount = in.nextChar();
    val parents = superType ::
      (for (val i <- List.range(0, ifaceCount))
       yield pool.getSuperClass(in.nextChar()).tpe);
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
      if (!hasMeta) {
	clazz.setInfo(classInfo);
	statics.setInfo(staticInfo);
      }
      staticModule.setInfo(statics.tpe);
      in.bp = curbp;
      val fieldCount = in.nextChar();
      for (val i <- Iterator.range(0, fieldCount)) parseField();
      val methodCount = in.nextChar();
      for (val i <- Iterator.range(0, methodCount)) parseMethod();
      if (instanceDefs.lookup(nme.CONSTRUCTOR) == NoSymbol && (sflags & INTERFACE) == 0) {
        System.out.println("adding constructor to " + clazz);//debug
        instanceDefs enter
          clazz.newConstructor(Position.NOPOS)
            .setFlag(clazz.flags & CONSTRFLAGS).setInfo(MethodType(List(), clazz.tpe));
      }
    }
  }

  def parseField(): unit = {
    val jflags = in.nextChar();
    var sflags = transFlags(jflags);
    if ((sflags & FINAL) != 0) sflags = sflags | MUTABLE;
    if ((sflags & PRIVATE) != 0) {
      in.skip(4); skipAttributes();
    } else {
      val name = pool.getName(in.nextChar());
      val info = pool.getType(in.nextChar());
      val sym = getOwner(jflags)
        .newValue(Position.NOPOS, name).setFlag(sflags).setInfo(info);
      parseAttributes(sym, info);
      getScope(jflags).enter(sym);
    }
  }

  def parseMethod(): unit = {
    val jflags = in.nextChar();
    var sflags = transFlags(jflags);
    if ((sflags & JAVA_ACC_BRIDGE) != 0) sflags = sflags | PRIVATE;
    if ((sflags & PRIVATE) != 0) {
      in.skip(4); skipAttributes();
    } else {
      val name = pool.getName(in.nextChar());
      val info = pool.getType(in.nextChar());
      val sym = getOwner(jflags)
        .newMethod(Position.NOPOS, name).setFlag(sflags).setInfo(info);
      parseAttributes(sym, info);
      getScope(jflags).enter(sym);
    }
  }

  def parseAttributes(sym: Symbol, symtype: Type): unit = {
    def parseAttribute(): unit = {
      val attrName = pool.getName(in.nextChar());
      val attrLen = in.nextInt();
      attrName match {
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
	  def cast(value: Any, tp: Type): Any = {
	    val s = symtype.symbol;
	    if (s == definitions.ByteClass) value.asInstanceOf[byte]
	    else if (s == definitions.ShortClass) value.asInstanceOf[short]
	    else if (s == definitions.CharClass) value.asInstanceOf[char]
	    else if (s == definitions.IntClass) value.asInstanceOf[int]
	    else if (s == definitions.LongClass) value.asInstanceOf[long]
	    else if (s == definitions.FloatClass) value.asInstanceOf[float]
	    else if (s == definitions.DoubleClass) value.asInstanceOf[double]
	    else if (s == definitions.UnitClass) ()
	    else if (s == definitions.BooleanClass) value.asInstanceOf[int] != 0
	    else if (s == definitions.StringClass) value.asInstanceOf[String]
	    else value
	  }
          val value = pool.getLiteral(in.nextChar());
          sym.setInfo(ConstantType(symtype, cast(value, symtype)))
        case nme.InnerClassesATTR =>
          parseInnerClasses()
        case nme.ScalaSignatureATTR =>
          //unpickle.parse(in.nextBytes(attrLen), clazz, statics.sourceModule);
	  //this.isScala = true;
        case nme.JacoMetaATTR =>
          val meta = pool.getName(in.nextChar()).toString().trim();
          metaParser.parse(meta, sym, symtype);
	  this.hasMeta = true;
        case _ =>
          in.skip(attrLen)
      }
    }
    def parseInnerClasses(): unit = {
      for (val i <- Iterator.range(0, in.nextChar())) {
	val innerIndex = in.nextChar();
	val outerIndex = in.nextChar();
	val nameIndex = in.nextChar();
	val jflags = in.nextChar();
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
    val attrCount = in.nextChar();
    for (val i <- Iterator.range(0, attrCount)) parseAttribute()
  }

  def skipAttributes(): unit = {
    val attrCount = in.nextChar();
    for (val i <- Iterator.range(0, attrCount)) {
      in.skip(2); in.skip(in.nextInt())
    }
  }

  def skipMembers(): unit = {
    val memberCount = in.nextChar();
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
    else if ((flags & JAVA_ACC_PUBLIC) == 0)
      res = res | PRIVATE;
    if ((flags & JAVA_ACC_ABSTRACT) != 0)
      res = res | DEFERRED;
    if ((flags & JAVA_ACC_FINAL) != 0)
      res = res | FINAL;
    if ((flags & JAVA_ACC_INTERFACE) != 0)
      res = res | TRAIT | INTERFACE | ABSTRACT;
    if ((flags & JAVA_ACC_SYNTHETIC) != 0)
      res = res | SYNTHETIC;
    res | JAVA;
  }
}
