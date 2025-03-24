/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package tools.nsc
package symtab
package classfile

import java.io.IOException
import java.lang.Integer.toHexString

import scala.annotation._
import scala.collection.{immutable, mutable}, mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.internal.JavaAccFlags
import scala.reflect.internal.pickling.ByteCodecs
import scala.reflect.internal.util.ReusableInstance
import scala.reflect.io.NoAbstractFile
import scala.tools.nsc.Reporting.WarningCategory
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.tasty.{TastyUniverse, TastyUnpickler}
import scala.util.control.NonFatal

/** This abstract class implements a class file parser.
 *
 *  @author Martin Odersky
 */
abstract class ClassfileParser(reader: ReusableInstance[ReusableDataReader]) {
  import ClassfileParser._

  val symbolTable: SymbolTable {
    def settings: Settings
  }
  val loaders: SymbolLoaders {
    val symbolTable: ClassfileParser.this.symbolTable.type
  }

  import symbolTable._
  /**
   * If typer phase is defined then perform member lookup of a symbol
   * `sym` at typer phase. This method results from refactoring. The
   * original author of the logic that uses typer phase didn't explain
   * why we need to force infos at that phase specifically. It only mentioned
   * that ClassfileParse can be called late (e.g. at flatten phase) and
   * we make to make sure we handle such situation properly.
   */
  protected def lookupMemberAtTyperPhaseIfPossible(sym: Symbol, name: Name): Symbol

  /** The compiler classpath. */
  def classPath: ClassPath

  import definitions._
  import scala.reflect.internal.ClassfileConstants._
  import Flags._

  protected var file: AbstractFile = _         // the class file
  protected var in: DataReader = _             // the class file reader
  protected var clazz: ClassSymbol = _         // the class symbol containing dynamic members
  protected var staticModule: ModuleSymbol = _ // the module symbol containing static members
  protected var instanceScope: Scope = _       // the scope of all instance definitions
  protected var staticScope: Scope = _         // the scope of all static definitions
  protected var pool: ConstantPool = _         // the classfile's constant pool
  protected var isScala: Boolean = _           // does class file describe a scala class?
  protected var isScalaRaw: Boolean = _        // this class file is a scala class with no pickled info
  protected var busy: Symbol = _               // lock to detect recursive reads
  protected var currentClass: String = _       // JVM name of the current class
  protected var classTParams = Map[Name,Symbol]()
  protected var srcfile0 : Option[AbstractFile] = None
  protected def moduleClass: Symbol = staticModule.moduleClass
  private var YtastyReader          = false

  private def ownerForFlags(jflags: JavaAccFlags) = if (jflags.isStatic) moduleClass else clazz

  def srcfile = srcfile0

  // u1, u2, and u4 are what these data types are called in the JVM spec.
  // They are an unsigned byte, unsigned char, and unsigned int respectively.
  // We bitmask u1 into an Int to make sure it's 0-255 (and u1 isn't used
  // for much beyond tags) but leave u2 alone as it's already unsigned.
  protected final def u1(): Int = in.nextByte & 0xFF
  protected final def u2(): Int = in.nextChar.toInt
  protected final def u4(): Int = in.nextInt

  protected final def s1(): Int = in.nextByte.toInt // sign-extend the byte to int
  protected final def s2(): Int = (in.nextByte.toInt << 8) | u1() // sign-extend and shift the first byte, or with the unsigned second byte

  private def readInnerClassFlags() = readClassFlags()
  private def readClassFlags()      = JavaAccFlags classFlags u2()
  private def readMethodFlags()     = JavaAccFlags methodFlags u2()
  private def readFieldFlags()      = JavaAccFlags fieldFlags u2()
  private def readTypeName()        = readName().toTypeName
  private def readName()            = pool.getName(u2()).name
  @annotation.unused
  private def readType()            = pool getType u2()

  private object unpickler extends scala.reflect.internal.pickling.UnPickler {
    val symbolTable: ClassfileParser.this.symbolTable.type = ClassfileParser.this.symbolTable
  }

  object TastyUniverse extends TastyUniverse {
    type SymbolTable = ClassfileParser.this.symbolTable.type
    val symbolTable: SymbolTable = ClassfileParser.this.symbolTable
  }

  private def handleMissing(e: MissingRequirementError) = {
    if (settings.isDebug) e.printStackTrace
    throw new IOException(s"Missing dependency '${e.req}', required by $file")
  }
  private def handleError(e: Exception) = {
    if (settings.isDebug) e.printStackTrace()
    throw new IOException(s"class file '$file' is broken\n(${e.getClass}/${e.getMessage})")
  }
  private def mismatchError(c: Symbol) = {
    throw new IOException(s"class file '$file' has location not matching its contents: contains $c")
  }

  private def parseErrorHandler[T]: PartialFunction[Throwable, T] = {
    case e: MissingRequirementError => handleMissing(e)
    case e: RuntimeException        => handleError(e)
  }
  @inline private def pushBusy[T](sym: Symbol)(body: => T): T = {
    if (busy eq sym)
      throw new IOException(s"unsatisfiable cyclic dependency in '$sym'")
    else if ((busy ne null) && (busy ne NoSymbol))
      throw new IOException(s"illegal class file dependency between '$sym' and '$busy'")

    busy = sym
    try body
    catch parseErrorHandler
    finally busy = NoSymbol
  }

  /**
   * `clazz` and `module` are the class and module symbols corresponding to the classfile being
   * parsed. Note that the ClassfileLoader unconditionally creates both of these symbols, they may
   * may get invalidated later on (.exists).
   *
   * Note that using `companionModule` / `companionClass` does not always work to navigate between
   * those two symbols, namely when they are shadowed by a type / value in the a package object
   * (scala-dev#248).
   */
  def parse(file: AbstractFile, clazz: ClassSymbol, module: ModuleSymbol): Unit = {
    this.file = file
    pushBusy(clazz) {
      reader.using { reader =>
        this.in           = reader.reset(file)
        this.clazz        = clazz
        this.staticModule = module
        this.isScala      = false
        this.YtastyReader = settings.YtastyReader.value

        val isJavaMagic = in.getInt(in.bp) == JAVA_MAGIC
        if (!isJavaMagic && file.name.endsWith(".sig")) {
          currentClass = clazz.javaClassName
          isScala = true
          unpickler.unpickle(in.buf.take(file.sizeOption.get), 0, clazz, staticModule, file.name)
        } else if (!isJavaMagic && file.name.endsWith(".tasty")) {
          if (!YtastyReader)
            MissingRequirementError.signal(s"Add -Ytasty-reader to scalac options to parse the TASTy in $file")

          // TODO [tasty]: it seems tests don't fail if we remove this, but previously this
          // was added for the following reason:
          // >  Force scala.AnyRef, otherwise we get "error: Symbol AnyRef is missing from the classpath"
          AnyRefClass

          val bytes = in.buf.take(file.sizeOption.get)
          TastyUnpickler.unpickle(TastyUniverse)(bytes, clazz, staticModule, file.path)
        } else {
          parseHeader()
          this.pool = new ConstantPool
          parseClass()
          pool = null
        }
      }
      in = null
    }
  }

  private def parseHeader(): Unit = {
    val magic = u4()
    if (magic != JAVA_MAGIC)
      abort(s"class file ${file} has wrong magic number 0x${toHexString(magic)}")

    val minor, major = u2()
    if (major < JAVA_MAJOR_VERSION || major == JAVA_MAJOR_VERSION && minor < JAVA_MINOR_VERSION)
      abort(s"class file ${file} has unknown version $major.$minor, should be at least $JAVA_MAJOR_VERSION.$JAVA_MINOR_VERSION")
  }

  protected class NameOrString(val value: String) {
    private var _name: Name = null
    def name: Name = {
      if (_name eq null) _name = TermName(value)
      _name
    }
  }

  def getClassSymbol(name: String): Symbol =
    name match {
      case name if name.endsWith(nme.MODULE_SUFFIX_STRING) => rootMirror.getModuleByName(name.stripSuffix(nme.MODULE_SUFFIX_STRING))
      case name                                            => classNameToSymbol(name)
    }

  /**
   * Constructor of this class should not be called directly, use `newConstantPool` instead.
   */
  protected class ConstantPool {
    protected val len          = u2()
    protected val starts       = new Array[Int](len)
    protected val values       = new Array[AnyRef](len)
    protected val internalized = new Array[NameOrString](len)

    val initBp = in.bp

    { var i = 1
      while (i < starts.length) {
        starts(i) = in.bp
        i += 1
        (u1(): @switch) match {
          case CONSTANT_UTF8 | CONSTANT_UNICODE                                => in skip u2()
          case CONSTANT_CLASS | CONSTANT_STRING | CONSTANT_METHODTYPE          => in skip 2
          case CONSTANT_MODULE | CONSTANT_PACKAGE                              => in skip 2
          case CONSTANT_METHODHANDLE                                           => in skip 3
          case CONSTANT_FIELDREF | CONSTANT_METHODREF | CONSTANT_INTFMETHODREF => in skip 4
          case CONSTANT_NAMEANDTYPE | CONSTANT_INTEGER | CONSTANT_FLOAT        => in skip 4
          case CONSTANT_DYNAMIC | CONSTANT_INVOKEDYNAMIC                       => in skip 4
          case CONSTANT_LONG | CONSTANT_DOUBLE                                 => in skip 8 ; i += 1
          case _                                                               => errorBadTag(in.bp - 1)
        }
      }
    }
    val endBp = in.bp
    def recordAtIndex[T <: AnyRef](value: T, idx: Int): T = {
      values(idx) = value
      value
    }

    def firstExpecting(index: Int, expected: Int): Int = {
      val start = starts(index)
      val first = in.getByte(start).toInt
      if (first == expected) start + 1
      else this errorBadTag start
    }

    /** Return the name found at given index. */
    def getName(index: Int): NameOrString = (
      if (index <= 0 || len <= index) errorBadIndex(index)
      else values(index) match {
        case name: NameOrString => name
        case _          =>
          val start = firstExpecting(index, CONSTANT_UTF8)
          val len   = in.getChar(start).toInt
          recordAtIndex(new NameOrString(in.getUTF(start, len + 2)), index)
      }
    )

    /** Return the name found at given index in the constant pool, with '/' replaced by '.'. */
    def getExternalName(index: Int): NameOrString = {
      if (index <= 0 || len <= index)
        errorBadIndex(index)

      if (internalized(index) == null)
        internalized(index) = new NameOrString(getName(index).value.replace('/', '.'))

      internalized(index)
    }

    def getClassSymbol(index: Int): Symbol = {
      if (index <= 0 || len <= index) errorBadIndex(index)
      values(index) match {
        case sym: Symbol => sym
        case _           =>
          val result = ClassfileParser.this.getClassSymbol(getClassName(index).value)
          recordAtIndex(result, index)
      }
    }

    /** Return the external name of the class info structure found at 'index'.
     *  Use 'getClassSymbol' if the class is sure to be a top-level class.
     */
    def getClassName(index: Int): NameOrString = {
      val start = firstExpecting(index, CONSTANT_CLASS)
      getExternalName((in.getChar(start)).toInt)
    }

    /** Return a name and a type at the given index. If the type is a method
     *  type, a dummy symbol is created in `ownerTpe`, which is used as the
     *  owner of its value parameters. This might lead to inconsistencies,
     *  if a symbol of the given name already exists, and has a different
     *  type.
     */
    protected def getNameAndType(index: Int, ownerTpe: Type): (Name, Type) = {
      if (index <= 0 || len <= index) errorBadIndex(index)
      values(index) match {
        case p: ((Name @unchecked, Type @unchecked)) => p
        case _                                       =>
          val start = firstExpecting(index, CONSTANT_NAMEANDTYPE)
          val name = getName(in.getChar(start).toInt)
          // create a dummy symbol for method types
          val dummy = ownerTpe.typeSymbol.newMethod(name.name.toTermName, ownerTpe.typeSymbol.pos)
          val tpe   = getType(dummy, in.getChar(start + 2).toInt)
          // fix the return type, which is blindly set to the class currently parsed
          val restpe = tpe match {
            case MethodType(formals, _) if name.name == nme.CONSTRUCTOR => MethodType(formals, ownerTpe)
            case _                                                      => tpe
          }
          ((name.name, restpe))
      }
    }

    /** Return the type of a class constant entry. Since
     *  arrays are considered to be class types, they might
     *  appear as entries in 'newarray' or 'cast' opcodes.
     */
    def getClassOrArrayType(index: Int): Type = {
      if (index <= 0 || len <= index) errorBadIndex(index)
      else values(index) match {
        case tp: Type    => tp
        case cls: Symbol => cls.tpe_*
        case _           =>
          val name = getClassName(index)
          name.value.charAt(0) match {
            case ARRAY_TAG => recordAtIndex(sigToType(null, name.value), index)
            case _         => recordAtIndex(classNameToSymbol(name.value), index).tpe_*
          }
      }
    }

    def getType(index: Int): Type              = getType(null, index)
    def getType(sym: Symbol, index: Int): Type = sigToType(sym, getExternalName(index).value)
    def getSuperClassName(index: Int): NameOrString = if (index == 0) null else getClassName(index) // the only classfile that is allowed to have `0` in the super_class is java/lang/Object (see jvm spec)

    private def createConstant(index: Int): Constant = {
      val start = starts(index)
      Constant((in.getByte(start).toInt: @switch) match {
        case CONSTANT_STRING  => getName(in.getChar(start + 1).toInt).value
        case CONSTANT_INTEGER => in.getInt(start + 1)
        case CONSTANT_FLOAT   => in.getFloat(start + 1)
        case CONSTANT_LONG    => in.getLong(start + 1)
        case CONSTANT_DOUBLE  => in.getDouble(start + 1)
        case CONSTANT_CLASS   => getClassOrArrayType(index).typeSymbol.tpe_* // !!! Is this necessary or desirable?
        case _                => errorBadTag(start)
      })
    }
    def getConstant(index: Char): Constant = getConstant(index.toInt)
    def getConstant(index: Int): Constant = (
      if (index <= 0 || len <= index) errorBadIndex(index)
      else values(index) match {
        case  const: Constant => const
        case sym: Symbol      => Constant(sym.tpe_*)
        case tpe: Type        => Constant(tpe)
        case _                => recordAtIndex(createConstant(index), index)
      }
    )

    private def getSubArray(bytes: Array[Byte]): Array[Byte] = {
      val decodedLength = ByteCodecs.decode(bytes)
      val arr           = new Array[Byte](decodedLength)
      System.arraycopy(bytes, 0, arr, 0, decodedLength)
      arr
    }

    /**
     * Get an array of bytes stored in the classfile as a string. The data is encoded in the format
     * described in object [[scala.reflect.internal.pickling.ByteCodecs]]. Used for the ScalaSignature annotation argument.
     */
    def getBytes(index: Int): Array[Byte] = {
      if (index <= 0 || len <= index) errorBadIndex(index)
      else values(index) match {
        case xs: Array[Byte] => xs
        case _ =>
          val start = firstExpecting(index, CONSTANT_UTF8)
          val len = (in getChar start).toInt
          val bytes = new Array[Byte](len)
          in.getBytes(start + 2, bytes)
          recordAtIndex(getSubArray(bytes), index)
      }
    }

    /**
     * Get an array of bytes stored in the classfile as an array of strings. The data is encoded in
     * the format described in object [[scala.reflect.internal.pickling.ByteCodecs]]. Used for the ScalaLongSignature annotation
     * argument.
     */
    def getBytes(indices: List[Int]): Array[Byte] = {
      val head = indices.head
      values(head) match {
        case xs: Array[Byte] => xs
        case _               =>
          val arr: Array[Byte] = indices.toArray flatMap { index =>
            if (index <= 0 || ConstantPool.this.len <= index) errorBadIndex(index)
            val start = firstExpecting(index, CONSTANT_UTF8)
            val len   = (in getChar start).toInt
            val s     = start + 2
            val result = new Array[Byte](len)
            in.getBytes(s, result)
            result
          }
          recordAtIndex(getSubArray(arr), head)
      }
    }

    /** Throws an exception signaling a bad constant index. */
    protected def errorBadIndex(index: Int) =
      abort(s"bad constant pool index: $index at pos: ${in.bp}")

    /** Throws an exception signaling a bad tag at given address. */
    protected def errorBadTag(start: Int) =
      abort(s"bad constant pool tag ${in.getByte(start)} at byte $start")
  }

  def stubClassSymbol(name: Name): Symbol = {
    // scala/bug#5593 Scaladoc's current strategy is to visit all packages in search of user code that can be documented
    // therefore, it will rummage through the classpath triggering errors whenever it encounters package objects
    // that are not in their correct place (see bug for details)

    // TODO More consistency with use of stub symbols in `Unpickler`
    //   - better owner than `NoSymbol`
    //   - remove eager warning
    val msg = s"Class $name not found - continuing with a stub."
    if ((!settings.isScaladoc) && (settings.verbose.value || settings.isDeveloper))
      loaders.warning(NoPosition, msg, WarningCategory.OtherDebug, clazz.fullNameString)
    NoSymbol.newStubSymbol(name.toTypeName, msg)
  }

  private def lookupClass(name: String) = try {
    def lookupTopLevel = {
      if (name contains '.')
        rootMirror getClassByName name
      else
      // FIXME - we shouldn't be doing ad hoc lookups in the empty package, getClassByName should return the class
        definitions.getMember(rootMirror.EmptyPackageClass, newTypeName(name))
    }

    // For inner classes we usually don't get here: `classNameToSymbol` already returns the symbol
    // of the inner class based on the InnerClass table. However, if the classfile is missing the
    // InnerClass entry for `name`, it might still be that there exists an inner symbol (because
    // some other classfile _does_ have an InnerClass entry for `name`). In this case, we want to
    // return the actual inner symbol (C.D, with owner C), not the top-level symbol C$D. This is
    // what the logic below is for (see PR #5822 / scala/bug#9937).
    val split = if (isScalaRaw) -1 else name.lastIndexOf('$')
    if (split > 0 && split < name.length) {
      val outerName = name.substring(0, split)
      val innerName = name.substring(split + 1, name.length)
      val outerSym = classNameToSymbol(outerName)

      // If the outer class C cannot be found, look for a top-level class C$D
      if (outerSym.isInstanceOf[StubSymbol]) lookupTopLevel
      else {
        val innerNameAsName = newTypeName(innerName)

        // We have a java-defined class name C$D and look for a member D of C. But we don't know if
        // D is declared static or not, so we have to search both in class C and its companion.
        val r = if (outerSym == clazz)
          staticScope.lookup(innerNameAsName) orElse
            instanceScope.lookup(innerNameAsName)
        else
          lookupMemberAtTyperPhaseIfPossible(outerSym, innerNameAsName) orElse
            lookupMemberAtTyperPhaseIfPossible(outerSym.companionModule, innerNameAsName)
        r orElse lookupTopLevel
      }
    } else
      lookupTopLevel
  } catch {
    // The handler
    //   - prevents crashes with deficient InnerClassAttributes (scala/bug#2464, 0ce0ad5)
    //   - was referenced in the bugfix commit for scala/bug#3756 (4fb0d53), not sure why
    //   - covers the case when a type alias in a package object shadows a class symbol,
    //     getClassByName throws a MissingRequirementError (scala-dev#248)
    case ex: FatalError =>
      // getClassByName can throw a MissingRequirementError (which extends FatalError)
      // definitions.getMember can throw a FatalError, for example in pos/t5165b
      if (settings.isDebug)
        ex.printStackTrace()
      stubClassSymbol(newTypeName(name))
  }

  /** Return the class symbol of the given name. */
  def classNameToSymbol(name: String): Symbol = {
    if (innerClasses contains name)
      innerClasses innerSymbol name
    else
      lookupClass(name)
  }

  def parseClass(): Unit = {
    unpickleOrParseInnerClasses()

    val jflags = readClassFlags()
    val classNameIndex = u2()
    currentClass = pool.getClassName(classNameIndex).value

    // Ensure that (top-level) classfiles are in the correct directory
    val isTopLevel = !(currentClass contains '$') // Java class name; *don't* try to to use Scala name decoding (scala/bug#7532)
    if (isTopLevel) {
      val c = pool.getClassSymbol(classNameIndex)
      // scala-dev#248: when a type alias (in a package object) shadows a class symbol, getClassSymbol returns a stub
      // TODO: this also prevents the error when it would be useful (`mv a/C.class .`)
      if (!c.isInstanceOf[StubSymbol] && c != clazz) mismatchError(c)
    }

    if (isScala) {
      () // We're done
    } else if (isScalaRaw) {
      val decls = clazz.enclosingPackage.info.decls
      for (c <- List(clazz, staticModule, staticModule.moduleClass)) {
        c.setInfo(NoType)
        decls.unlink(c)
      }
    } else {
      val sflags = jflags.toScalaFlags // includes JAVA

      addEnclosingTParams(clazz)

      // Create scopes before calling `enterOwnInnerClasses`
      instanceScope = newScope
      staticScope = newScope
      val staticInfo = ClassInfoType(List(), staticScope, moduleClass)

      val parentIndex = u2()
      val parentName = if (parentIndex == 0) null else pool.getClassName(parentIndex)
      val ifaceCount = u2()
      val ifaces = for (_ <- List.range(0, ifaceCount)) yield pool.getSuperClassName(index = u2())
      val completer = new ClassTypeCompleter(clazz.name, jflags, parentName, ifaces)

      enterOwnInnerClasses()

      clazz setInfo completer
      clazz setFlag sflags
      moduleClass setInfo staticInfo
      moduleClass setFlag JAVA
      staticModule setInfo moduleClass.tpe
      staticModule setFlag JAVA

      propagatePackageBoundary(jflags, clazz, staticModule, moduleClass)

      val fieldsStartBp = in.bp
      skipMembers() // fields
      skipMembers() // methods

      parseAttributes(clazz, completer)

      in.bp = fieldsStartBp
      u2() times parseField()
      u2() times parseMethod()
      val needsConstructor = (sflags & JAVA_ANNOTATION) != 0L
      if (needsConstructor)
        instanceScope enter clazz.newClassConstructor(NoPosition)

      // we could avoid this if we eagerly created class type param symbols here to expose through the
      // ClassTypeCompleter to satisfy the calls to rawInfo.typeParams from Symbol.typeParams. That would
      // require a refactor of `sigToType`.
      //
      // We would also need to make sure that clazzTParams is populated before member type completers called sig2type.
      clazz.initialize
    }
  }

  /** Add type parameters of enclosing classes */
  def addEnclosingTParams(clazz: Symbol): Unit = {
    var sym = clazz.owner
    while (sym.isClass && !sym.isModuleClass) {
      for (t <- sym.tpe.typeArgs)
        classTParams += (t.typeSymbol.name -> t.typeSymbol)

      sym = sym.owner
    }
  }

  def parseField(): Unit = {
    val jflags = readFieldFlags()
    val sflags = jflags.toScalaFlags

    if ((sflags & PRIVATE) != 0L) {
      in.skip(4); skipAttributes()
    } else {
      val name     = readName()
      val lazyInfo = new MemberTypeCompleter(name, jflags, pool.getExternalName(u2()).value)
      val sym      = ownerForFlags(jflags).newValue(name.toTermName, NoPosition, sflags)

      // Note: the info may be overwritten later with a generic signature
      // parsed from SignatureATTR
      sym setInfo {
        if (jflags.isEnum) ConstantType(Constant(sym))
        else lazyInfo
      }
      propagatePackageBoundary(jflags, sym)
      parseAttributes(sym, lazyInfo)
      addJavaFlagsAnnotations(sym, jflags)
      getScope(jflags) enter sym

      // sealed java enums
      if (jflags.isEnum)
        sym.owner.linkedClassOfClass match {
          case NoSymbol =>
            devWarning(s"no linked class for java enum $sym in ${sym.owner}. A referencing class file might be missing an InnerClasses entry.")
          case enumClass =>
            enumClass.addChild(sym)
        }
    }
  }

  def parseMethod(): Unit = {
    val jflags = readMethodFlags()
    if (jflags.isPrivate) {
      in.skip(4); skipAttributes()
    } else {
      val name = readName()
      val sflags = jflags.toScalaFlags
      val sym = ownerForFlags(jflags).newMethod(name.toTermName, NoPosition, sflags)
      // Note: the info may be overwritten later with a generic signature
      // parsed from SignatureATTR
      val lazyInfo = new MemberTypeCompleter(name, jflags, pool.getExternalName(u2()).value)
      sym.info = lazyInfo
      propagatePackageBoundary(jflags, sym)
      parseAttributes(sym, lazyInfo)
      addJavaFlagsAnnotations(sym, jflags)
      getScope(jflags) enter sym
    }
  }

  private def sigToType(sym: Symbol, sig: String): Type = {
    val sigChars = sig.toCharArray
    var index = 0
    val end = sig.length
    def accept(ch: Char): Unit = {
      assert(sig.charAt(index) == ch, (sig.charAt(index), ch))
      index += 1
    }
    def subName(isDelimiter: Char => Boolean): String = {
      val start = index
      while (!isDelimiter(sig.charAt(index))) { index += 1 }
      new String(sigChars, start, index - start)
    }
    def sig2type(tparams: immutable.Map[Name,Symbol], skiptvs: Boolean): Type = {
      val tag = sig.charAt(index); index += 1
      tag match {
        case BYTE_TAG   => ByteTpe
        case CHAR_TAG   => CharTpe
        case DOUBLE_TAG => DoubleTpe
        case FLOAT_TAG  => FloatTpe
        case INT_TAG    => IntTpe
        case LONG_TAG   => LongTpe
        case SHORT_TAG  => ShortTpe
        case VOID_TAG   => UnitTpe
        case BOOL_TAG   => BooleanTpe
        case 'L' =>
          def processInner(tp: Type): Type = tp match {
            case TypeRef(pre, sym, args) if !sym.isStatic => typeRef(processInner(pre.widen), sym, args)
            case _ => tp
          }
          def processClassType(tp: Type): Type = tp match {
            case TypeRef(pre, classSym, args) =>
              val existentials = new ListBuffer[Symbol]()
              if (sig.charAt(index) == '<') {
                accept('<')
                val xs = new ListBuffer[Type]()
                var i = 0
                while (sig.charAt(index) != '>') {
                  sig.charAt(index) match {
                    case variance @ ('+' | '-' | '*') =>
                      index += 1
                      val bounds = variance match {
                        case '+' => TypeBounds.upper(sig2type(tparams, skiptvs))
                        case '-' =>
                          val tp = sig2type(tparams, skiptvs)
                          // Interpret `sig2type` returning `Any` as "no bounds";
                          // morally equivalent to TypeBounds.empty, but we're representing Java code, so use ObjectTpeJava for AnyTpe.
                          if (tp.typeSymbol == AnyClass) TypeBounds.upper(definitions.ObjectTpeJava)
                          else TypeBounds(tp, definitions.ObjectTpeJava)
                        case '*' => TypeBounds.upper(definitions.ObjectTpeJava)
                      }
                      val newtparam = sym.newExistential(newTypeName("?"+i), sym.pos) setInfo bounds
                      existentials += newtparam
                      xs += newtparam.tpeHK
                      i += 1
                    case _ =>
                      xs += sig2type(tparams, skiptvs)
                  }
                }
                accept('>')
                assert(xs.length > 0, tp)
                debuglogResult("new existential")(newExistentialType(existentials.toList, typeRef(pre, classSym, xs.toList)))
              }
              // isMonomorphicType is false if the info is incomplete, as it usually is here
              // so have to check unsafeTypeParams.isEmpty before worrying about raw type case below,
              // or we'll create a boatload of needless existentials.
              else if (classSym.isMonomorphicType || classSym.unsafeTypeParams.isEmpty) tp
              else debuglogResult(s"raw type from $classSym") {
                // raw type - existentially quantify all type parameters
                classExistentialType(pre, classSym)
              }
            case tp =>
              assert(sig.charAt(index) != '<', s"sig=$sig, index=$index, tp=$tp")
              tp
          }

          val classTpe = {
            val classSym = classNameToSymbol(subName(c => c == ';' || c == '<'))
            assert(!classSym.isOverloaded, classSym.alternatives)
            if (classSym eq ObjectClass) ObjectTpeJava else classSym.tpe_*
          }
          var tpe = processClassType(processInner(classTpe))
          while (sig.charAt(index) == '.') {
            accept('.')
            val name = newTypeName(subName(c => c == ';' || c == '<' || c == '.'))
            val member = if (tpe.typeSymbol == clazz) instanceScope.lookup(name) else tpe.member(name)
            val dummyArgs = Nil // the actual arguments are added in processClassType
            val inner = typeRef(pre = tpe, sym = member, args = dummyArgs)
            tpe = processClassType(inner)
          }
          accept(';')
          tpe
        case ARRAY_TAG =>
          while ('0' <= sig.charAt(index) && sig.charAt(index) <= '9') index += 1
          var elemtp = sig2type(tparams, skiptvs)
          // make unbounded Array[T] where T is a type variable into Array[T with Object]
          // (this is necessary because such arrays have a representation which is incompatible
          // with arrays of primitive types.
          // see also RestrictJavaArraysMap (when compiling java sources directly)
          if (elemtp.typeSymbol.isAbstractType && elemtp.upperBound =:= ObjectTpe) {
            elemtp = intersectionType(List(elemtp, ObjectTpe))
          }

          arrayType(elemtp)
        case '(' =>
          // we need a method symbol. given in line 486 by calling getType(methodSym, ..)
          assert(sym ne null, sig)
          val paramtypes = new ListBuffer[Type]()
          while (sig.charAt(index) != ')') {
            paramtypes += sig2type(tparams, skiptvs)
          }
          index += 1
          val restype = if (sym != null && sym.isClassConstructor) {
            accept('V')
            clazz.tpe_*
          } else
            sig2type(tparams, skiptvs)
          MethodType(sym.newSyntheticValueParams(paramtypes.toList), restype)
        case 'T' =>
          val n = newTypeName(subName(';'.==))
          index += 1
          if (skiptvs) AnyTpe
          else tparams(n).typeConstructor
      }
    } // sig2type(tparams, skiptvs)

    def sig2typeBounds(tparams: immutable.Map[Name, Symbol], skiptvs: Boolean): Type = {
      val ts = new ListBuffer[Type]
      while (sig.charAt(index) == ':') {
        index += 1
        if (sig.charAt(index) != ':') // guard against empty class bound
          ts += sig2type(tparams, skiptvs)
      }
      TypeBounds.upper(intersectionType(ts.toList, sym))
    }

    var tparams = classTParams
    val newTParams = new ListBuffer[Symbol]()
    if (sig.charAt(index) == '<') {
      assert(sym != null, sig)
      index += 1
      val start = index
      while (sig.charAt(index) != '>') {
        val tpname = newTypeName(subName(':'.==))
        val s = sym.newTypeParameter(tpname)
        tparams = tparams + (tpname -> s)
        sig2typeBounds(tparams, skiptvs = true)
        newTParams += s
      }
      index = start
      while (sig.charAt(index) != '>') {
        val tpname = newTypeName(subName(':'.==))
        val s = tparams(tpname)
        s.setInfo(sig2typeBounds(tparams, skiptvs = false))
      }
      accept('>')
    }
    val ownTypeParams = newTParams.toList
    if (!ownTypeParams.isEmpty)
      sym.setInfo(new TypeParamsType(ownTypeParams))
    val tpe =
      if ((sym eq null) || !sym.isClass)
        sig2type(tparams, skiptvs = false)
      else {
        classTParams = tparams
        val parents = new ListBuffer[Type]()
        while (index < end) {
          val parent = sig2type(tparams, skiptvs = false) // here the variance doesn't matter
          parents += (if (parent == ObjectTpeJava) ObjectTpe else parent)
        }
        ClassInfoType(parents.toList, instanceScope, sym)
      }
    GenPolyType(ownTypeParams, tpe)
  } // sigToType

  /**
   * Only invoked for java classfiles.
   */
  private def parseAttributes(sym: symbolTable.Symbol, completer: JavaTypeCompleter): Unit = {
    def parseAttribute(): Unit = {
      val attrName = readTypeName()
      val attrLen  = u4()
      attrName match {
        case tpnme.SignatureATTR =>
          val sigIndex = u2()
          val sig = pool.getExternalName(sigIndex)
          assert(sym.rawInfo == completer, sym)
          completer.sig = sig.value
        case tpnme.SyntheticATTR =>
          sym.setFlag(SYNTHETIC | ARTIFACT)
          in.skip(attrLen)

        case tpnme.BridgeATTR =>
          sym.setFlag(BRIDGE | ARTIFACT)
          in.skip(attrLen)

        case tpnme.DeprecatedATTR =>
          in.skip(attrLen)
          if (!sym.hasAnnotation(JavaDeprecatedAttr))
            sym.addAnnotation(JavaDeprecatedAttr)
          if (sym == clazz && !staticModule.hasAnnotation(JavaDeprecatedAttr))
            staticModule.addAnnotation(JavaDeprecatedAttr)

        case tpnme.ConstantValueATTR =>
          completer.constant = pool.getConstant(u2())

        case tpnme.MethodParametersATTR =>
          def readParamNames(): Unit = {
            val paramCount = u1()
            val paramNames = new Array[NameOrString](paramCount)
            val paramNameAccess = new Array[Int](paramCount)
            var i = 0
            while (i < paramCount) {
              paramNames(i) = u2() match {
                case 0 => null  // may occur on JDK 21+, as per scala/bug#12783
                case index => pool.getExternalName(index)
              }
              paramNameAccess(i) = u2()
              i += 1
            }
            completer.paramNames = new ParamNames(paramNames, paramNameAccess)
          }
          readParamNames()

        case tpnme.AnnotationDefaultATTR => // Methods of java annotation classes that have a default
          sym.addAnnotation(AnnotationDefaultAttr)
          in.skip(attrLen)

        case tpnme.RuntimeAnnotationATTR =>
          val numAnnots = u2()
          numAnnots times {
            parseAnnotation(u2()).foreach(addUniqueAnnotation(sym, _))
          }

        // TODO 1: parse runtime visible annotations on parameters
        // case tpnme.RuntimeParamAnnotationATTR

        // TODO 2: also parse RuntimeInvisibleAnnotation / RuntimeInvisibleParamAnnotation,
        // i.e. java annotations with RetentionPolicy.CLASS?

        case tpnme.ExceptionsATTR =>
          parseExceptions(attrLen, completer)

        case tpnme.SourceFileATTR =>
          /*
          if (forInteractive) {
            // opt: disable this code in the batch compiler for performance reasons.
            // it appears to be looking for the .java source file mentioned in this attribute
            // in the output directories of scalac.
            //
            // References:
            // https://issues.scala-lang.org/browse/SI-2689
            // https://github.com/scala/scala/commit/7315339782f6e19ddd6199768352a91ef66eb27d
            // https://github.com/scala-ide/scala-ide/commit/786ea5d4dc44065379a05eb3ac65d37f8948c05d
            //
            // TODO: Does Scala-IDE actually intermingle source and classfiles in a way that this could ever find something?
            //       If they want to use this, they'll need to enable the new setting -Ypresentation-locate-source-file.
            val srcfileLeaf = readName().toString.trim
            val srcpath = sym.enclosingPackage match {
              case NoSymbol => srcfileLeaf
              case rootMirror.EmptyPackage => srcfileLeaf
              case pkg => pkg.fullName(File.separatorChar)+File.separator+srcfileLeaf
            }
            srcfile0 = settings.outputDirs.srcFilesFor(file, srcpath).find(_.exists)
          } else in.skip(attrLen)
          */
          in.skip(attrLen)

        case tpnme.CodeATTR =>
          if (sym.owner.isInterface) {
            sym setFlag JAVA_DEFAULTMETHOD
            debuglog(s"$sym in ${sym.owner} is a java8+ default method.")
          }
          in.skip(attrLen)

        case tpnme.PermittedSubclassesATTR =>
          sym.setFlag(SEALED)
          val numberOfClasses = u2()
          numberOfClasses times {
            val k = pool.getClassSymbol(u2())
            completer match {
              case ctc: ClassTypeCompleter => ctc.permittedSubclasses ::= k   // sym.addChild(k)
              case _ =>
            }
          }

        case _ =>
          in.skip(attrLen)
      }
    }

    /*
     * Parse the "Exceptions" attribute which denotes the exceptions
     * thrown by a method.
     */
    def parseExceptions(@unused len: Int, completer: JavaTypeCompleter): Unit = {
      val nClasses = u2()
      for (_ <- 0 until nClasses) {
        // FIXME: this performs an equivalent of getExceptionTypes instead of getGenericExceptionTypes (scala/bug#7065)
        val cls = pool.getClassName(u2())
        completer.exceptions ::= cls
      }
    }
    // begin parseAttributes
    u2() times parseAttribute()
  }


  def parseAnnotArg(): Option[ClassfileAnnotArg] = {
    val tag = u1()
    val index = u2()
    tag match {
      case STRING_TAG =>
        Some(LiteralAnnotArg(Constant(pool.getName(index).value)))
      case BOOL_TAG | BYTE_TAG | CHAR_TAG | SHORT_TAG | INT_TAG |
           LONG_TAG | FLOAT_TAG | DOUBLE_TAG =>
        Some(LiteralAnnotArg(pool.getConstant(index)))
      case CLASS_TAG  =>
        Some(LiteralAnnotArg(Constant(pool.getType(index))))
      case ENUM_TAG   =>
        val t = pool.getType(index)
        val n = readName()
        val module = t.typeSymbol.companionModule
        val s = module.info.decls.lookup(n)
        if (s != NoSymbol) Some(LiteralAnnotArg(Constant(s)))
        else {
          loaders.warning(
            NoPosition,
            sm"""While parsing annotations in ${file}, could not find $n in enum ${module.nameString}.
                |This is likely due to an implementation restriction: an annotation argument cannot refer to a member of the annotated class (scala/bug#7014).""",
            WarningCategory.Other,
            clazz.fullNameString)
          None
        }

      case ARRAY_TAG  =>
        val arr = new ArrayBuffer[ClassfileAnnotArg]()
        var hasError = false
        index times {
          parseAnnotArg() match {
            case Some(c) => arr += c
            case None => hasError = true
          }
        }
        if (hasError) None
        else Some(ArrayAnnotArg(arr.toArray))
      case ANNOTATION_TAG =>
        parseAnnotation(index).map(NestedAnnotArg(_))
    }
  }


  // TODO scala/bug#9296 duplicated code, refactor
  /**
   * Parse and return a single annotation.  If it is malformed, return None.
   */
  def parseAnnotation(attrNameIndex: Int): Option[AnnotationInfo] = try {
    val attrType = pool.getType(attrNameIndex)
    val nargs = u2()
    val nvpairs = new ListBuffer[(Name, ClassfileAnnotArg)]
    var hasError = false
    nargs times {
      val name = readName()
      parseAnnotArg() match {
        case Some(c) => nvpairs += ((name, c))
        case None => hasError = true
      }
    }
    if (hasError) None
    else Some(AnnotationInfo(attrType, List(), nvpairs.toList))
  } catch {
    case f: FatalError => throw f // don't eat fatal errors, they mean a class was not found
    case NonFatal(ex) =>
      // We want to be robust when annotations are unavailable, so the very least
      // we can do is warn the user about the exception
      // There was a reference to ticket 1135, but that is outdated: a reference to a class not on
      // the classpath would *not* end up here. A class not found is signaled
      // with a `FatalError` exception, handled above. Here you'd end up after a NPE (for example),
      // and that should never be swallowed silently.
      loaders.warning(NoPosition, s"Caught: $ex while parsing annotations in ${file}", WarningCategory.Other, clazz.fullNameString)
      if (settings.isDebug) ex.printStackTrace()
      None // ignore malformed annotations
  }

  /** Apply `@native`/`@transient`/`@volatile` annotations to `sym`,
    * if the corresponding flag is set in `flags`.
    */
  def addJavaFlagsAnnotations(sym: Symbol, flags: JavaAccFlags): Unit =
    flags.toScalaAnnotations(symbolTable).foreach(sym.addAnnotation(_))

  /** Enter own inner classes in the right scope. It needs the scopes to be set up,
   *  and implicitly current class' superclasses.
   */
  private def enterOwnInnerClasses(): Unit = {
    def className(name: String): String =
      name.substring(name.lastIndexOf('.') + 1, name.length)

    def enterClassAndModule(entry: InnerClassEntry, file: AbstractFile): Unit = {
      def jflags      = entry.jflags
      val name        = entry.originalName
      val sflags      = jflags.toScalaFlags
      val owner       = ownerForFlags(jflags)
      val scope       = getScope(jflags)
      def newStub(name: Name) = {
        val stub = owner.newStubSymbol(name, s"Class file for ${entry.externalName} not found")
        stub.setPos(owner.pos)
        stub.setFlag(JAVA)
      }

      val (innerClass, innerModule) = if (file == NoAbstractFile) {
        (newStub(name.toTypeName), newStub(name.toTermName))
      } else {
        val cls = owner.newClass(name.toTypeName, NoPosition, sflags)
        val mod = owner.newModule(name.toTermName, NoPosition, sflags)
        val completer = new loaders.ClassfileLoader(file, cls, mod)
        cls setInfo completer
        mod setInfo completer
        mod.moduleClass setInfo loaders.moduleClassLoader
        cls.associatedFile = file
        mod.moduleClass.associatedFile = file

        /*
         * need to set privateWithin here because the classfile of a nested protected class is public in bytecode,
         * so propagatePackageBoundary will not set it when the symbols are completed
         */
        if (jflags.isProtected) {
          cls.privateWithin = cls.enclosingPackage
          mod.privateWithin = cls.enclosingPackage
        }

        (cls, mod)
      }

      scope enter innerClass
      scope enter innerModule

      val decls = innerClass.enclosingPackage.info.decls
      def unlinkIfPresent(name: Name) = {
        val e = decls lookupEntry name
        if (e ne null)
          decls unlink e
      }

      val cName = newTermName(className(entry.externalName))
      unlinkIfPresent(cName)
      unlinkIfPresent(cName.toTypeName)
    }

    for (entry <- innerClasses.entries) {
      // create a new class member for immediate inner classes
      if (entry.outerName == currentClass) {
        val file = classPath.findClassFile(entry.externalName.toString)
        enterClassAndModule(entry, file.getOrElse(NoAbstractFile))
      }
    }
  }

  /**
   * Either
   *   - set `isScala` and invoke the unpickler, or
   *   - set `isScalaRaw`, or
   *   - parse inner classes (for Java classfiles)
   *
   * Expects `in.bp` to point to the `access_flags` entry, restores the old `bp`.
   */
  def unpickleOrParseInnerClasses(): Unit = {
    val oldbp = in.bp
    in.skip(4) // access_flags, this_class
    skipSuperclasses()
    skipMembers() // fields
    skipMembers() // methods

    var innersStart = -1
    var runtimeAnnotStart = -1

    val numAttrs = u2()
    var i = 0
    while (i < numAttrs) {
      val attrName = readTypeName()
      val attrLen = u4()
      attrName match {
        case tpnme.ScalaSignatureATTR =>
          isScala = true
          if (runtimeAnnotStart != -1) i = numAttrs
        case tpnme.ScalaATTR =>
          isScalaRaw = true
          i = numAttrs
        case tpnme.TASTYATTR =>
          MissingRequirementError.notFound(s"TASTy file for associated class file $file")
        case tpnme.InnerClassesATTR =>
          innersStart = in.bp
        case tpnme.RuntimeAnnotationATTR =>
          runtimeAnnotStart = in.bp
          if (isScala) i = numAttrs
        case _ =>
      }
      in.skip(attrLen)
      i += 1
    }

    if (isScala) {
      def parseScalaSigBytes(): Array[Byte] = {
        val tag = u1()
        assert(tag == STRING_TAG, tag)
        pool.getBytes(u2())
      }

      def parseScalaLongSigBytes(): Array[Byte] = {
        val tag = u1()
        assert(tag == ARRAY_TAG, tag)
        val stringCount = u2()
        val entries =
          for (_ <- 0 until stringCount) yield {
            val stag = u1()
            assert(stag == STRING_TAG, stag)
            u2()
          }
        pool.getBytes(entries.toList)
      }

      def checkScalaSigAnnotArg() = {
        val numArgs = u2()
        assert(numArgs == 1, s"ScalaSignature has $numArgs arguments")
        val name = readName()
        assert(name == nme.bytes, s"ScalaSignature argument has name $name")
      }

      def skipAnnotArg(): Unit = u1() match {
        case STRING_TAG | BOOL_TAG | BYTE_TAG | CHAR_TAG | SHORT_TAG |
             INT_TAG | LONG_TAG | FLOAT_TAG | DOUBLE_TAG | CLASS_TAG =>
          in.skip(2)

        case ENUM_TAG =>
          in.skip(4)

        case ARRAY_TAG =>
          val num = u2()
          num times skipAnnotArg()

        case ANNOTATION_TAG =>
          in.skip(2) // type
          skipAnnotArgs()
      }

      def skipAnnotArgs() = {
        val numArgs = u2()
        numArgs times {
          in.skip(2)
          skipAnnotArg()
        }
      }

      val SigTpe = ScalaSignatureAnnotation.tpe
      val LongSigTpe = ScalaLongSignatureAnnotation.tpe

      assert(runtimeAnnotStart != -1, s"No RuntimeVisibleAnnotations in classfile with ScalaSignature attribute: $clazz")
      in.bp = runtimeAnnotStart
      val numAnnots = u2()
      var i = 0
      var bytes: Array[Byte] = null
      while (i < numAnnots && bytes == null) {
        pool.getType(u2()) match {
          case SigTpe =>
            checkScalaSigAnnotArg()
            bytes = parseScalaSigBytes()
          case LongSigTpe =>
            checkScalaSigAnnotArg()
            bytes = parseScalaLongSigBytes()
          case _ =>
            skipAnnotArgs()
        }
        i += 1
      }

      AnyRefClass // Force scala.AnyRef, otherwise we get "error: Symbol AnyRef is missing from the classpath"
      assert(bytes != null, s"No Scala(Long)Signature annotation in classfile with ScalaSignature attribute: $clazz")
      unpickler.unpickle(bytes, 0, clazz, staticModule, file.name)
    } else if (!isScalaRaw && innersStart != -1) {
      in.bp = innersStart
      val entries = u2()
      entries times {
        val innerIndex, outerIndex, nameIndex = u2()
        val jflags = readInnerClassFlags()
        if (innerIndex != 0 && outerIndex != 0 && nameIndex != 0)
          innerClasses add InnerClassEntry(pool.getClassName(innerIndex), pool.getClassName(outerIndex), pool.getName(nameIndex), jflags)
      }
    }
    in.bp = oldbp
  }

  /** An entry in the InnerClasses attribute of this class file. */
  case class InnerClassEntry(external: NameOrString, outer: NameOrString, name: NameOrString, jflags: JavaAccFlags) {
    def externalName = external.value
    def outerName    = outer.value
    def originalName = name.name
    def isModule     = originalName.isTermName
    def scope        = if (jflags.isStatic) staticScope else instanceScope
    def enclosing    = if (jflags.isStatic) enclModule else enclClass

    // The name of the outer class, without its trailing $ if it has one.
    private def strippedOuter = outerName.stripSuffix(nme.MODULE_SUFFIX_STRING)
    private def isInner       = innerClasses contains strippedOuter
    private def enclClass     = if (isInner) innerClasses innerSymbol strippedOuter else classNameToSymbol(strippedOuter)
    private def enclModule    = enclClass.companionModule
  }

  /** Return the class symbol for the given name. It looks it up in its outer class.
   *  Forces all outer class symbols to be completed.
   *
   *  If the given name is not an inner class, it returns the symbol found in `definitions`.
   */
  object innerClasses {
    private val inners = mutable.HashMap[String, InnerClassEntry]()

    def contains(name: String) = inners contains name
    def getEntry(name: String) = inners get name
    def entries              = inners.values

    def add(entry: InnerClassEntry): Unit = {
      devWarningIf(inners contains entry.externalName) {
        val existing = inners(entry.externalName)
        s"Overwriting inner class entry! Was $existing, now $entry"
      }
      inners(entry.externalName) = entry
    }
    def innerSymbol(externalName: String): Symbol = this getEntry externalName match {
      case Some(entry) => innerSymbol(entry)
      case _           => NoSymbol
    }

    private def innerSymbol(entry: InnerClassEntry): Symbol = {
      val name      = entry.originalName.toTypeName
      val enclosing = entry.enclosing
      val member = {
        if (enclosing == clazz) entry.scope lookup name
        else lookupMemberAtTyperPhaseIfPossible(enclosing, name)
      }
      def newStub = {
        enclosing
          .newStubSymbol(name, s"Unable to locate class corresponding to inner class entry for $name in owner ${entry.outerName}")
          .setPos(enclosing.pos)
      }
      member.orElse(newStub)
    }
  }

  class TypeParamsType(override val typeParams: List[Symbol]) extends LazyType with FlagAgnosticCompleter {
    override def complete(sym: Symbol): Unit = { throw new AssertionError("cyclic type dereferencing") }
  }
  class LazyAliasType(alias: Symbol) extends LazyType with FlagAgnosticCompleter {
    override def complete(sym: Symbol): Unit = {
      sym setInfo createFromClonedSymbols(alias.initialize.typeParams, alias.tpe)(typeFun)
    }
  }
  // on JDK 21+, `names` may include nulls, as per scala/bug#12783
  private class ParamNames(val names: Array[NameOrString], val access: Array[Int]) {
    assert(names.length == access.length, "Require as many names as access")
    def length = names.length
  }
  private abstract class JavaTypeCompleter extends LazyType {
    var constant: Constant = _
    var sig: String = _
    var paramNames: ParamNames = _
    var exceptions: List[NameOrString] = Nil
  }
  private final class ClassTypeCompleter(@unused name: Name, @unused jflags: JavaAccFlags, parent: NameOrString, ifaces: List[NameOrString]) extends JavaTypeCompleter {
    var permittedSubclasses: List[symbolTable.Symbol] = Nil
    override def complete(sym: symbolTable.Symbol): Unit = {
      val info = if (sig != null) sigToType(sym, sig) else {
        val superTpe = if (parent == null) definitions.AnyClass.tpe_* else getClassSymbol(parent.value).tpe_*
        val superTpe1 = if (superTpe == ObjectTpeJava) ObjectTpe else superTpe
        val ifacesTypes = ifaces.filterNot(_ eq null).map(x => getClassSymbol(x.value).tpe_*)
        ClassInfoType(superTpe1 :: ifacesTypes, instanceScope, clazz)
      }
      sym.setInfo(info)
      // enum children are its enum fields, so don't register subclasses (which are listed as permitted)
      if (!sym.hasJavaEnumFlag)
        for (k <- permittedSubclasses)
          if (k.parentSymbols.contains(sym))
            sym.addChild(k)
    }
  }

  private final class MemberTypeCompleter(name: Name, jflags: JavaAccFlags, descriptor: String) extends JavaTypeCompleter {
    override def isJavaVarargsMethod: Boolean = jflags.isVarargs
    override def javaThrownExceptions: List[Symbol] = exceptions.map(e => classNameToSymbol(e.value))
    override def complete(sym: symbolTable.Symbol): Unit = {
      def descriptorInfo = sigToType(sym, descriptor)
      val hasOuterParam = (name == nme.CONSTRUCTOR) && (descriptorInfo match {
        case MethodType(params, _) =>
          // if this is a non-static inner class, remove the explicit outer parameter
          innerClasses getEntry currentClass match {
            case Some(entry) if !entry.jflags.isStatic =>
              /* About `clazz.owner.hasPackageFlag` below: scala/bug#5957
               * For every nested java class A$B, there are two symbols in the scala compiler.
               *  1. created by SymbolLoader, because of the existence of the A$B.class file, owner: package
               *  2. created by ClassfileParser of A when reading the inner classes, owner: A
               * If symbol 1 gets completed (e.g. because the compiled source mentions `A$B`, not `A#B`), the
               * ClassfileParser for 1 executes, and clazz.owner is the package.
               */
              assert(params.head.tpe.typeSymbol == clazz.owner || clazz.owner.hasPackageFlag, "" + params.head.tpe.typeSymbol + ": " + clazz.owner)
              true
            case _ =>
              false
          }
        case _ => false
      })

      val info = if (sig != null) {
        sigToType(sym, sig)
      } else if (name == nme.CONSTRUCTOR) {
        descriptorInfo match {
          case MethodType(params, _) =>
            val paramsNoOuter = if (hasOuterParam) params.tail else params
            val newParams = paramsNoOuter match {
              case init :+ _ if jflags.isSynthetic =>
                // scala/bug#7455 strip trailing dummy argument ("access constructor tag") from synthetic constructors which
                // are added when an inner class needs to access a private constructor.
                init
              case _ =>
                paramsNoOuter
            }
            MethodType(newParams, clazz.tpe)
          case info => info
        }
      } else {
        descriptorInfo
      }
      if (constant != null) {
        val c1 = convertTo(constant, info.resultType)
        if (c1 ne null) sym.setInfo(ConstantType(c1))
        else {
          devWarning(s"failure to convert $constant to ${info.resultType}")
          sym.setInfo(info)
        }
      } else {
        sym.setInfo(if (sym.isMethod && jflags.isVarargs) arrayToRepeated(info) else info)
      }

      for (e <- exceptions) {
        // we call initialize due to the fact that we call Symbol.isMonomorphicType in addThrowsAnnotation
        // and that method requires Symbol to be forced to give the right answers, see scala/bug#7107 for details
        val cls = getClassSymbol(e.value)
        sym withAnnotation AnnotationInfo.lazily {
          val throwableTpe = cls.tpe_*
          AnnotationInfo(appliedType(ThrowsClass, throwableTpe), List(Literal(Constant(throwableTpe))), Nil)
        }
      }

      // Note: the info may be overwritten later with a generic signature
      // parsed from SignatureATTR
      if (paramNames != null) {
        import scala.tools.asm.Opcodes.ACC_SYNTHETIC

        if (sym.hasRawInfo && sym.isMethod) {
          val paramNamesNoOuter = (if (hasOuterParam) 1 else 0) to paramNames.length
          val params = sym.rawInfo.params
          foreach2(paramNamesNoOuter.toList, params) {
            case (i, param) =>
              val isSynthetic = (paramNames.access(i) & ACC_SYNTHETIC) != 0
              if (!isSynthetic) {
                param.resetFlag(SYNTHETIC)
                val nameOrString = paramNames.names(i)
                if (nameOrString != null)
                  param.name = nameOrString.name.toTermName.encode
              }
          }
          // there's not anything we can do, but it's slightly worrisome
          devWarningIf(!sameLength(paramNamesNoOuter.toList, params)) {
            sm"""MethodParameters length mismatch while parsing $sym:
                |  rawInfo.params: ${sym.rawInfo.params}"""
          }
        }
      }
    }
    private def convertTo(c: Constant, pt: Type): Constant = {
      if (pt.typeSymbol == BooleanClass && c.tag == IntTag)
        Constant(c.value != 0)
      else
        c convertTo pt
    }
  }

  def skipAttributes(): Unit = {
    var attrCount: Int = u2()
    while (attrCount > 0) {
      in skip 2
      in skip u4()
      attrCount -= 1
    }
  }

  def skipMembers(): Unit = {
    var memberCount: Int = u2()
    while (memberCount > 0) {
      in skip 6
      skipAttributes()
      memberCount -= 1
    }
  }

  def skipSuperclasses(): Unit = {
    in.skip(2) // superclass
    val ifaces = u2()
    in.skip(2 * ifaces)
  }

  protected def getScope(flags: JavaAccFlags): Scope =
    if (flags.isStatic) staticScope else instanceScope

  // Append annotation. For Java deprecation, prefer an annotation with values (since, etc).
  private def addUniqueAnnotation(symbol: Symbol, annot: AnnotationInfo): symbol.type =
    if (annot.atp.typeSymbol == JavaDeprecatedAttr) {
      def ensureDepr(sym: Symbol): sym.type = {
        if (sym.hasAnnotation(JavaDeprecatedAttr))
          if (List(0, 1).exists(annot.constantAtIndex(_).isDefined))
            sym.setAnnotations {
              def drop(cur: AnnotationInfo): Boolean = cur.atp.typeSymbol == JavaDeprecatedAttr
              sym.annotations.foldRight(annot :: Nil)((a, all) => if (drop(a)) all else a :: all)
            }
          else sym
        else sym.addAnnotation(annot)
      }
      if (symbol == clazz)
        ensureDepr(staticModule)
      ensureDepr(symbol)
    }
    else symbol.addAnnotation(annot)
}
object ClassfileParser {
  private implicit class GoodTimes(private val n: Int) extends AnyVal {
    def times(body: => Unit): Unit = {
      var i = n
      while (i > 0) {
        body
        i -= 1
      }
    }
  }
}
