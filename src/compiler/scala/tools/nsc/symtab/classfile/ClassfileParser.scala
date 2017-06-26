/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package tools.nsc
package symtab
package classfile

import java.io.{ByteArrayInputStream, DataInputStream, File, IOException}
import java.lang.Integer.toHexString

import scala.collection.{immutable, mutable}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.annotation.switch
import scala.reflect.internal.JavaAccFlags
import scala.reflect.internal.pickling.{ByteCodecs, PickleBuffer}
import scala.reflect.io.NoAbstractFile
import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.io.AbstractFile
import scala.util.control.NonFatal

/** This abstract class implements a class file parser.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
abstract class ClassfileParser {
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

  protected var file: AbstractFile     = _     // the class file
  protected var in: AbstractFileReader = _     // the class file reader
  protected var clazz: ClassSymbol = _         // the class symbol containing dynamic members
  protected var staticModule: ModuleSymbol = _ // the module symbol containing static members
  protected var instanceScope: Scope = _       // the scope of all instance definitions
  protected var staticScope: Scope = _         // the scope of all static definitions
  protected var pool: ConstantPool = _         // the classfile's constant pool
  protected var isScala: Boolean = _           // does class file describe a scala class?
  protected var isScalaRaw: Boolean = _        // this class file is a scala class with no pickled info
  protected var busy: Symbol = _               // lock to detect recursive reads
  protected var currentClass: Name = _         // JVM name of the current class
  protected var classTParams = Map[Name,Symbol]()
  protected var srcfile0 : Option[AbstractFile] = None
  protected def moduleClass: Symbol = staticModule.moduleClass
  private var sawPrivateConstructor = false

  private def ownerForFlags(jflags: JavaAccFlags) = if (jflags.isStatic) moduleClass else clazz

  def srcfile = srcfile0

  private def optimized         = settings.optimise.value

  // u1, u2, and u4 are what these data types are called in the JVM spec.
  // They are an unsigned byte, unsigned char, and unsigned int respectively.
  // We bitmask u1 into an Int to make sure it's 0-255 (and u1 isn't used
  // for much beyond tags) but leave u2 alone as it's already unsigned.
  protected final def u1(): Int = in.nextByte & 0xFF
  protected final def u2(): Int = in.nextChar.toInt
  protected final def u4(): Int = in.nextInt

  protected final def s1(): Int = in.nextByte.toInt // sign-extend the byte to int
  protected final def s2(): Int = (in.nextByte.toInt << 8) | u1 // sign-extend and shift the first byte, or with the unsigned second byte

  private def readInnerClassFlags() = readClassFlags()
  private def readClassFlags()      = JavaAccFlags classFlags u2
  private def readMethodFlags()     = JavaAccFlags methodFlags u2
  private def readFieldFlags()      = JavaAccFlags fieldFlags u2
  private def readTypeName()        = readName().toTypeName
  private def readName()            = pool getName u2
  private def readType()            = pool getType u2

  private object unpickler extends scala.reflect.internal.pickling.UnPickler {
    val symbolTable: ClassfileParser.this.symbolTable.type = ClassfileParser.this.symbolTable
  }

  private def handleMissing(e: MissingRequirementError) = {
    if (settings.debug) e.printStackTrace
    throw new IOException(s"Missing dependency '${e.req}', required by $file")
  }
  private def handleError(e: Exception) = {
    if (settings.debug) e.printStackTrace()
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
  @inline private def raiseLoaderLevel[T](body: => T): T = {
    loaders.parentsLevel += 1
    try body
    finally loaders.parentsLevel -= 1
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
      this.in           = new AbstractFileReader(file)
      this.clazz        = clazz
      this.staticModule = module
      this.isScala      = false

      parseHeader()
      this.pool = new ConstantPool
      parseClass()
    }
  }

  private def parseHeader() {
    val magic = u4
    if (magic != JAVA_MAGIC)
      abort(s"class file ${in.file} has wrong magic number 0x${toHexString(magic)}")

    val minor, major = u2
    if (major < JAVA_MAJOR_VERSION || major == JAVA_MAJOR_VERSION && minor < JAVA_MINOR_VERSION)
      abort(s"class file ${in.file} has unknown version $major.$minor, should be at least $JAVA_MAJOR_VERSION.$JAVA_MINOR_VERSION")
  }

  /**
   * Constructor of this class should not be called directly, use `newConstantPool` instead.
   */
  protected class ConstantPool {
    protected val len          = u2
    protected val starts       = new Array[Int](len)
    protected val values       = new Array[AnyRef](len)
    protected val internalized = new Array[Name](len)

    { var i = 1
      while (i < starts.length) {
        starts(i) = in.bp
        i += 1
        (u1: @switch) match {
          case CONSTANT_UTF8 | CONSTANT_UNICODE                                => in skip u2
          case CONSTANT_CLASS | CONSTANT_STRING | CONSTANT_METHODTYPE          => in skip 2
          case CONSTANT_METHODHANDLE                                           => in skip 3
          case CONSTANT_FIELDREF | CONSTANT_METHODREF | CONSTANT_INTFMETHODREF => in skip 4
          case CONSTANT_NAMEANDTYPE | CONSTANT_INTEGER | CONSTANT_FLOAT        => in skip 4
          case CONSTANT_INVOKEDYNAMIC                                          => in skip 4
          case CONSTANT_LONG | CONSTANT_DOUBLE                                 => in skip 8 ; i += 1
          case _                                                               => errorBadTag(in.bp - 1)
        }
      }
    }

    def recordAtIndex[T <: AnyRef](value: T, idx: Int): T = {
      values(idx) = value
      value
    }

    def firstExpecting(index: Int, expected: Int): Int = {
      val start = starts(index)
      val first = in.buf(start).toInt
      if (first == expected) start + 1
      else this errorBadTag start
    }

    /** Return the name found at given index. */
    def getName(index: Int): Name = (
      if (index <= 0 || len <= index) errorBadIndex(index)
      else values(index) match {
        case name: Name => name
        case _          =>
          val start = firstExpecting(index, CONSTANT_UTF8)
          val len   = in.getChar(start).toInt
          recordAtIndex(TermName(fromMUTF8(in.buf, start, len + 2)), index)
      }
    )

    private def fromMUTF8(bytes: Array[Byte], offset: Int, len: Int): String =
      new DataInputStream(new ByteArrayInputStream(bytes, offset, len)).readUTF

    /** Return the name found at given index in the constant pool, with '/' replaced by '.'. */
    def getExternalName(index: Int): Name = {
      if (index <= 0 || len <= index)
        errorBadIndex(index)

      if (internalized(index) == null)
        internalized(index) = getName(index).replace('/', '.')

      internalized(index)
    }

    def getClassSymbol(index: Int): Symbol = {
      if (index <= 0 || len <= index) errorBadIndex(index)
      values(index) match {
        case sym: Symbol => sym
        case _           =>
          val result = getClassName(index) match {
            case name if nme.isModuleName(name) => rootMirror getModuleByName name.dropModule
            case name                           => classNameToSymbol(name)
          }
          recordAtIndex(result, index)
      }
    }

    /** Return the external name of the class info structure found at 'index'.
     *  Use 'getClassSymbol' if the class is sure to be a top-level class.
     */
    def getClassName(index: Int): Name = {
      val start = firstExpecting(index, CONSTANT_CLASS)
      getExternalName((in getChar start).toInt)
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
          val dummy = ownerTpe.typeSymbol.newMethod(name.toTermName, ownerTpe.typeSymbol.pos)
          val tpe   = getType(dummy, in.getChar(start + 2).toInt)
          // fix the return type, which is blindly set to the class currently parsed
          val restpe = tpe match {
            case MethodType(formals, _) if name == nme.CONSTRUCTOR => MethodType(formals, ownerTpe)
            case _                                                 => tpe
          }
          ((name, restpe))
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
          name charAt 0 match {
            case ARRAY_TAG => recordAtIndex(sigToType(null, name), index)
            case _         => recordAtIndex(classNameToSymbol(name), index).tpe_*
          }
      }
    }

    def getType(index: Int): Type              = getType(null, index)
    def getType(sym: Symbol, index: Int): Type = sigToType(sym, getExternalName(index))
    def getSuperClass(index: Int): Symbol      = if (index == 0) AnyClass else getClassSymbol(index) // the only classfile that is allowed to have `0` in the super_class is java/lang/Object (see jvm spec)

    private def createConstant(index: Int): Constant = {
      val start = starts(index)
      Constant((in.buf(start).toInt: @switch) match {
        case CONSTANT_STRING  => getName(in.getChar(start + 1).toInt).toString
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

    def getBytes(index: Int): Array[Byte] = (
      if (index <= 0 || len <= index) errorBadIndex(index)
      else values(index) match {
        case xs: Array[Byte] => xs
        case _               =>
          val start = firstExpecting(index, CONSTANT_UTF8)
          val len   = (in getChar start).toInt
          val bytes = new Array[Byte](len)
          System.arraycopy(in.buf, start + 2, bytes, 0, len)
          recordAtIndex(getSubArray(bytes), index)
      }
    )

    def getBytes(indices: List[Int]): Array[Byte] = {
      val head = indices.head
      values(head) match {
        case xs: Array[Byte] => xs
        case _               =>
          val arr: Array[Byte] = indices.toArray flatMap { index =>
            if (index <= 0 || ConstantPool.this.len <= index) errorBadIndex(index)
            val start = firstExpecting(index, CONSTANT_UTF8)
            val len   = (in getChar start).toInt
            in.buf drop start + 2 take len
          }
          recordAtIndex(getSubArray(arr), head)
      }
    }

    /** Throws an exception signaling a bad constant index. */
    protected def errorBadIndex(index: Int) =
      abort(s"bad constant pool index: $index at pos: ${in.bp}")

    /** Throws an exception signaling a bad tag at given address. */
    protected def errorBadTag(start: Int) =
      abort(s"bad constant pool tag ${in.buf(start)} at byte $start")
  }

  def stubClassSymbol(name: Name): Symbol = {
    // scala/bug#5593 Scaladoc's current strategy is to visit all packages in search of user code that can be documented
    // therefore, it will rummage through the classpath triggering errors whenever it encounters package objects
    // that are not in their correct place (see bug for details)

    // TODO More consistency with use of stub symbols in `Unpickler`
    //   - better owner than `NoSymbol`
    //   - remove eager warning
    val msg = s"Class $name not found - continuing with a stub."
    if ((!settings.isScaladoc) && (settings.verbose || settings.developer)) warning(msg)
    NoSymbol.newStubSymbol(name.toTypeName, msg)
  }

  private def lookupClass(name: Name) = try {
    def lookupTopLevel = {
      if (name containsChar '.')
        rootMirror getClassByName name
      else
      // FIXME - we shouldn't be doing ad hoc lookups in the empty package, getClassByName should return the class
        definitions.getMember(rootMirror.EmptyPackageClass, name.toTypeName)
    }

    // For inner classes we usually don't get here: `classNameToSymbol` already returns the symbol
    // of the inner class based on the InnerClass table. However, if the classfile is missing the
    // InnerClass entry for `name`, it might still be that there exists an inner symbol (because
    // some other classfile _does_ have an InnerClass entry for `name`). In this case, we want to
    // return the actual inner symbol (C.D, with owner C), not the top-level symbol C$D. This is
    // what the logic below is for (see PR #5822 / scala/bug#9937).
    val split = if (isScalaRaw) -1 else name.lastIndexOf('$')
    if (split > 0 && split < name.length) {
      val outerName = name.subName(0, split)
      val innerName = name.subName(split + 1, name.length).toTypeName
      val outerSym = classNameToSymbol(outerName)

      // If the outer class C cannot be found, look for a top-level class C$D
      if (outerSym.isInstanceOf[StubSymbol]) lookupTopLevel
      else {
        // We have a java-defined class name C$D and look for a member D of C. But we don't know if
        // D is declared static or not, so we have to search both in class C and its companion.
        val r = if (outerSym == clazz)
          staticScope.lookup(innerName) orElse
            instanceScope.lookup(innerName)
        else
          lookupMemberAtTyperPhaseIfPossible(outerSym, innerName) orElse
            lookupMemberAtTyperPhaseIfPossible(outerSym.companionModule, innerName)
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
    case _: FatalError =>
      // getClassByName can throw a MissingRequirementError (which extends FatalError)
      // definitions.getMember can throw a FatalError, for example in pos/t5165b
      stubClassSymbol(name)
  }

  /** Return the class symbol of the given name. */
  def classNameToSymbol(name: Name): Symbol = {
    if (innerClasses contains name)
      innerClasses innerSymbol name
    else
      lookupClass(name)
  }

  def parseClass() {
    unpickleOrParseInnerClasses()

    val jflags = readClassFlags()
    val classNameIndex = u2
    currentClass = pool.getClassName(classNameIndex)

    // Ensure that (top-level) classfiles are in the correct directory
    val isTopLevel = !(currentClass containsChar '$') // Java class name; *don't* try to to use Scala name decoding (scala/bug#7532)
    if (isTopLevel) {
      val c = pool.getClassSymbol(classNameIndex)
      // scala-dev#248: when a type alias (in a package object) shadows a class symbol, getClassSymbol returns a stub
      // TODO: this also prevents the error when it would be useful (`mv a/C.class .`)
      if (!c.isInstanceOf[StubSymbol] && c != clazz) mismatchError(c)
    }

    // TODO: remove after the next 2.13 milestone
    // A bug in the backend caused classes ending in `$` do get only a Scala marker attribute
    // instead of a ScalaSig and a Signature annotaiton. This went unnoticed because isScalaRaw
    // classes were parsed like Java classes. The below covers the cases in the std lib.
    def isNothingOrNull = {
      val n = clazz.fullName.toString
      n == "scala.runtime.Nothing$" || n == "scala.runtime.Null$"
    }

    if (isScala) {
      () // We're done
    } else if (isScalaRaw && !isNothingOrNull) {
      val decls = clazz.enclosingPackage.info.decls
      for (c <- List(clazz, staticModule, staticModule.moduleClass)) {
        c.setInfo(NoType)
        decls.unlink(c)
      }
    } else {
      val sflags = jflags.toScalaFlags // includes JAVA

      def parseParents(): List[Type] = raiseLoaderLevel {
        val superType = if (jflags.isAnnotation) { u2; AnnotationClass.tpe }
                        else pool.getSuperClass(u2).tpe_*
        val ifaceCount = u2
        var ifaces = for (i <- List.range(0, ifaceCount)) yield pool.getSuperClass(u2).tpe_*
        if (jflags.isAnnotation) ifaces ::= ClassfileAnnotationClass.tpe
        superType :: ifaces
      }

      addEnclosingTParams(clazz)

      // Create scopes before calling `enterOwnInnerClasses`
      instanceScope = newScope
      staticScope = newScope
      val staticInfo = ClassInfoType(List(), staticScope, moduleClass)
      val classInfo = ClassInfoType(parseParents(), instanceScope, clazz)

      enterOwnInnerClasses()

      clazz setInfo classInfo
      clazz setFlag sflags
      moduleClass setInfo staticInfo
      moduleClass setFlag JAVA
      staticModule setInfo moduleClass.tpe
      staticModule setFlag JAVA

      propagatePackageBoundary(jflags, clazz, staticModule, moduleClass)

      val fieldsStartBp = in.bp
      skipMembers() // fields
      skipMembers() // methods

      // attributes now depend on having infos set already
      parseAttributes(clazz, classInfo)

      def queueLoad() {
        in.bp = fieldsStartBp
        0 until u2 foreach (_ => parseField())
        sawPrivateConstructor = false
        0 until u2 foreach (_ => parseMethod())
        val needsConstructor = (
             !sawPrivateConstructor
          && !(instanceScope containsName nme.CONSTRUCTOR)
          && (sflags & INTERFACE) == 0
        )
        if (needsConstructor)
          instanceScope enter clazz.newClassConstructor(NoPosition)
      }

      loaders.pendingLoadActions ::= (queueLoad _)
      if (loaders.parentsLevel == 0) {
        while (loaders.pendingLoadActions.nonEmpty) {
          val item = loaders.pendingLoadActions.head
          loaders.pendingLoadActions = loaders.pendingLoadActions.tail
          item()
        }
      }
    }
  }

  /** Add type parameters of enclosing classes */
  def addEnclosingTParams(clazz: Symbol) {
    var sym = clazz.owner
    while (sym.isClass && !sym.isModuleClass) {
      for (t <- sym.tpe.typeArgs)
        classTParams = classTParams + (t.typeSymbol.name -> t.typeSymbol)

      sym = sym.owner
    }
  }

  def parseField() {
    val jflags = readFieldFlags()
    val sflags = jflags.toScalaFlags

    if ((sflags & PRIVATE) != 0L && !optimized) {
      in.skip(4); skipAttributes()
    } else {
      val name    = readName()
      val info    = readType()
      val sym     = ownerForFlags(jflags).newValue(name.toTermName, NoPosition, sflags)

      // Note: the info may be overwritten later with a generic signature
      // parsed from SignatureATTR
      sym setInfo {
        if (jflags.isEnum) ConstantType(Constant(sym))
        else info
      }
      propagatePackageBoundary(jflags, sym)
      parseAttributes(sym, info)
      getScope(jflags) enter sym

      // sealed java enums
      if (jflags.isEnum) {
        val enumClass = sym.owner.linkedClassOfClass
        enumClass match {
          case NoSymbol =>
            devWarning(s"no linked class for java enum $sym in ${sym.owner}. A referencing class file might be missing an InnerClasses entry.")
          case linked =>
            if (!linked.isSealed)
              // Marking the enum class SEALED | ABSTRACT enables exhaustiveness checking. See also JavaParsers.
              // This is a bit of a hack and requires excluding the ABSTRACT flag in the backend, see method javaClassfileFlags.
              linked setFlag (SEALED | ABSTRACT)
            linked addChild sym
        }
      }
    }
  }

  def parseMethod() {
    val jflags = readMethodFlags()
    val sflags = jflags.toScalaFlags
    if (jflags.isPrivate && !optimized) {
      val name = readName()
      if (name == nme.CONSTRUCTOR)
        sawPrivateConstructor = true
      in.skip(2); skipAttributes()
    } else {
      if ((sflags & PRIVATE) != 0L && optimized) { // TODO this should be !optimized, no? See c4181f656d.
        in.skip(4); skipAttributes()
      } else {
        val name = readName()
        val sym = ownerForFlags(jflags).newMethod(name.toTermName, NoPosition, sflags)
        var info = pool.getType(sym, u2)
        var removedOuterParameter = false
        if (name == nme.CONSTRUCTOR)
          info match {
            case MethodType(params, restpe) =>
              // if this is a non-static inner class, remove the explicit outer parameter
              val paramsNoOuter = innerClasses getEntry currentClass match {
                case Some(entry) if !entry.jflags.isStatic =>
                  /* About `clazz.owner.hasPackageFlag` below: scala/bug#5957
                   * For every nested java class A$B, there are two symbols in the scala compiler.
                   *  1. created by SymbolLoader, because of the existence of the A$B.class file, owner: package
                   *  2. created by ClassfileParser of A when reading the inner classes, owner: A
                   * If symbol 1 gets completed (e.g. because the compiled source mentions `A$B`, not `A#B`), the
                   * ClassfileParser for 1 executes, and clazz.owner is the package.
                   */
                  assert(params.head.tpe.typeSymbol == clazz.owner || clazz.owner.hasPackageFlag, params.head.tpe.typeSymbol + ": " + clazz.owner)
                  removedOuterParameter = true
                  params.tail
                case _ =>
                  params
              }
              val newParams = paramsNoOuter match {
                case (init :+ tail) if jflags.isSynthetic =>
                  // scala/bug#7455 strip trailing dummy argument ("access constructor tag") from synthetic constructors which
                  // are added when an inner class needs to access a private constructor.
                  init
                case _ =>
                  paramsNoOuter
              }

              info = MethodType(newParams, clazz.tpe)
          }
        // Note: the info may be overwritten later with a generic signature
        // parsed from SignatureATTR
        sym setInfo info
        propagatePackageBoundary(jflags, sym)
        parseAttributes(sym, info, removedOuterParameter)
        if (jflags.isVarargs)
          sym modifyInfo arrayToRepeated

        getScope(jflags) enter sym
      }
    }
  }

  private def sigToType(sym: Symbol, sig: Name): Type = {
    var index = 0
    val end = sig.length
    def accept(ch: Char) {
      assert(sig.charAt(index) == ch, (sig.charAt(index), ch))
      index += 1
    }
    def subName(isDelimiter: Char => Boolean): Name = {
      val start = index
      while (!isDelimiter(sig.charAt(index))) { index += 1 }
      sig.subName(start, index)
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
            case TypeRef(pre, sym, args) if (!sym.isStatic) =>
              typeRef(processInner(pre.widen), sym, args)
            case _ =>
              tp
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
                        case '+' => TypeBounds.upper(objToAny(sig2type(tparams, skiptvs)))
                        case '-' =>
                          val tp = sig2type(tparams, skiptvs)
                          // sig2type seems to return AnyClass regardless of the situation:
                          // we don't want Any as a LOWER bound.
                          if (tp.typeSymbol == AnyClass) TypeBounds.empty
                          else TypeBounds.lower(tp)
                        case '*' => TypeBounds.empty
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

          val classSym = classNameToSymbol(subName(c => c == ';' || c == '<'))
          assert(!classSym.isOverloaded, classSym.alternatives)
          var tpe = processClassType(processInner(classSym.tpe_*))
          while (sig.charAt(index) == '.') {
            accept('.')
            val name = subName(c => c == ';' || c == '<' || c == '.').toTypeName
            val clazz = tpe.member(name)
            val dummyArgs = Nil // the actual arguments are added in processClassType
            val inner = typeRef(pre = tpe, sym = clazz, args = dummyArgs)
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
          // NOTE that the comparison to Object only works for abstract types bounded by classes that are strict subclasses of Object
          // if the bound is exactly Object, it will have been converted to Any, and the comparison will fail
          // see also RestrictJavaArraysMap (when compiling java sources directly)
          if (elemtp.typeSymbol.isAbstractType && !(elemtp <:< ObjectTpe)) {
            elemtp = intersectionType(List(elemtp, ObjectTpe))
          }

          arrayType(elemtp)
        case '(' =>
          // we need a method symbol. given in line 486 by calling getType(methodSym, ..)
          assert(sym ne null, sig)
          val paramtypes = new ListBuffer[Type]()
          while (sig.charAt(index) != ')') {
            paramtypes += objToAny(sig2type(tparams, skiptvs))
          }
          index += 1
          val restype = if (sym != null && sym.isClassConstructor) {
            accept('V')
            clazz.tpe_*
          } else
            sig2type(tparams, skiptvs)
          JavaMethodType(sym.newSyntheticValueParams(paramtypes.toList), restype)
        case 'T' =>
          val n = subName(';'.==).toTypeName
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
          ts += objToAny(sig2type(tparams, skiptvs))
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
        val tpname = subName(':'.==).toTypeName
        val s = sym.newTypeParameter(tpname)
        tparams = tparams + (tpname -> s)
        sig2typeBounds(tparams, skiptvs = true)
        newTParams += s
      }
      index = start
      while (sig.charAt(index) != '>') {
        val tpname = subName(':'.==).toTypeName
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
          parents += sig2type(tparams, skiptvs = false)  // here the variance doesn't matter
        }
        ClassInfoType(parents.toList, instanceScope, sym)
      }
    GenPolyType(ownTypeParams, tpe)
  } // sigToType

  /**
   * Only invoked for java classfiles.
   */
  def parseAttributes(sym: Symbol, symtype: Type, removedOuterParameter: Boolean = false) {
    def convertTo(c: Constant, pt: Type): Constant = {
      if (pt.typeSymbol == BooleanClass && c.tag == IntTag)
        Constant(c.value != 0)
      else
        c convertTo pt
    }

    def parseAttribute() {
      val attrName = readTypeName()
      val attrLen  = u4
      attrName match {
        case tpnme.SignatureATTR =>
          val sig = pool.getExternalName(u2)
          val newType = sigToType(sym, sig)
          sym.setInfo(newType)

        case tpnme.SyntheticATTR =>
          sym.setFlag(SYNTHETIC | ARTIFACT)
          in.skip(attrLen)

        case tpnme.BridgeATTR =>
          sym.setFlag(BRIDGE | ARTIFACT)
          in.skip(attrLen)

        case tpnme.DeprecatedATTR =>
          val arg = Literal(Constant("see corresponding Javadoc for more information."))
          sym.addAnnotation(DeprecatedAttr, arg, Literal(Constant("")))
          in.skip(attrLen)

        case tpnme.ConstantValueATTR =>
          val c = pool.getConstant(u2)
          val c1 = convertTo(c, symtype)
          if (c1 ne null) sym.setInfo(ConstantType(c1))
          else devWarning(s"failure to convert $c to $symtype")

        case tpnme.MethodParametersATTR =>
          def readParamNames(): Unit = {
            import scala.tools.asm.Opcodes.ACC_SYNTHETIC
            val paramCount = u1
            var i = 0
            if (removedOuterParameter && i < paramCount) {
              in.skip(4)
              i += 1
            }
            var remainingParams = sym.paramss.head // Java only has exactly one parameter list
            while (i < paramCount) {
              val name = pool.getName(u2)
              val access = u2
              if (remainingParams.nonEmpty) {
                val param = remainingParams.head
                remainingParams = remainingParams.tail
                if ((access & ACC_SYNTHETIC) != ACC_SYNTHETIC) { // name not synthetic
                  param.name = name.encode
                  param.resetFlag(SYNTHETIC)
                }
              }
              i += 1
            }
          }
          readParamNames()

        case tpnme.AnnotationDefaultATTR => // Methods of java annotation classes that have a default
          sym.addAnnotation(AnnotationDefaultAttr)
          in.skip(attrLen)

        case tpnme.RuntimeAnnotationATTR =>
          val numAnnots = u2
          for (n <- 0 until numAnnots; annot <- parseAnnotation(u2))
            sym.addAnnotation(annot)

        // TODO 1: parse runtime visible annotations on parameters
        // case tpnme.RuntimeParamAnnotationATTR

        // TODO 2: also parse RuntimeInvisibleAnnotation / RuntimeInvisibleParamAnnotation,
        // i.e. java annotations with RetentionPolicy.CLASS?

        case tpnme.ExceptionsATTR =>
          parseExceptions(attrLen)

        case tpnme.SourceFileATTR =>
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
            // TODO: can we disable this altogether? Does Scala-IDE actually intermingle source and classfiles in a way
            //       that this could ever find something?
            val srcfileLeaf = readName().toString.trim
            val srcpath = sym.enclosingPackage match {
              case NoSymbol => srcfileLeaf
              case rootMirror.EmptyPackage => srcfileLeaf
              case pkg => pkg.fullName(File.separatorChar)+File.separator+srcfileLeaf
            }
            srcfile0 = settings.outputDirs.srcFilesFor(in.file, srcpath).find(_.exists)
          } else in.skip(attrLen)

        case tpnme.CodeATTR =>
          if (sym.owner.isInterface) {
            sym setFlag JAVA_DEFAULTMETHOD
            log(s"$sym in ${sym.owner} is a java8+ default method.")
          }
          in.skip(attrLen)

        case _ =>
          in.skip(attrLen)
      }
    }

    /*
     * Parse the "Exceptions" attribute which denotes the exceptions
     * thrown by a method.
     */
    def parseExceptions(len: Int) {
      val nClasses = u2
      for (n <- 0 until nClasses) {
        // FIXME: this performs an equivalent of getExceptionTypes instead of getGenericExceptionTypes (scala/bug#7065)
        val cls = pool.getClassSymbol(u2)
        // we call initialize due to the fact that we call Symbol.isMonomorphicType in addThrowsAnnotation
        // and that method requires Symbol to be forced to give the right answers, see scala/bug#7107 for details
        cls.initialize
        sym.addThrowsAnnotation(cls)
      }
    }

    // begin parseAttributes
    for (i <- 0 until u2) parseAttribute()
  }


  def parseAnnotArg(): Option[ClassfileAnnotArg] = {
    val tag = u1
    val index = u2
    tag match {
      case STRING_TAG =>
        Some(LiteralAnnotArg(Constant(pool.getName(index).toString)))
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
          warning(s"""While parsing annotations in ${in.file}, could not find $n in enum $module.\nThis is likely due to an implementation restriction: an annotation argument cannot refer to a member of the annotated class (scala/bug#7014).""")
          None
        }

      case ARRAY_TAG  =>
        val arr = new ArrayBuffer[ClassfileAnnotArg]()
        var hasError = false
        for (i <- 0 until index)
          parseAnnotArg() match {
            case Some(c) => arr += c
            case None => hasError = true
          }
        if (hasError) None
        else Some(ArrayAnnotArg(arr.toArray))
      case ANNOTATION_TAG =>
        parseAnnotation(index) map (NestedAnnotArg(_))
    }
  }


  // TODO scala/bug#9296 duplicated code, refactor
  /**
   * Parse and return a single annotation.  If it is malformed, return None.
   */
  def parseAnnotation(attrNameIndex: Int): Option[AnnotationInfo] = try {
    val attrType = pool.getType(attrNameIndex)
    val nargs = u2
    val nvpairs = new ListBuffer[(Name, ClassfileAnnotArg)]
    var hasError = false
    for (i <- 0 until nargs) {
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
      warning(s"Caught: $ex while parsing annotations in ${in.file}")
      if (settings.debug) ex.printStackTrace()
      None // ignore malformed annotations
  }

  /** Enter own inner classes in the right scope. It needs the scopes to be set up,
   *  and implicitly current class' superclasses.
   */
  private def enterOwnInnerClasses() {
    def className(name: Name): Name =
      name.subName(name.lastPos('.') + 1, name.length)

    def enterClassAndModule(entry: InnerClassEntry, file: AbstractFile) {
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

      val cName = className(entry.externalName)
      unlinkIfPresent(cName.toTermName)
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
  def unpickleOrParseInnerClasses() {
    val oldbp = in.bp
    in.skip(4) // access_flags, this_class
    skipSuperclasses()
    skipMembers() // fields
    skipMembers() // methods

    var innersStart = -1
    var runtimeAnnotStart = -1

    val numAttrs = u2
    var i = 0
    while (i < numAttrs) {
      val attrName = readTypeName()
      val attrLen = u4
      attrName match {
        case tpnme.ScalaSignatureATTR =>
          isScala = true
          if (runtimeAnnotStart != -1) i = numAttrs
        case tpnme.ScalaATTR =>
          isScalaRaw = true
          i = numAttrs
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
        val tag = u1
        assert(tag == STRING_TAG, tag)
        pool.getBytes(u2)
      }

      def parseScalaLongSigBytes(): Array[Byte] = {
        val tag = u1
        assert(tag == ARRAY_TAG, tag)
        val stringCount = u2
        val entries =
          for (i <- 0 until stringCount) yield {
            val stag = u1
            assert(stag == STRING_TAG, stag)
            u2
          }
        pool.getBytes(entries.toList)
      }

      def checkScalaSigAnnotArg() = {
        val numArgs = u2
        assert(numArgs == 1, s"ScalaSignature has $numArgs arguments")
        val name = readName()
        assert(name == nme.bytes, s"ScalaSignature argument has name $name")
      }

      def skipAnnotArg(): Unit = u1 match {
        case STRING_TAG | BOOL_TAG | BYTE_TAG | CHAR_TAG | SHORT_TAG |
             INT_TAG | LONG_TAG | FLOAT_TAG | DOUBLE_TAG | CLASS_TAG =>
          in.skip(2)

        case ENUM_TAG =>
          in.skip(4)

        case ARRAY_TAG =>
          val num = u2
          for (i <- 0 until num) skipAnnotArg()

        case ANNOTATION_TAG =>
          in.skip(2) // type
          skipAnnotArgs()
      }

      def skipAnnotArgs() = {
        val numArgs = u2
        for (i <- 0 until numArgs) {
          in.skip(2)
          skipAnnotArg()
        }
      }

      val SigTpe = ScalaSignatureAnnotation.tpe
      val LongSigTpe = ScalaLongSignatureAnnotation.tpe

      assert(runtimeAnnotStart != -1, s"No RuntimeVisibleAnnotations in classfile with ScalaSignature attribute: $clazz")
      in.bp = runtimeAnnotStart
      val numAnnots = u2
      var i = 0
      var bytes: Array[Byte] = null
      while (i < numAnnots && bytes == null) pool.getType(u2) match {
        case SigTpe =>
          checkScalaSigAnnotArg()
          bytes = parseScalaSigBytes()
        case LongSigTpe =>
          checkScalaSigAnnotArg()
          bytes = parseScalaLongSigBytes()
        case _ =>
          skipAnnotArgs()
      }

      AnyRefClass // Force scala.AnyRef, otherwise we get "error: Symbol AnyRef is missing from the classpath"
      assert(bytes != null, s"No Scala(Long)Signature annotation in classfile with ScalaSignature attribute: $clazz")
      unpickler.unpickle(bytes, 0, clazz, staticModule, in.file.name)
    } else if (!isScalaRaw && innersStart != -1) {
      in.bp = innersStart
      val entries = u2
      for (i <- 0 until entries) {
        val innerIndex, outerIndex, nameIndex = u2
        val jflags = readInnerClassFlags()
        if (innerIndex != 0 && outerIndex != 0 && nameIndex != 0)
          innerClasses add InnerClassEntry(innerIndex, outerIndex, nameIndex, jflags)
      }
    }
    in.bp = oldbp
  }

  /** An entry in the InnerClasses attribute of this class file. */
  case class InnerClassEntry(external: Int, outer: Int, name: Int, jflags: JavaAccFlags) {
    def externalName = pool getClassName external
    def outerName    = pool getClassName outer
    def originalName = pool getName name
    def isModule     = originalName.isTermName
    def scope        = if (jflags.isStatic) staticScope else instanceScope
    def enclosing    = if (jflags.isStatic) enclModule else enclClass

    // The name of the outer class, without its trailing $ if it has one.
    private def strippedOuter = outerName.dropModule
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
    private val inners = mutable.HashMap[Name, InnerClassEntry]()

    def contains(name: Name) = inners contains name
    def getEntry(name: Name) = inners get name
    def entries              = inners.values

    def add(entry: InnerClassEntry): Unit = {
      inners get entry.externalName foreach (existing =>
        devWarning(s"Overwriting inner class entry! Was $existing, now $entry")
      )
      inners(entry.externalName) = entry
    }
    def innerSymbol(externalName: Name): Symbol = this getEntry externalName match {
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
    override def complete(sym: Symbol) { throw new AssertionError("cyclic type dereferencing") }
  }
  class LazyAliasType(alias: Symbol) extends LazyType with FlagAgnosticCompleter {
    override def complete(sym: Symbol) {
      sym setInfo createFromClonedSymbols(alias.initialize.typeParams, alias.tpe)(typeFun)
    }
  }

  def skipAttributes() {
    var attrCount: Int = u2
    while (attrCount > 0) {
      in skip 2
      in skip u4
      attrCount -= 1
    }
  }

  def skipMembers() {
    var memberCount: Int = u2
    while (memberCount > 0) {
      in skip 6
      skipAttributes()
      memberCount -= 1
    }
  }

  def skipSuperclasses() {
    in.skip(2) // superclass
    val ifaces = u2
    in.skip(2 * ifaces)
  }

  protected def getScope(flags: JavaAccFlags): Scope =
    if (flags.isStatic) staticScope else instanceScope
}
