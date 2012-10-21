/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package symtab

import java.io.IOException
import scala.compat.Platform.currentTime
import scala.tools.nsc.util.{ ClassPath }
import classfile.ClassfileParser
import scala.reflect.internal.MissingRequirementError
import scala.reflect.internal.util.Statistics
import scala.reflect.io.{ AbstractFile, NoAbstractFile }

/** This class ...
 *
 *  @author  Martin Odersky
 *  @version 1.0
 */
abstract class SymbolLoaders {
  val global: Global
  import global._
  import SymbolLoadersStats._

  protected def enterIfNew(owner: Symbol, member: Symbol, completer: SymbolLoader): Symbol = {
    assert(owner.info.decls.lookup(member.name) == NoSymbol, owner.fullName + "." + member.name)
    owner.info.decls enter member
    member
  }

  /** Enter class with given `name` into scope of `root`
   *  and give them `completer` as type.
   */
  def enterClass(owner: Symbol, name: String, completer: SymbolLoader): Symbol = {
    val clazz = owner.newClass(newTypeName(name))
    clazz setInfo completer
    enterIfNew(owner, clazz, completer)
  }

  /** Enter object with given `name` into scope of `root`
   *  and give them `completer` as type.
   */
  def enterObject(owner: Symbol, name: String, completer: SymbolLoader): Symbol = {
    val obj = owner.newObject(newTermName(name))
    obj setInfo completer
    obj.objectClass setInfo objectClassLoader
    enterIfNew(owner, obj, completer)
  }
  @deprecated("Use `enterObject` instead.", "2.11.0")
  def enterModule(owner: Symbol, name: String, completer: SymbolLoader): Symbol =
    enterObject(owner, name, completer)

  /** Enter package with given `name` into scope of `root`
   *  and give them `completer` as type.
   */
  def enterPackage(root: Symbol, name: String, completer: SymbolLoader): Symbol = {
    val pname = newTermName(name)
    val preExisting = root.info.decls lookup pname
    if (preExisting != NoSymbol) {
      // Some jars (often, obfuscated ones) include a package and
      // object with the same name. Rather than render them unusable,
      // offer a setting to resolve the conflict one way or the other.
      // This was motivated by the desire to use YourKit probes, which
      // require yjp.jar at runtime. See SI-2089.
      if (settings.termConflict.isDefault)
        throw new TypeError(
          root+" contains object and package with same name: "+
          name+"\none of them needs to be removed from classpath"
        )
      else if (settings.termConflict.value == "package") {
        global.warning(
          "Resolving package/object name conflict in favor of package " +
          preExisting.fullName + ".  The object will be inaccessible."
        )
        root.info.decls.unlink(preExisting)
      }
      else {
        global.warning(
          "Resolving package/object name conflict in favor of object " +
          preExisting.fullName + ".  The package will be inaccessible."
        )
        return NoSymbol
      }
    }
    // todo: find out initialization sequence for pkg/pkg.objectClass is different from enterObject
    val pkg = root.newPackage(pname)
    pkg.objectClass setInfo completer
    pkg setInfo pkg.objectClass.tpe
    root.info.decls enter pkg
    pkg
  }

  /** Enter class and object with given `name` into scope of `root`
   *  and give them `completer` as type.
   */
  def enterClassAndObject(root: Symbol, name: String, completer: SymbolLoader) {
    val clazz = enterClass(root, name, completer)
    val obj = enterObject(root, name, completer)
    if (!clazz.isAnonymousClass) {
      assert(clazz.companionObject == obj, obj)
      assert(obj.companionClass == clazz, clazz)
    }
  }
  @deprecated("Use `enterClassAndObject` instead.", "2.11.0")
  def enterClassAndModule(root: Symbol, name: String, completer: SymbolLoader): Unit =
    enterClassAndObject(root, name, completer)

  /** In batch mode: Enter class and object with given `name` into scope of `root`
   *  and give them a source completer for given `src` as type.
   *  In IDE mode: Find all toplevel definitions in `src` and enter then into scope of `root`
   *  with source completer for given `src` as type.
   *  (overridden in interactive.Global).
   */
  def enterToplevelsFromSource(root: Symbol, name: String, src: AbstractFile) {
    enterClassAndObject(root, name, new SourcefileLoader(src))
  }

  /** The package objects of scala and scala.reflect should always
   *  be loaded in binary if classfiles are available, even if sourcefiles
   *  are newer. Late-compiling these objects from source leads to compilation
   *  order issues.
   *  Note: We do a name-base comparison here because the method is called before we even
   *  have ReflectPackage defined.
   */
  def binaryOnly(owner: Symbol, name: String): Boolean =
    name == "package" &&
    (owner.fullName == "scala" || owner.fullName == "scala.reflect")

  /** Initialize toplevel class and object symbols in `owner` from class path representation `classRep`
   */
  def initializeFromClassPath(owner: Symbol, classRep: ClassPath[platform.BinaryRepr]#ClassRep) {
    ((classRep.binary, classRep.source) : @unchecked) match {
      case (Some(bin), Some(src))
      if platform.needCompile(bin, src) && !binaryOnly(owner, classRep.name) =>
        if (settings.verbose.value) inform("[symloader] picked up newer source file for " + src.path)
        global.loaders.enterToplevelsFromSource(owner, classRep.name, src)
      case (None, Some(src)) =>
        if (settings.verbose.value) inform("[symloader] no class, picked up source file for " + src.path)
        global.loaders.enterToplevelsFromSource(owner, classRep.name, src)
      case (Some(bin), _) =>
        global.loaders.enterClassAndObject(owner, classRep.name, platform.newClassLoader(bin))
    }
  }

  /**
   * A lazy type that completes itself by calling parameter doComplete.
   * Any linked objects/classes or object classes are also initialized.
   * Todo: consider factoring out behavior from TopClassCompleter/SymbolLoader into
   * supertrait SymLoader
   */
  abstract class SymbolLoader extends SymLoader {

    /** Load source or class file for `root`, return */
    protected def doComplete(root: Symbol): Unit

    def sourcefile: Option[AbstractFile] = None

    /**
     * Description of the resource (ClassPath, AbstractFile)
     * being processed by this loader
     */
    protected def description: String

    private var ok = false

    private def setSource(sym: Symbol) {
      sourcefile foreach (sf => sym match {
        case cls: ClassSymbol => cls.associatedFile = sf
        case mod: ObjectSymbol => mod.objectClass.associatedFile = sf
        case _ => ()
      })
    }

    override def complete(root: Symbol) {
      def signalError(ex: Exception) {
        ok = false
        if (settings.debug.value) ex.printStackTrace()
        val msg = ex.getMessage()
        // SI-5593 Scaladoc's current strategy is to visit all packages in search of user code that can be documented
        // therefore, it will rummage through the classpath triggering errors whenever it encounters package objects
        // that are not in their correct place (see bug for details)
        if (!settings.isScaladoc)
          globalError(
            if (msg eq null) "i/o error while loading " + root.name
            else "error while loading " + root.name + ", " + msg);
      }
      try {
        val start = currentTime
        val currentphase = phase
        doComplete(root)
        phase = currentphase
        informTime("loaded " + description, start)
        ok = true
        setSource(root)
        setSource(root.companionSymbol) // object -> class, class -> object
      } catch {
        case ex: IOException =>
          signalError(ex)
        case ex: MissingRequirementError =>
          signalError(ex)
      }
      initRoot(root)
      if (!root.isPackageClass) initRoot(root.companionSymbol)
    }

    override def load(root: Symbol) { complete(root) }

    private def markAbsent(sym: Symbol): Unit = {
      val tpe: Type = if (ok) NoType else ErrorType

      if (sym != NoSymbol)
        sym setInfo tpe
    }
    private def initRoot(root: Symbol) {
      if (root.rawInfo == this)
        List(root, root.objectClass) foreach markAbsent
      else if (root.isClass && !root.isObjectClass)
        root.rawInfo.load(root)
    }
  }

  /**
   * Load contents of a package
   */
  class PackageLoader(classpath: ClassPath[platform.BinaryRepr]) extends SymbolLoader with FlagAgnosticCompleter {
    protected def description = "package loader "+ classpath.name

    protected def doComplete(root: Symbol) {
      assert(root.isPackageClass, root)
      root.setInfo(new PackageClassInfoType(newScope, root))

      if (!root.isRoot) {
        for (classRep <- classpath.classes if platform.doLoad(classRep)) {
          initializeFromClassPath(root, classRep)
        }
      }
      if (!root.isEmptyPackageClass) {
        for (pkg <- classpath.packages) {
          enterPackage(root, pkg.name, new PackageLoader(pkg))
        }

        openPackageObject(root)
      }
    }
  }

  class ClassfileLoader(val classfile: AbstractFile) extends SymbolLoader with FlagAssigningCompleter {
    private object classfileParser extends ClassfileParser {
      val global: SymbolLoaders.this.global.type = SymbolLoaders.this.global
    }

    protected def description = "class file "+ classfile.toString

    protected def doComplete(root: Symbol) {
      val start = if (Statistics.canEnable) Statistics.startTimer(classReadNanos) else null
      classfileParser.parse(classfile, root)
      if (root.associatedFile eq NoAbstractFile) {
        root match {
          // In fact, the ObjectSymbol forwards its setter to the object class
          case _: ClassSymbol | _: ObjectSymbol =>
            debuglog("ClassfileLoader setting %s.associatedFile = %s".format(root.name, classfile))
            root.associatedFile = classfile
          case _ =>
            debuglog("Not setting associatedFile to %s because %s is a %s".format(classfile, root.name, root.shortSymbolClass))
        }
      }
      if (Statistics.canEnable) Statistics.stopTimer(classReadNanos, start)
    }
    override def sourcefile: Option[AbstractFile] = classfileParser.srcfile
  }

  class SourcefileLoader(val srcfile: AbstractFile) extends SymbolLoader with FlagAssigningCompleter {
    protected def description = "source file "+ srcfile.toString
    override def fromSource = true
    override def sourcefile = Some(srcfile)
    protected def doComplete(root: Symbol): Unit = global.currentRun.compileLate(srcfile)
  }

  object objectClassLoader extends SymbolLoader with FlagAssigningCompleter {
    protected def description = "object class loader"
    protected def doComplete(root: Symbol) { root.sourceObject.initialize }
  }

  /** used from classfile parser to avoid cyclies */
  var parentsLevel = 0
  var pendingLoadActions: List[() => Unit] = Nil
}

object SymbolLoadersStats {
  import scala.reflect.internal.TypesStats.typerNanos
  val classReadNanos = Statistics.newSubTimer  ("time classfilereading", typerNanos)
}
