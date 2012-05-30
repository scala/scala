/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package symtab

import java.io.IOException
import scala.compat.Platform.currentTime
import scala.tools.nsc.util.{ ClassPath }
import classfile.ClassfileParser
import reflect.internal.Flags._
import reflect.internal.MissingRequirementError
import util.Statistics._
import scala.tools.nsc.io.{ AbstractFile, MsilFile }

/** This class ...
 *
 *  @author  Martin Odersky
 *  @version 1.0
 */
abstract class SymbolLoaders {
  val global: Global
  import global._

  protected def enterIfNew(owner: Symbol, member: Symbol, completer: SymbolLoader): Symbol = {
    assert(owner.info.decls.lookup(member.name) == NoSymbol, owner.fullName + "." + member.name)
    owner.info.decls enter member
    member
  }

  /** Enter class with given `name` into scope of `root`
   *  and give them `completer` as type.
   */
  def enterClass(root: Symbol, name: String, completer: SymbolLoader): Symbol = {
    val owner = root.ownerOfNewSymbols
    val clazz = owner.newClass(newTypeName(name))
    clazz setInfo completer
    enterIfNew(owner, clazz, completer)
  }

  /** Enter module with given `name` into scope of `root`
   *  and give them `completer` as type.
   */
  def enterModule(root: Symbol, name: String, completer: SymbolLoader): Symbol = {
    val owner = root.ownerOfNewSymbols
    val module = owner.newModule(newTermName(name))
    module setInfo completer
    module.moduleClass setInfo moduleClassLoader
    enterIfNew(owner, module, completer)
  }

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
    // todo: find out initialization sequence for pkg/pkg.moduleClass is different from enterModule
    val pkg = root.newPackage(pname)
    pkg.moduleClass setInfo completer
    pkg setInfo pkg.moduleClass.tpe
    root.info.decls enter pkg
    pkg
  }

  /** Enter class and module with given `name` into scope of `root`
   *  and give them `completer` as type.
   */
  def enterClassAndModule(root: Symbol, name: String, completer: SymbolLoader) {
    val clazz = enterClass(root, name, completer)
    val module = enterModule(root, name, completer)
    if (!clazz.isAnonymousClass) {
      assert(clazz.companionModule == module, module)
      assert(module.companionClass == clazz, clazz)
    }
  }

  /** In batch mode: Enter class and module with given `name` into scope of `root`
   *  and give them a source completer for given `src` as type.
   *  In IDE mode: Find all toplevel definitions in `src` and enter then into scope of `root`
   *  with source completer for given `src` as type.
   *  (overridden in interactive.Global).
   */
  def enterToplevelsFromSource(root: Symbol, name: String, src: AbstractFile) {
    enterClassAndModule(root, name, new SourcefileLoader(src))
  }

  /** Initialize toplevel class and module symbols in `owner` from class path representation `classRep`
   */
  def initializeFromClassPath(owner: Symbol, classRep: ClassPath[platform.BinaryRepr]#ClassRep) {
    ((classRep.binary, classRep.source) : @unchecked) match {
      case (Some(bin), Some(src)) if platform.needCompile(bin, src) =>
        if (settings.verbose.value) inform("[symloader] picked up newer source file for " + src.path)
        global.loaders.enterToplevelsFromSource(owner, classRep.name, src)
      case (None, Some(src)) =>
        if (settings.verbose.value) inform("[symloader] no class, picked up source file for " + src.path)
        global.loaders.enterToplevelsFromSource(owner, classRep.name, src)
      case (Some(bin), _) =>
        global.loaders.enterClassAndModule(owner, classRep.name, platform.newClassLoader(bin))
    }
  }

  /**
   * A lazy type that completes itself by calling parameter doComplete.
   * Any linked modules/classes or module classes are also initialized.
   * Todo: consider factoring out behavior from TopClassCompleter/SymbolLoader into
   * supertrait SymLoader
   */
  abstract class SymbolLoader extends SymLoader {

    /** Load source or class file for `root`, return */
    protected def doComplete(root: Symbol): Unit

    def sourcefile: Option[AbstractFile] = None

    /**
     * Description of the resource (ClassPath, AbstractFile, MsilFile)
     * being processed by this loader
     */
    protected def description: String

    private var ok = false

    private def setSource(sym: Symbol) {
      sourcefile foreach (sf => sym match {
        case cls: ClassSymbol => cls.sourceFile = sf
        case mod: ModuleSymbol => mod.moduleClass.sourceFile = sf
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
        setSource(root.companionSymbol) // module -> class, class -> module
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
        List(root, root.moduleClass) foreach markAbsent
      else if (root.isClass && !root.isModuleClass)
        root.rawInfo.load(root)
    }
  }

  /**
   * Load contents of a package
   */
  class PackageLoader(classpath: ClassPath[platform.BinaryRepr]) extends SymbolLoader {
    protected def description = "package loader "+ classpath.name

    protected def doComplete(root: Symbol) {
      assert(root.isPackageClass, root)
      root.setInfo(new PackageClassInfoType(newScope, root))

      val sourcepaths = classpath.sourcepaths
      for (classRep <- classpath.classes if platform.doLoad(classRep)) {
        initializeFromClassPath(root, classRep)
      }

      for (pkg <- classpath.packages) {
        enterPackage(root, pkg.name, new PackageLoader(pkg))
      }

      openPackageModule(root)
    }
  }

  class ClassfileLoader(val classfile: AbstractFile) extends SymbolLoader {
    private object classfileParser extends ClassfileParser {
      val global: SymbolLoaders.this.global.type = SymbolLoaders.this.global
    }

    protected def description = "class file "+ classfile.toString

    protected def doComplete(root: Symbol) {
      val start = startTimer(classReadNanos)
      classfileParser.parse(classfile, root)
      if (root.associatedFile eq null) {
        root match {
          // In fact, the ModuleSymbol forwards its setter to the module class
          case _: ClassSymbol | _: ModuleSymbol =>
            debuglog("ClassfileLoader setting %s.associatedFile = %s".format(root.name, classfile))
            root.associatedFile = classfile
          case _ =>
            debuglog("Not setting associatedFile to %s because %s is a %s".format(classfile, root.name, root.shortSymbolClass))
        }
      }
      stopTimer(classReadNanos, start)
    }
    override def sourcefile: Option[AbstractFile] = classfileParser.srcfile
  }

  class MsilFileLoader(msilFile: MsilFile) extends SymbolLoader {
    private def typ = msilFile.msilType
    private object typeParser extends clr.TypeParser {
      val global: SymbolLoaders.this.global.type = SymbolLoaders.this.global
    }

    protected def description = "MsilFile "+ typ.FullName + ", assembly "+ typ.Assembly.FullName
    protected def doComplete(root: Symbol) { typeParser.parse(typ, root) }
  }

  class SourcefileLoader(val srcfile: AbstractFile) extends SymbolLoader {
    protected def description = "source file "+ srcfile.toString
    override def fromSource = true
    override def sourcefile = Some(srcfile)
    protected def doComplete(root: Symbol): Unit = global.currentRun.compileLate(srcfile)
  }

  object moduleClassLoader extends SymbolLoader {
    protected def description = "module class loader"
    protected def doComplete(root: Symbol) { root.sourceModule.initialize }
  }

  object clrTypes extends clr.CLRTypes {
    val global: SymbolLoaders.this.global.type = SymbolLoaders.this.global
    if (global.forMSIL) init()
  }

  /** used from classfile parser to avoid cyclies */
  var parentsLevel = 0
  var pendingLoadActions: List[() => Unit] = Nil
}
