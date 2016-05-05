/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package symtab

import classfile.ClassfileParser
import java.io.IOException
import scala.reflect.internal.MissingRequirementError
import scala.reflect.internal.util.Statistics
import scala.reflect.io.{AbstractFile, NoAbstractFile}
import scala.tools.nsc.util.{ClassPath, ClassRepresentation}

/** This class ...
 *
 *  @author  Martin Odersky
 *  @version 1.0
 */
abstract class SymbolLoaders {
  val symbolTable: symtab.SymbolTable {
    def settings: Settings
  }
  val platform: backend.Platform {
    val symbolTable: SymbolLoaders.this.symbolTable.type
  }
  import symbolTable._
  /**
   * Required by ClassfileParser. Check documentation in that class for details.
   */
  def lookupMemberAtTyperPhaseIfPossible(sym: Symbol, name: Name): Symbol
  /**
   * Should forward to `Run.compileLate`. The more principled fix would be to
   * determine why this functionality is needed and extract it into a separate
   * interface.
   */
  protected def compileLate(srcfile: AbstractFile): Unit
  import SymbolLoadersStats._

  protected def enterIfNew(owner: Symbol, member: Symbol, completer: SymbolLoader): Symbol = {
    assert(owner.info.decls.lookup(member.name) == NoSymbol, owner.fullName + "." + member.name)
    owner.info.decls enter member
    member
  }

  protected def signalError(root: Symbol, ex: Throwable) {
    if (settings.debug) ex.printStackTrace()
    globalError(ex.getMessage() match {
      case null => "i/o error while loading " + root.name
      case msg  => "error while loading " + root.name + ", " + msg
    })
  }

  /** Enter class with given `name` into scope of `root`
   *  and give them `completer` as type.
   */
  def enterClass(owner: Symbol, name: String, completer: SymbolLoader): Symbol = {
    val clazz = owner.newClass(newTypeName(name))
    clazz setInfo completer
    enterIfNew(owner, clazz, completer)
  }

  /** Enter module with given `name` into scope of `root`
   *  and give them `completer` as type.
   */
  def enterModule(owner: Symbol, name: String, completer: SymbolLoader): Symbol = {
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
          s"$root contains object and package with same name: $name\none of them needs to be removed from classpath"
        )
      else if (settings.termConflict.value == "package") {
        warning(
          "Resolving package/object name conflict in favor of package " +
          preExisting.fullName + ".  The object will be inaccessible."
        )
        root.info.decls.unlink(preExisting)
      }
      else {
        warning(
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
      // Diagnostic for SI-7147
      def msg: String = {
        def symLocation(sym: Symbol) = if (sym == null) "null" else s"${clazz.fullLocationString} (from ${clazz.associatedFile})"
        sm"""Inconsistent class/module symbol pair for `$name` loaded from ${symLocation(root)}.
            |clazz = ${symLocation(clazz)}; clazz.companionModule = ${clazz.companionModule}
            |module = ${symLocation(module)}; module.companionClass = ${module.companionClass}"""
      }
      assert(clazz.companionModule == module, msg)
      assert(module.companionClass == clazz, msg)
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

  /** Initialize toplevel class and module symbols in `owner` from class path representation `classRep`
   */
  def initializeFromClassPath(owner: Symbol, classRep: ClassRepresentation) {
    ((classRep.binary, classRep.source) : @unchecked) match {
      case (Some(bin), Some(src))
      if platform.needCompile(bin, src) && !binaryOnly(owner, classRep.name) =>
        if (settings.verbose) inform("[symloader] picked up newer source file for " + src.path)
        enterToplevelsFromSource(owner, classRep.name, src)
      case (None, Some(src)) =>
        if (settings.verbose) inform("[symloader] no class, picked up source file for " + src.path)
        enterToplevelsFromSource(owner, classRep.name, src)
      case (Some(bin), _) =>
        enterClassAndModule(owner, classRep.name, newClassLoader(bin))
    }
  }

  /** Create a new loader from a binary classfile.
   *  This is intended as a hook allowing to support loading symbols from
   *  files other than .class files.
   */
  protected def newClassLoader(bin: AbstractFile): SymbolLoader =
    new ClassfileLoader(bin)

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
     * Description of the resource (ClassPath, AbstractFile)
     * being processed by this loader
     */
    protected def description: String

    private var ok = false

    private def setSource(sym: Symbol) {
      sourcefile foreach (sf => sym match {
        case cls: ClassSymbol => cls.associatedFile = sf
        case mod: ModuleSymbol => mod.moduleClass.associatedFile = sf
        case _ => ()
      })
    }

    override def complete(root: Symbol) {
      try {
        val start = java.util.concurrent.TimeUnit.NANOSECONDS.toMillis(System.nanoTime())
        val currentphase = phase
        doComplete(root)
        phase = currentphase
        informTime("loaded " + description, start)
        ok = true
        setSource(root)
        setSource(root.companionSymbol) // module -> class, class -> module
      }
      catch {
        case ex @ (_: IOException | _: MissingRequirementError) =>
          ok = false
          signalError(root, ex)
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

  private def phaseBeforeRefchecks: Phase = {
    var resPhase = phase
    while (resPhase.refChecked) resPhase = resPhase.prev
    resPhase
  }

  /**
   * Loads contents of a package
   */
  class PackageLoader(packageName: String, classPath: ClassPath) extends SymbolLoader with FlagAgnosticCompleter {
    protected def description = {
      val shownPackageName = if (packageName == ClassPath.RootPackage) "<root package>" else packageName
      s"package loader $shownPackageName"
    }

    protected def doComplete(root: Symbol) {
      assert(root.isPackageClass, root)
      root.setInfo(new PackageClassInfoType(newScope, root))

      val classPathEntries = classPath.list(packageName)

      if (!root.isRoot)
        for (entry <- classPathEntries.classesAndSources) initializeFromClassPath(root, entry)
      if (!root.isEmptyPackageClass) {
        for (pkg <- classPathEntries.packages) {
          val fullName = pkg.name

          val name =
            if (packageName == ClassPath.RootPackage) fullName
            else fullName.substring(packageName.length + 1)
          val packageLoader = new PackageLoader(fullName, classPath)
          enterPackage(root, name, packageLoader)
        }

        openPackageModule(root)
      }
    }
  }

  class ClassfileLoader(val classfile: AbstractFile) extends SymbolLoader with FlagAssigningCompleter {
    private object classfileParser extends {
      val symbolTable: SymbolLoaders.this.symbolTable.type = SymbolLoaders.this.symbolTable
    } with ClassfileParser {
      override protected type ThisConstantPool = ConstantPool
      override protected def newConstantPool: ThisConstantPool = new ConstantPool
      override protected def lookupMemberAtTyperPhaseIfPossible(sym: Symbol, name: Name): Symbol =
        SymbolLoaders.this.lookupMemberAtTyperPhaseIfPossible(sym, name)
      /*
       * The type alias and the cast (where the alias is used) is needed due to problem described
       * in SI-7585. In this particular case, the problem is that we need to make sure that symbol
       * table used by symbol loaders is exactly the same as they one used by classfileParser.
       * If you look at the path-dependent types we have here everything should work out ok but
       * due to issue described in SI-7585 type-checker cannot tie the knot here.
       *
       */
      private type SymbolLoadersRefined = SymbolLoaders { val symbolTable: classfileParser.symbolTable.type }

      val loaders = SymbolLoaders.this.asInstanceOf[SymbolLoadersRefined]

      override def classPath: ClassPath = platform.classPath
    }

    protected def description = "class file "+ classfile.toString

    protected def doComplete(root: Symbol) {
      val start = if (Statistics.canEnable) Statistics.startTimer(classReadNanos) else null

      // Running the classfile parser after refchecks can lead to "illegal class file dependency"
      // errors. More concretely, the classfile parser calls "sym.companionModule", which calls
      // "isModuleNotMethod" on the companion. After refchecks, this method forces the info, which
      // may run the classfile parser. This produces the error.
      enteringPhase(phaseBeforeRefchecks)(classfileParser.parse(classfile, root))

      if (root.associatedFile eq NoAbstractFile) {
        root match {
          // In fact, the ModuleSymbol forwards its setter to the module class
          case _: ClassSymbol | _: ModuleSymbol =>
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
    protected def doComplete(root: Symbol): Unit = compileLate(srcfile)
  }

  object moduleClassLoader extends SymbolLoader with FlagAssigningCompleter {
    protected def description = "module class loader"
    protected def doComplete(root: Symbol) { root.sourceModule.initialize }
  }

  /** used from classfile parser to avoid cycles */
  var parentsLevel = 0
  var pendingLoadActions: List[() => Unit] = Nil
}

object SymbolLoadersStats {
  import scala.reflect.internal.TypesStats.typerNanos
  val classReadNanos = Statistics.newSubTimer  ("time classfilereading", typerNanos)
}
