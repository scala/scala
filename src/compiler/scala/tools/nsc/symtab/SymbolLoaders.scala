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

package scala.tools.nsc
package symtab

import classfile.{ClassfileParser, ReusableDataReader}
import java.io.IOException

import scala.annotation._
import scala.reflect.internal.MissingRequirementError
import scala.reflect.internal.util.ReusableInstance
import scala.reflect.io.{AbstractFile, NoAbstractFile}
import scala.tools.nsc.Reporting.WarningCategory
import scala.tools.nsc.util.{ClassPath, ClassRepresentation}

/** This class ...
 *
 *  @author  Martin Odersky
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

  // forwards to runReporting.warning, but we don't have global in scope here
  def warning(pos: Position, msg: String, category: WarningCategory, site: String): Unit

  protected def enterIfNew(owner: Symbol, member: Symbol, @unused completer: SymbolLoader): Symbol = {
    assert(owner.info.decls.lookup(member.name) == NoSymbol, owner.fullName + "." + member.name)
    owner.info.decls enter member
    member
  }

  protected def signalError(root: Symbol, ex: Throwable): Unit = {
    if (settings.isDebug) ex.printStackTrace()
    globalError(ex.getMessage() match {
      case null => "i/o error while loading " + root.name
      case msg  => "error while loading " + root.name + ", " + msg
    })
  }

  def newClass(owner: Symbol, name: String): ClassSymbol = owner.newClass(newTypeName(name))

  /** Enter class with given `name` into scope of `root`
   *  and give them `completer` as type.
   */
  def enterClass(owner: Symbol, name: String, completer: SymbolLoader): Symbol =
    enterClass(owner, newClass(owner, name), completer)

  def enterClass(owner: Symbol, clazz: ClassSymbol, completer: SymbolLoader): Symbol = {
    clazz setInfo completer
    enterIfNew(owner, clazz, completer)
  }

  def newModule(owner: Symbol, name: String): ModuleSymbol = owner.newModule(newTermName(name))

  /** Enter module with given `name` into scope of `root`
   *  and give them `completer` as type.
   */
  def enterModule(owner: Symbol, name: String, completer: SymbolLoader): Symbol =
    enterModule(owner, newModule(owner, name), completer)

  def enterModule(owner: Symbol, module: ModuleSymbol, completer: SymbolLoader): Symbol = {
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
      // require yjp.jar at runtime. See scala/bug#2089.
      if (settings.termConflict.isDefault)
        throw new TypeError(
          s"$root contains object and package with same name: $name\none of them needs to be removed from classpath"
        )
      else if (settings.termConflict.value == "package") {
        warning(
          NoPosition,
          "Resolving package/object name conflict in favor of package " +
          preExisting.fullName + ".  The object will be inaccessible.",
          WarningCategory.Other,
          site = "")
        root.info.decls.unlink(preExisting)
      }
      else {
        warning(
          NoPosition,
          "Resolving package/object name conflict in favor of object " +
          preExisting.fullName + ".  The package will be inaccessible.",
          WarningCategory.Other,
          site = "")
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
  def enterClassAndModule(root: Symbol, name: TermName, getCompleter: (ClassSymbol, ModuleSymbol) => SymbolLoader): Unit = {
    val clazz0 = root.newClass(name.toTypeName)
    val module0 = root.newModule(name)
    val completer = getCompleter(clazz0, module0)
    // enterClass/Module may return an existing symbol instead of the ones we created above
    // this may happen when there's both sources and binaries on the classpath, but the class
    // name is different from the file name, so the classpath can't match the binary and source
    // representation. `companionModule/Class` prefers the source version, so we should be careful
    // to reuse the symbols returned below.
    val clazz = enterClass(root, clazz0, completer)
    val module = enterModule(root, module0, completer)
    if (!clazz.isAnonymousClass) {
      // Diagnostic for scala/bug#7147
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
  def enterToplevelsFromSource(root: Symbol, name: TermName, src: AbstractFile): Unit = {
    enterClassAndModule(root, name, (_, _) => new SourcefileLoader(src))
  }

  /** The package objects of scala and scala.reflect should always
   *  be loaded in binary if classfiles are available, even if sourcefiles
   *  are newer. Late-compiling these objects from source leads to compilation
   *  order issues.
   *  Note: We do a name-base comparison here because the method is called before we even
   *  have ReflectPackage defined.
   */
  def binaryOnly(owner: Symbol, name: TermName): Boolean =
    name == nme.PACKAGE &&
    (owner.fullName == "scala" || owner.fullName == "scala.reflect")

  /** Initialize toplevel class and module symbols in `owner` from class path representation `classRep`
   */
  def initializeFromClassPath(owner: Symbol, classRep: ClassRepresentation): Unit = {
    ((classRep.binary, classRep.source) : @unchecked) match {
      case (Some(bin), Some(src))
      if platform.needCompile(bin, src) && !binaryOnly(owner, nameOf(classRep)) =>
        if (settings.verbose.value) inform("[symloader] picked up newer source file for " + src.path)
        enterToplevelsFromSource(owner, nameOf(classRep), src)
      case (None, Some(src)) =>
        if (settings.verbose.value) inform("[symloader] no class, picked up source file for " + src.path)
        enterToplevelsFromSource(owner, nameOf(classRep), src)
      case (Some(bin), _) =>
        enterClassAndModule(owner, nameOf(classRep), new ClassfileLoader(bin, _, _))
    }
  }
  private def nameOf(classRep: ClassRepresentation): TermName = {
    val name = classRep.name
    val nameLength = name.length
    if (nameLength <= nameCharBuffer.length) {
      name.getChars(0, nameLength, nameCharBuffer, 0)
      newTermName(nameCharBuffer, 0, nameLength)
    } else {
      newTermName(name)
    }
  }
  private val nameCharBuffer = new Array[Char](512)

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
    def associatedFile(self: Symbol): AbstractFile = NoAbstractFile

    /**
     * Description of the resource (ClassPath, AbstractFile)
     * being processed by this loader
     */
    protected def description: String

    private var ok = false

    private def setSource(sym: Symbol): Unit = {
      sourcefile foreach (sf => sym match {
        case cls: ClassSymbol => cls.associatedFile = sf
        case mod: ModuleSymbol => mod.moduleClass.associatedFile = sf
        case _ => ()
      })
    }

    override def complete(root: Symbol): Unit = {
      val assocFile = associatedFile(root)
      currentRunProfilerBeforeCompletion(root, assocFile)
      try {
        try {
          informingProgress("loaded " + description) {
            val currentphase = phase
            try doComplete(root)
            finally phase = currentphase
          }
          ok = true
          setSource(root)
          setSource(root.companionSymbol) // module -> class, class -> module
        }
        catch {
          case ex@(_: IOException | _: MissingRequirementError) =>
            ok = false
            signalError(root, ex)
        }
        initRoot(root)
        if (!root.isPackageClass) initRoot(root.companionSymbol)
      } finally {
        currentRunProfilerAfterCompletion(root, assocFile)
      }
    }

    override def load(root: Symbol): Unit = { complete(root) }

    private def markAbsent(sym: Symbol): Unit = {
      val tpe: Type = if (ok) NoType else ErrorType

      if (sym != NoSymbol)
        sym setInfo tpe
    }
    private def initRoot(root: Symbol): Unit = {
      if (root.rawInfo == this)
        List(root, root.moduleClass) foreach markAbsent
      else if (root.isClass && !root.isModuleClass)
        root.rawInfo.load(root)
    }
  }

  /**
   * Loads contents of a package
   */
  class PackageLoader(packageName: String, classPath: ClassPath) extends SymbolLoader with FlagAgnosticCompleter {
    protected def description = {
      val shownPackageName = if (packageName == ClassPath.RootPackage) "<root package>" else packageName
      s"package loader $shownPackageName"
    }

    protected def doComplete(root: Symbol): Unit = {
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
  private lazy val classFileDataReader: ReusableInstance[ReusableDataReader] = ReusableInstance[ReusableDataReader](new ReusableDataReader(), initialSize = 1, enabled = isCompilerUniverse)
  class ClassfileLoader(val classfile: AbstractFile, clazz: ClassSymbol, module: ModuleSymbol) extends SymbolLoader with FlagAssigningCompleter {
    private object classfileParser extends {
      val symbolTable: SymbolLoaders.this.symbolTable.type = SymbolLoaders.this.symbolTable
    } with ClassfileParser(classFileDataReader) {
      override protected def lookupMemberAtTyperPhaseIfPossible(sym: Symbol, name: Name): Symbol =
        SymbolLoaders.this.lookupMemberAtTyperPhaseIfPossible(sym, name)
      /*
       * The type alias and the cast (where the alias is used) is needed due to problem described
       * in scala/bug#7585. In this particular case, the problem is that we need to make sure that symbol
       * table used by symbol loaders is exactly the same as they one used by classfileParser.
       * If you look at the path-dependent types we have here everything should work out ok but
       * due to issue described in scala/bug#7585 type-checker cannot tie the knot here.
       *
       */
      private type SymbolLoadersRefined = SymbolLoaders { val symbolTable: classfileParser.symbolTable.type }

      val loaders = SymbolLoaders.this.asInstanceOf[SymbolLoadersRefined]

      override def classPath: ClassPath = platform.classPath
    }

    protected def description = "class file "+ classfile.toString

    protected def doComplete(root: Symbol): Unit = {
      val start = if (settings.areStatisticsEnabled) statistics.startTimer(statistics.classReadNanos) else null
      classfileParser.parse(classfile, clazz, module)
      if (clazz.associatedFile eq NoAbstractFile) clazz.associatedFile = classfile
      if (module.associatedFile eq NoAbstractFile) module.associatedFile = classfile
      if (settings.areStatisticsEnabled) statistics.stopTimer(statistics.classReadNanos, start)
    }
    override def sourcefile: Option[AbstractFile] = classfileParser.srcfile
    override def associatedFile(self: Symbol): AbstractFile = classfile
  }

  class SourcefileLoader(val srcfile: AbstractFile) extends SymbolLoader with FlagAssigningCompleter {
    protected def description = "source file "+ srcfile.toString
    override def fromSource = true
    override def sourcefile = Some(srcfile)
    override def associatedFile(self: Symbol): AbstractFile = srcfile
    protected def doComplete(root: Symbol): Unit = compileLate(srcfile)
  }

  object moduleClassLoader extends SymbolLoader with FlagAssigningCompleter {
    protected def description = "module class loader"
    protected def doComplete(root: Symbol): Unit = { root.sourceModule.initialize }
    override def associatedFile(self: Symbol): AbstractFile = {
      val sourceModule = self.sourceModule
      sourceModule.rawInfo match {
        case loader: SymbolLoader => loader.associatedFile(sourceModule)
        case _ => super.associatedFile(self)
      }
    }
  }

  /** used from classfile parser to avoid cycles */
  var parentsLevel = 0
  var pendingLoadActions: List[() => Unit] = Nil
}
