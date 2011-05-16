/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package symtab

import java.io.IOException
import ch.epfl.lamp.compiler.msil.{ Type => MSILType, Attribute => MSILAttribute }

import scala.compat.Platform.currentTime
import scala.tools.nsc.util.{ ClassPath }
import classfile.ClassfileParser
import reflect.internal.Flags._
import util.Statistics._

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

  private def realOwner(root: Symbol): Symbol = {
    if (root.isRoot) definitions.EmptyPackageClass else root
  }

  /** Enter class with given `name` into scope of `root`
   *  and give them `completer` as type.
   */
  def enterClass(root: Symbol, name: String, completer: SymbolLoader): Symbol = {
    val owner = realOwner(root)
    val clazz = owner.newClass(NoPosition, newTypeName(name))
    clazz setInfo completer
    enterIfNew(owner, clazz, completer)
  }

  /** Enter module with given `name` into scope of `root`
   *  and give them `completer` as type.
   */
  def enterModule(root: Symbol, name: String, completer: SymbolLoader): Symbol = {
    val owner = realOwner(root)
    val module = owner.newModule(NoPosition, newTermName(name))
    module setInfo completer
    module.moduleClass setInfo moduleClassLoader
    enterIfNew(owner, module, completer)
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

  /**
   * A lazy type that completes itself by calling parameter doComplete.
   * Any linked modules/classes or module classes are also initialized.
   */
  abstract class SymbolLoader extends LazyType {

    /** Load source or class file for `root', return */
    protected def doComplete(root: Symbol): Unit

    def sourcefile: Option[AbstractFile] = None

    /**
     * Description of the resource (ClassPath, AbstractFile, MSILType)
     * being processed by this loader
     */
    protected def description: String

    private var ok = false

    private def setSource(sym: Symbol) {
      sourcefile map (sf => sym match {
        case cls: ClassSymbol => cls.sourceFile = sf
        case mod: ModuleSymbol => mod.moduleClass.sourceFile = sf
        case _ => ()
      })
    }
    override def complete(root: Symbol) : Unit = {
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
          ok = false
          if (settings.debug.value) ex.printStackTrace()
          val msg = ex.getMessage()
          globalError(
            if (msg eq null) "i/o error while loading " + root.name
            else "error while loading " + root.name + ", " + msg);
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
  abstract class PackageLoader[T](classpath: ClassPath[T]) extends SymbolLoader {
    protected def description = "package loader "+ classpath.name

    def enterPackage(root: Symbol, name: String, completer: SymbolLoader) {
      val preExisting = root.info.decls.lookup(newTermName(name))
      if (preExisting != NoSymbol)
        throw new TypeError(
          root+" contains object and package with same name: "+name+"\none of them needs to be removed from classpath")
      val pkg = root.newPackage(NoPosition, newTermName(name))
      pkg.moduleClass.setInfo(completer)
      pkg.setInfo(pkg.moduleClass.tpe)
      root.info.decls.enter(pkg)
    }

    /**
     * Tells whether a class with both a binary and a source representation
     * (found in classpath and in sourcepath) should be re-compiled. Behaves
     * similar to javac, i.e. if the source file is newer than the classfile,
     * a re-compile is triggered.
     */
    protected def needCompile(bin: T, src: AbstractFile): Boolean

    /**
     * Tells whether a class should be loaded and entered into the package
     * scope. On .NET, this method returns `false' for all synthetic classes
     * (anonymous classes, implementation classes, module classes), their
     * symtab is encoded in the pickle of another class.
     */
    protected def doLoad(cls: classpath.AnyClassRep): Boolean

    protected def newClassLoader(bin: T): SymbolLoader

    protected def newPackageLoader(pkg: ClassPath[T]): SymbolLoader

    protected def doComplete(root: Symbol) {
      assert(root.isPackageClass, root)
      root.setInfo(new PackageClassInfoType(new Scope(), root))

      val sourcepaths = classpath.sourcepaths
      for (classRep <- classpath.classes if doLoad(classRep)) {
        ((classRep.binary, classRep.source) : @unchecked) match {
          case (Some(bin), Some(src)) if needCompile(bin, src) =>
            if (settings.verbose.value) inform("[symloader] picked up newer source file for " + src.path)
            enterToplevelsFromSource(root, classRep.name, src)
          case (None, Some(src)) =>
            if (settings.verbose.value) inform("[symloader] no class, picked up source file for " + src.path)
            enterToplevelsFromSource(root, classRep.name, src)
          case (Some(bin), _) =>
            enterClassAndModule(root, classRep.name, newClassLoader(bin))
        }
      }

      for (pkg <- classpath.packages) {
        enterPackage(root, pkg.name, newPackageLoader(pkg))
      }

      // if there's a $member object, enter its members as well.
      val pkgModule = root.info.decl(nme.PACKAGEkw)
      if (pkgModule.isModule && !pkgModule.rawInfo.isInstanceOf[SourcefileLoader]) {
        // println("open "+pkgModule)//DEBUG
        openPackageModule(pkgModule)()
      }
    }
  }

  def openPackageModule(module: Symbol)(packageClass: Symbol = module.owner): Unit = {
    // unlink existing symbols in the package
    for (member <- module.info.decls.iterator) {
      if (!member.isPrivate && !member.isConstructor) {
        // todo: handle overlapping definitions in some way: mark as errors
        // or treat as abstractions. For now the symbol in the package module takes precedence.
        for (existing <- packageClass.info.decl(member.name).alternatives)
          packageClass.info.decls.unlink(existing)
      }
    }
    // enter non-private decls the class
    for (member <- module.info.decls.iterator) {
      if (!member.isPrivate && !member.isConstructor) {
        packageClass.info.decls.enter(member)
      }
    }
    // enter decls of parent classes
    for (pt <- module.info.parents; val p = pt.typeSymbol) {
      if (p != definitions.ObjectClass && p != definitions.ScalaObjectClass) {
        openPackageModule(p)(packageClass)
      }
    }
  }

  class JavaPackageLoader(classpath: ClassPath[AbstractFile]) extends PackageLoader(classpath) {
    protected def needCompile(bin: AbstractFile, src: AbstractFile) =
      (src.lastModified >= bin.lastModified)

    protected def doLoad(cls: classpath.AnyClassRep) = true

    protected def newClassLoader(bin: AbstractFile) =
      new ClassfileLoader(bin)

    protected def newPackageLoader(pkg: ClassPath[AbstractFile]) =
      new JavaPackageLoader(pkg)
  }

  class NamespaceLoader(classpath: ClassPath[MSILType]) extends PackageLoader(classpath) {
    protected def needCompile(bin: MSILType, src: AbstractFile) =
      false // always use compiled file on .net

    protected def doLoad(cls: classpath.AnyClassRep) = {
      if (cls.binary.isDefined) {
        val typ = cls.binary.get
        if (typ.IsDefined(clrTypes.SCALA_SYMTAB_ATTR, false)) {
          val attrs = typ.GetCustomAttributes(clrTypes.SCALA_SYMTAB_ATTR, false)
          assert (attrs.length == 1, attrs.length)
          val a = attrs(0).asInstanceOf[MSILAttribute]
          // symtab_constr takes a byte array argument (the pickle), i.e. typ has a pickle.
          // otherwise, symtab_default_constr was used, which marks typ as scala-synthetic.
          a.getConstructor() == clrTypes.SYMTAB_CONSTR
        } else true // always load non-scala types
      } else true // always load source
    }

    protected def newClassLoader(bin: MSILType) =
      new MSILTypeLoader(bin)

    protected def newPackageLoader(pkg: ClassPath[MSILType]) =
      new NamespaceLoader(pkg)
  }

  class ClassfileLoader(val classfile: AbstractFile) extends SymbolLoader {
    private object classfileParser extends ClassfileParser {
      val global: SymbolLoaders.this.global.type = SymbolLoaders.this.global
    }

    protected def description = "class file "+ classfile.toString

    protected def doComplete(root: Symbol) {
      val start = startTimer(classReadNanos)
      classfileParser.parse(classfile, root)
      stopTimer(classReadNanos, start)
    }
    override def sourcefile: Option[AbstractFile] = classfileParser.srcfile
  }

  class MSILTypeLoader(typ: MSILType) extends SymbolLoader {
    private object typeParser extends clr.TypeParser {
      val global: SymbolLoaders.this.global.type = SymbolLoaders.this.global
    }

    protected def description = "MSILType "+ typ.FullName + ", assembly "+ typ.Assembly.FullName
    protected def doComplete(root: Symbol) { typeParser.parse(typ, root) }
  }

  class SourcefileLoader(val srcfile: AbstractFile) extends SymbolLoader {
    protected def description = "source file "+ srcfile.toString
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
