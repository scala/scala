/* NSC -- new Scala compiler
 * Copyright 2005-2008 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.symtab

import java.io.{File, IOException}

import ch.epfl.lamp.compiler.msil.{Type => MSILType, Attribute => MSILAttribute}

import scala.collection.mutable.{HashMap, HashSet}
import scala.compat.Platform.currentTime
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.{Position, NoPosition}
import classfile.ClassfileParser
import Flags._

/** This class ...
 *
 *  @author  Martin Odersky
 *  @version 1.0
 */
abstract class SymbolLoaders {
  val global: Global
  import global._

  /** A lazy type that completes itself by calling parameter doComplete.
   *  Any linked modules/classes or module classes are also initialized.
   *
   *  @param doComplete    The type completion procedure to be run.
   *                       It takes symbol to compkete as parameter and returns
   *                       name of file loaded for completion as a result.
   *                       Can throw an IOException on error.
   */
  abstract class SymbolLoader extends LazyType {

    /** Load source or class file for `root', return */
    protected def doComplete(root: Symbol): Unit

    /** The kind of file that's processed by this loader */
    protected def kindString: String

    private var ok = false

    def sourceFile: AbstractFile = null
    protected def sourceString: String

    override def complete(root: Symbol) : Unit = {
      if (inIDE && root.owner != NoSymbol) {
        assert(root.rawInfo == this)
        if (root.isModuleClass) {
          val clazz = root.sourceModule.linkedClassOfModule
          assert(root.rawInfo == this)
          if (clazz != NoSymbol && !clazz.rawInfo.isInstanceOf[SymbolLoader]) {
            // bail
            assert(true)
            root.setInfo(ErrorType)
            Console.println("ditch " + root)
            return
          }
        } else if (root.isClass) {
          val module = root.linkedModuleOfClass
          assert(root.rawInfo == this)
          if (module != NoSymbol && !module.rawInfo.isInstanceOf[SymbolLoader]) {
            assert(true)
            root.setInfo(ErrorType)
            Console.println("ditch " + root)
            return
          }
        } else {
          assert(root.isModule)
          assert(true)
        }
      }
      try {
        val start = currentTime
        val currentphase = phase
        doComplete(root)
        phase = currentphase
        def source = kindString + " " + sourceString
        informTime("loaded " + source, start)
        //if (root.rawInfo == this && root.linkedSym.rawInfo == this)
        //  throw new TypeError(source + " does not define " + root)
        ok = true
      } catch {
        case ex: IOException =>
          ok = false
          if (settings.debug.value) ex.printStackTrace()
          val msg = ex.getMessage()
          error(
            if (msg eq null) "i/o error while loading " + root.name
            else "error while loading " + root.name + ", " + msg);
      }
      initRoot(root)
      if (!root.isPackageClass) initRoot(root.linkedSym)
    }

    override def load(root: Symbol) { complete(root) }

    private def initRoot(root: Symbol) {
      if (root.rawInfo == this) {
        def markAbsent(sym: Symbol) =
          if (sym != NoSymbol) sym.setInfo(if (ok) NoType else ErrorType);
        markAbsent(root)
        markAbsent(root.moduleClass)
      } else if (root.isClass && !root.isModuleClass) root.rawInfo.load(root)
    }
  }

  /** Load contents of a package
   */
  class PackageLoader(val directory: global.classPath0.Context) extends SymbolLoader {

    // XXX: for IDE.
    protected def sourceString = directory.toString()

    protected def kindString: String = "directory path"

    protected def newPackageLoader(dir: global.classPath0.Context): PackageLoader =
      new PackageLoader(dir)

    protected def checkSource(name: String, source: AbstractFile): Boolean = true

    var root: Symbol = _

    def enterPackage(name: String, completer: SymbolLoader) {
      if (inIDE && root.info.decls.lookup(newTermName(name)) != NoSymbol) {
        return // refresh
      }
      val pkg = root.newPackage(NoPosition, newTermName(name))
      pkg.moduleClass.setInfo(completer)
      pkg.setInfo(pkg.moduleClass.tpe)
      root.info.decls.enter(pkg)
    }
    // @return - the symbol of the class
    def enterClassAndModule(name: String, completer: SymbolLoader): Symbol = {
      val owner = if (root.isRoot) definitions.EmptyPackageClass else root
      val className = newTermName(name)
      assert(owner.info.decls.lookup(name) == NoSymbol, owner.fullNameString + "." + name)
      var clazz = owner.newClass(NoPosition, name.toTypeName)
      var module = owner.newModule(NoPosition, name)
      clazz setInfo completer
      module setInfo completer
      module.moduleClass setInfo moduleClassLoader
      clazz = (owner.info.decls enter clazz).asInstanceOf[ClassSymbol]
      module = (owner.info.decls enter module).asInstanceOf[ModuleSymbol]
      assert(clazz.linkedModuleOfClass == module, module)
      assert(module.linkedClassOfModule == clazz, clazz)
      clazz
    }
    def checkAdd(name0 : String) = {
      var name = name0
      while ((name indexOf '$') != -1) {
        name = name.substring(0, name indexOf '$')
      }
    }
    lazy val scope = newPackageScope(computeDepends(this))
    protected def doComplete(root: Symbol) {
      assert(root.isPackageClass, root)
      this.root = root
      root.setInfo(new PackageClassInfoType(scope, root, this))
      refresh
    }
    def refresh = {
      /** Is the given name a valid input file base name? */
      def isValid(name: String): Boolean =
        name.length() > 0 && !name.endsWith("$class") && (/*settings.XO.value*/true ||
          (name.indexOf("$anon") == -1));

      val classes  = new HashMap[String, global.classPath0.Context]
      val packages = new HashMap[String, global.classPath0.Context]
      for (dir <- directory.entries) if (dir.location ne null) {
        for (file <- dir.location) {
          if (file.isDirectory && directory.validPackage(file.name) && !packages.isDefinedAt(file.name))
            packages(file.name) = directory.find(file.name, true);
          else if (!global.forMSIL && !file.isDirectory && file.name.endsWith(".class")) {
            val name = file.name.substring(0, file.name.length() - (".class").length());
            if (isValid(name) && !classes.isDefinedAt(name)) {
              val clazz = directory.find(name, false)
              if (clazz ne null) classes(name) = clazz
            }
          }
        }
      }
      for (dir <- directory.entries) if (dir.source ne null) {
        for (file <- dir.source.location) {
          if (file.isDirectory && directory.validPackage(file.name) && !packages.isDefinedAt(file.name))
            packages(file.name) = directory.find(file.name, true)
          else if (dir.source.compile && !file.isDirectory && file.name.endsWith(".scala")) {
            val name = file.name.substring(0, file.name.length() - (".scala").length())
            if (isValid(name) && !classes.isDefinedAt(name)) {
              val source = directory.find(name, false)
              if ((source ne null) && (source.sourceFile ne null))
                if (checkSource(name, source.sourceFile))
                  classes(name) = source
                else if (settings.debug.value)
                  Console.println("Skipping source file " + source.sourceFile)
            }
          }
        }
      }

      // do classes first
      for ((name, file) <- classes.elements) {
        val loader = if (!file.isSourceFile) {
          new ClassfileLoader(file.classFile, file.sourceFile, file.sourcePath)
        } else {
          assert(file.sourceFile ne null)
          new SourcefileLoader(file.sourceFile)
        }
        enterClassAndModule(name, loader)
      }
      for ((name, file) <- packages.elements)
        enterPackage(name, newPackageLoader(file))
    }
  }

  class NamespaceLoader(directory: global.classPath0.Context) extends PackageLoader(directory) {

    override protected def kindString: String = "namespace " + namespace

    override protected def sourceString = ""

    override def newPackageLoader(dir: global.classPath0.Context): PackageLoader =
      new NamespaceLoader(dir)

    val types = new HashMap[String, MSILType]()

    val namespaces = new HashSet[String]()

    def namespace: String = if (root.isRoot) "" else root.fullNameString

    // TODO: Add check whether the source is newer than the assembly
    override protected def checkSource(name: String, source: AbstractFile): Boolean =
      !types.contains(name)

    override protected def doComplete(root: Symbol) {
      clrTypes.collectMembers(root, types, namespaces)

      super.doComplete(root)

      for (namespace <- namespaces.elements) {
        val oldPkg = root.info.decls lookup newTermName(namespace)
        if (oldPkg == NoSymbol)
          enterPackage(namespace, new NamespaceLoader(new classPath0.Context(List())))
        //else System.out.println("PackageLoader: package already in scope: " + oldPkg.fullNameString)
      }

      // import the CLR types contained in the package (namespace)
      for ((name, typ) <- types.elements) {
        assert(namespace == typ.Namespace, typ.FullName)

        if (typ.IsDefined(clrTypes.SCALA_SYMTAB_ATTR, false)) {
          val attrs = typ.GetCustomAttributes(clrTypes.SCALA_SYMTAB_ATTR, false)
          assert (attrs.length == 1, attrs.length)
          val a = attrs(0).asInstanceOf[MSILAttribute]
          if (a.getConstructor() == clrTypes.SYMTAB_CONSTR)
            enterClassAndModule(name, new MSILTypeLoader(typ))
        }
        else
          enterClassAndModule(name, new MSILTypeLoader(typ))
      }
    }
  }  // NamespaceLoader

  class MSILTypeLoader(typ: MSILType) extends SymbolLoader {
    private object typeParser extends clr.TypeParser {
      val global: SymbolLoaders.this.global.type = SymbolLoaders.this.global
    }
    protected def doComplete(root: Symbol) {
      typeParser.parse(typ, root.asInstanceOf[typeParser.global.loaders.clrTypes.global.Symbol]) // don't check this
    }
    protected def kindString: String = typ.FullName
    protected def sourceString = typ.Assembly.FullName
  }
  // IDE hook.
  protected def completeClassfile(root : Symbol, loader : ClassfileLoader)(f : => Unit) : Unit = f
  import scala.collection.jcl
  // incremental builder hook
  protected def computeDepends(loader : PackageLoader) : PackageScopeDependMap = {
    null
  }

  class ClassfileLoader(val classFile: AbstractFile, override val sourceFile: AbstractFile, sourcePath0: AbstractFile) extends SymbolLoader {
    private object classfileParser extends ClassfileParser {
      val global: SymbolLoaders.this.global.type = SymbolLoaders.this.global
      override def sourcePath = sourcePath0 /* could be null */
    }
    protected def doComplete(root: Symbol) {
      completeClassfile(root, this) {
        classfileParser.parse(classFile, root)
      }
      root match {
        case clazz: ClassSymbol =>
          if ((sourceFile ne null) && (clazz.sourceFile eq null))
            clazz.sourceFile = sourceFile
        case module: ModuleSymbol if module.moduleClass.isInstanceOf[ClassSymbol] =>
          val clazz = module.moduleClass.asInstanceOf[ClassSymbol]
          if ((sourceFile ne null) && (clazz.sourceFile eq null))
            clazz.sourceFile = sourceFile
        case _ =>
      }
      if (root.sourceFile ne null)
        prepareReset(root, this)
    }
    protected def kindString: String = "class file"
    protected def sourceString = classFile.toString()
  }

  class SourcefileLoader(override val sourceFile: AbstractFile) extends SymbolLoader {
    protected def doComplete(root: Symbol): Unit = global.currentRun.compileLate(sourceFile)
    protected def kindString: String = "source file"
    protected def sourceString = sourceFile.toString()
  }

  object moduleClassLoader extends SymbolLoader {
    protected def doComplete(root: Symbol) { root.sourceModule.initialize }
    protected def kindString: String = ""
    protected def sourceString = ""
  }

  object clrTypes extends clr.CLRTypes {
    val global: SymbolLoaders.this.global.type = SymbolLoaders.this.global
    if (global.forMSIL) init()
  }

}
