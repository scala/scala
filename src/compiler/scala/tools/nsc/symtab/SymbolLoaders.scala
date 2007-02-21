/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.symtab

import compat.Platform.currentTime
import java.io.{File, IOException}
import scala.collection.mutable.{HashMap, HashSet}
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.{ClassPath, NameTransformer, Position}
import classfile.{ClassfileParser, SymblfileParser}
import Flags._
import ch.epfl.lamp.compiler.msil.{Type => MSILType, Attribute => MSILAttribute};

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
   *  @param doComplete    The type completion procedure to be run.
   *                       It takes symbol to compkete as parameter and returns
   *                       name of file loaded for completion as a result.
   *                       Can throw an IOException on error.
   */
  abstract class SymbolLoader extends LazyType {

    /** Load source or class file for `root', return */
    protected def doComplete(root: Symbol): unit

    /** The kind of file that's processed by this loader */
    protected def kindString: String

    private var ok = false
/*
    private def setSource(sym: Symbol, sourceFile0: AbstractFile): unit = sym match {
      case clazz: ClassSymbol => if (sourceFile0 ne null) clazz.sourceFile = sourceFile0;
      case _ => if (sourceFile0 ne null) if (false) System.err.println("YYY: " + sym + " " + sourceFile0);
    }
*/
    def sourceFile : AbstractFile = null
    protected def sourceString : String

    override def complete(root: Symbol): unit = {
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
          if (settings.debug.value) ex.printStackTrace();
          val msg = ex.getMessage()
          error(
            if (msg eq null) "i/o error while loading " + root.name
            else "error while loading " + root.name + ", " + msg);
      }
      initRoot(root)
      if (!root.isPackageClass) initRoot(root.linkedSym)
    }

    override def load(root: Symbol): unit = complete(root)

    private def initRoot(root: Symbol): unit = {
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
  class PackageLoader(directory: global.classPath0.Context) extends SymbolLoader {

    protected def sourceString = directory.toString()

    protected def kindString: String = "directory path"

    protected def newPackageLoader(dir: global.classPath0.Context): PackageLoader =
      new PackageLoader(dir)

    protected def checkSource(name: String, source: AbstractFile): Boolean = true;

    protected var root: Symbol = _

    def enterPackage(name: String, completer: SymbolLoader): unit = {
      val pkg = root.newPackage(NoPos, newTermName(name))
      pkg.moduleClass.setInfo(completer)
      pkg.setInfo(pkg.moduleClass.tpe)
      root.info.decls.enter(pkg)
    }

    // @return - the symbol of the class
    def enterClassAndModule(name: String, completer: SymbolLoader): Symbol = {
      val owner = if (root.isRoot) definitions.EmptyPackageClass else root
      val className = newTermName(name)
      assert(owner.info.decls.lookup(name) == NoSymbol, owner.fullNameString + "." + name)
      val clazz = owner.newClass(NoPos, name.toTypeName)
      val module = owner.newModule(NoPos, name)
      clazz.setInfo(completer)
      module.setInfo(completer)
      module.moduleClass.setInfo(moduleClassLoader)
      owner.info.decls.enter(clazz)
      owner.info.decls.enter(module)
//       if (completer.sourceFile != null) {
//         clazz.sourceFile = completer.sourceFile;
//         module.moduleClass.sourceFile = completer.sourceFile
//       }
      assert(clazz.linkedModuleOfClass == module, module)
      assert(module.linkedClassOfModule == clazz, clazz)
      //System.out.println("Added class " + clazz.fullNameString);
      clazz
    }

    protected def doComplete(root: Symbol): unit = {
      assert(root.isPackageClass, root)
      this.root = root
      val scope = newScope
      root.setInfo(new PackageClassInfoType(scope, root))

      /** Is the given name a valid input file base name? */
      def isValid(name: String): boolean =
        name.length() > 0 && (settings.XbytecodeRead.value ||
          (!name.endsWith("$class") && name.indexOf("$anon") == -1));

      val classes  = new HashMap[String, global.classPath0.Context]
      val packages = new HashMap[String, global.classPath0.Context]
      for (val dir <- directory.entries) if (dir.location ne null) {
        for (val file <- dir.location) {
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
      for (val dir <- directory.entries) if (dir.source ne null) {
        for (val file <- dir.source.location) {
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
      for (val (name, file) <- classes.elements) {
        val loader = if (!file.isSourceFile) {
          new ClassfileLoader(file.classFile, file.sourceFile, file.sourcePath)
        } else {
          assert(file.sourceFile ne null)
          new SourcefileLoader(file.sourceFile)
        }
        enterClassAndModule(name, loader)
      }

      for (val (name, file) <- packages.elements)
        enterPackage(name, newPackageLoader(file))
    }
  }

/*
  private object symblfileParser extends SymblfileParser {
    val global: SymbolLoaders.this.global.type = SymbolLoaders.this.global;
  }
*/

  class NamespaceLoader(directory: global.classPath0.Context) extends PackageLoader(directory) {

    override protected def kindString: String = "namespace " + namespace

    override protected def sourceString = "";

    override def newPackageLoader(dir: global.classPath0.Context): PackageLoader =
      new NamespaceLoader(dir);

    val types = new HashMap[String,MSILType]();

    val namespaces = new HashSet[String]();

    def namespace: String = if (root.isRoot) "" else root.fullNameString;

    // TODO: Add check whether the source is newer than the assembly
    override protected def checkSource(name: String, source: AbstractFile): Boolean = {
      !types.contains(name)
    }

    override protected def doComplete(root: Symbol): Unit = {
      clrTypes.collectMembers(root, types, namespaces);

      super.doComplete(root);

      for (val namespace <- namespaces.elements) {
        val oldPkg = root.info.decls.lookup(newTermName(namespace));
        if (oldPkg == NoSymbol)
          enterPackage(namespace, new NamespaceLoader(new classPath0.Context(List())));
        //else System.out.println("PackageLoader: package already in scope: " + oldPkg.fullNameString);
      }

      // import the CLR types contained in the package (namespace)
      for (val (name, typ) <- types.elements) {
        assert(namespace == typ.Namespace, typ.FullName);

        if (typ.IsDefined(clrTypes.SCALA_SYMTAB_ATTR, false)) {
          val attrs = typ.GetCustomAttributes(clrTypes.SCALA_SYMTAB_ATTR, false);
          assert (attrs.length == 1, attrs.length);
          val a = attrs(0).asInstanceOf[MSILAttribute];
          if (a.getConstructor() == clrTypes.SYMTAB_CONSTR)
            enterClassAndModule(name, new MSILTypeLoader(typ));
        }
        else
          enterClassAndModule(name, new MSILTypeLoader(typ));
      }
    }
  }  // NamespaceLoader

  class MSILTypeLoader(typ: MSILType) extends SymbolLoader {
    private object typeParser extends clr.TypeParser {
      val global: SymbolLoaders.this.global.type = SymbolLoaders.this.global;
    }
    protected def doComplete(root: Symbol): Unit = {
      typeParser.parse(typ, root);
    }
    protected def kindString: String = typ.FullName;
    protected def sourceString = typ.Assembly.FullName;
  }

  class ClassfileLoader(classFile: AbstractFile, override val sourceFile: AbstractFile, sourcePath0: AbstractFile) extends SymbolLoader {
    //throw new Error("classfile = " + classFile + "; sourcefile = " + sourceFile + "; sourcepath = " + sourcePath0)
    private object classfileParser extends ClassfileParser {
      val global: SymbolLoaders.this.global.type = SymbolLoaders.this.global
      override def sourcePath = sourcePath0 /* could be null */
    }
    protected def doComplete(root: Symbol): unit = {
      classfileParser.parse(classFile, root)
      if (sourceFile ne null) root match {
        case clazz : ClassSymbol => clazz.sourceFile = sourceFile
        case _ =>
      }
    }
    protected def kindString: String = "class file"
    protected def sourceString = classFile.toString()
  }
/*
  class SymblfileLoader(file: AbstractFile) extends SymbolLoader(file) {
    protected def doComplete(root: Symbol): unit = symblfileParser.parse(file, root);
    protected def kindString: String = "symbl file";
  }
*/
  class SourcefileLoader(override val sourceFile: AbstractFile) extends SymbolLoader {
    protected def doComplete(root: Symbol): unit =
      global.currentRun.compileLate(sourceFile)
    protected def kindString: String = "source file"
    protected def sourceString = sourceFile.toString()
  }

  object moduleClassLoader extends SymbolLoader {
    protected def doComplete(root: Symbol): unit = root.sourceModule.initialize
    protected def kindString: String = "";
    protected def sourceString = "";
  }

  object clrTypes extends clr.CLRTypes {
    val global: SymbolLoaders.this.global.type = SymbolLoaders.this.global;
    if (global.forMSIL) init();
  }

}
