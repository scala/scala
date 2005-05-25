/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.symtab;

import java.io.IOException;
import scala.tools.util.{AbstractFile, Position}
import scala.tools.nsc.util.NameTransformer;
import scala.collection.mutable.HashMap;
import classfile.{ClassfileParser, SymblfileParser};
import Flags._;


abstract class SymbolLoaders {
  val global: Global;
  import global._;

  /** A lazy type that completes itself by calling parameter doComplete.
   *  Any linked modules/classes or module classes are also initialized.
   *  @param doComplete    The type completion procedure to be run.
   *                       It takes symbol to compkete as parameter and returns
   *                       name of file loaded for completion as a result.
   *                       Can throw an IOException on error.
   */
  abstract class SymbolLoader(file: AbstractFile) extends LazyType {
    /** Load source or class file for `root', return */
    protected def doComplete(root: Symbol): unit;
    /** The kind of file that's processed by this loader */
    protected def kindString: String;
    private var ok = false;
    private def setSource(sym: Symbol): unit = sym match {
      case clazz: ClassSymbol => clazz.sourceFile = file;
      case _ =>
    }
    override def complete(root: Symbol): unit = {
      try {
        val start = System.currentTimeMillis();
        val currentphase = phase;
        phase = firstPhase;
        doComplete(root);
        phase = currentphase;
	def source = kindString + " " + file;
        informTime("loaded " + source, start);
        if (root.rawInfo != this) {
	  ok = true;
	  setSource(root.linkedModule);
	  setSource(root.linkedClass);
	} else error(source + " does not define " + root)
      } catch {
        case ex: IOException =>
	  if (settings.debug.value) ex.printStackTrace();
          val msg = ex.getMessage();
          error(
            if (msg == null) "i/o error while loading " + root.name
            else "error while loading " + root.name + ", " + msg);
      }
      initRoot(root);
      if (!root.isPackageClass) initRoot(root.linkedSym);
    }
    override def load(root: Symbol): unit = complete(root);

    private def initRoot(root: Symbol): unit = {
      if (root.rawInfo == this) {
        root.setInfo(if (ok) NoType else ErrorType);
        if (root.isModule)
          root.moduleClass.setInfo(if (ok) NoType else ErrorType)
      }
      if (root.isClass && !root.isModuleClass) root.rawInfo.load(root)
    }
  }

  /** Load contents of a package
   */
  class PackageLoader(directory: AbstractFile) extends SymbolLoader(directory) {
    protected def doComplete(root: Symbol): unit = {
      assert(root.isPackageClass, root);
      root.setInfo(new PackageClassInfoType(new Scope(), root));

      def enterPackage(str: String, completer: SymbolLoader): unit = {
        val pkg = root.newPackage(Position.NOPOS, newTermName(str));
        pkg.moduleClass.setInfo(completer);
        pkg.setInfo(pkg.moduleClass.tpe);
        root.info.decls.enter(pkg)
      }

      def enterClassAndModule(str: String, completer: SymbolLoader): unit = {
	val owner = if (root.isRoot) definitions.EmptyPackageClass else root;
	val name = newTermName(str);
        val clazz = owner.newClass(Position.NOPOS, name.toTypeName);
        val module = owner.newModule(Position.NOPOS, name);
        clazz.setInfo(completer);
	module.setInfo(completer);
        module.moduleClass.setInfo(errorLoader);
        owner.info.decls.enter(clazz);
        owner.info.decls.enter(module);
	assert(clazz.linkedModule == module, module);
	assert(module.linkedClass == clazz, clazz);
      }

      val sources  = new HashMap[String, AbstractFile];
      val classes  = new HashMap[String, AbstractFile];
      val packages = new HashMap[String, AbstractFile];
      val it = directory.list();
      while (it.hasNext()) {
        val file = it.next().asInstanceOf[AbstractFile];
        val filename = file.getName();
        if (file.isDirectory()) {
          if (filename != "META_INF" && !packages.isDefinedAt(filename)) packages(filename) = file;
        } else if (filename.endsWith(".symbl")) {
          val name = filename.substring(0, filename.length() - 6);
          if (!classes.isDefinedAt(name) ||
	      classes(name).getName().endsWith(".class")) classes(name) = file;
        } else if (filename.endsWith(".class")) {
          val name = filename.substring(0, filename.length() - 6);
          if (!classes.isDefinedAt(name)) classes(name) = file;
        } else if (filename.endsWith(".scala")) {
          val name = filename.substring(0, filename.length() - 6);
          if (!sources.isDefinedAt(name)) sources(name) = file;
        }
      }
      for (val Pair(name, sfile) <- sources.elements) {
        classes.get(name) match {
          case Some(cfile) if (cfile.lastModified() >= sfile.lastModified()) => {}
          case _ => enterClassAndModule(name, new SourcefileLoader(sfile));
        }
      }
      for (val Pair(name, cfile) <- classes.elements) {
        sources.get(name) match {
          case Some(sfile) if (sfile.lastModified() > cfile.lastModified()) => {}
          case _ =>
            val loader =
	      if (cfile.getName().endsWith(".symbl")) new SymblfileLoader(cfile)
              else new ClassfileLoader(cfile);
            enterClassAndModule(name, loader)
        }
      }
      for (val Pair(name, file) <- packages.elements) {
        if (!sources.contains(name) && !classes.contains(name))
          enterPackage(name, new PackageLoader(file));
      }
    }
    protected def kindString: String = "directory path"
  }

  private object classfileParser extends ClassfileParser {
    val global: SymbolLoaders.this.global.type = SymbolLoaders.this.global;
  }

  private object symblfileParser extends SymblfileParser {
    val global: SymbolLoaders.this.global.type = SymbolLoaders.this.global;
  }

  class ClassfileLoader(file: AbstractFile) extends SymbolLoader(file) {
    protected def doComplete(root: Symbol): unit = classfileParser.parse(file, root);
    protected def kindString: String = "class file";
  }

  class SymblfileLoader(file: AbstractFile) extends SymbolLoader(file) {
    protected def doComplete(root: Symbol): unit = symblfileParser.parse(file, root);
    protected def kindString: String = "symbl file";
  }

  class SourcefileLoader(file: AbstractFile) extends SymbolLoader(file) {
    protected def doComplete(root: Symbol): unit = global.compileLate(file);
    protected def kindString: String = "source file";
  }

  object errorLoader extends SymbolLoader(null) {
    protected def doComplete(root: Symbol): unit =
      throw new Error(" loading " + root + " without loading module first");
    protected def kindString: String = "";
  }
}
