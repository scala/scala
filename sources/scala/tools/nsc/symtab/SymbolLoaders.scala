package scala.tools.nsc.symtab;

import java.io.IOException;
import scala.tools.util.{AbstractFile, Position}
import scala.tools.nsc.util.NameTransformer;
import scala.collection.mutable.HashMap;
import classfile.ClassfileParser;
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
  class SymbolLoader(doComplete: Symbol => String) extends LazyType {
    private var ok = false;
    override def complete(root: Symbol): unit = {
      try {
        val start = System.currentTimeMillis();
        val currentphase = phase;
        phase = analyzerPhase;
        val source = doComplete(root);
        phase = currentphase;
        informTime("loaded " + source, start);
        if (root.rawInfo != this) ok = true
        else error(source + " does not define " + root)
      } catch {
        case ex: IOException =>
	  if (settings.debug.value) ex.printStackTrace();
          val msg = ex.getMessage();
          error(
            if (msg == null) "i/o error while loading " + root.name
            else "error while loading " + root.name + ", " + msg);
      }
      initRoot(root);
      if (!root.isPackageClass)
        initRoot(if (root.isModule) root.linkedClass else root.linkedModule);
    }

    private def initRoot(root: Symbol): unit = {
      if (root.rawInfo == this)
        root.setInfo(if (ok) NoType else ErrorType);
      if (root.isModule) initRoot(root.moduleClass)
    }
  }

  /** Load contents of a package
   */
  def packageLoader(directory: AbstractFile): SymbolLoader =
    new SymbolLoader(root => {
      System.out.println("loading " + root);//debug
      assert(root.isPackageClass, root);
      val members = new Scope();
      root.setInfo(new PackageClassInfoType(members, root));

      def enterPackage(str: String, completer: SymbolLoader): unit = {
        val pkg = root.newPackage(Position.NOPOS, newTermName(str));
        pkg.moduleClass.setInfo(completer);
        pkg.setInfo(pkg.moduleClass.tpe);
        members.enter(pkg)
      }

      def enterClassAndModule(str: String, completer: SymbolLoader): unit = {
	val name = newTermName(str);
        val clazz = root.newClass(Position.NOPOS, name.toTypeName);
        val module = root.newModule(Position.NOPOS, name);
        clazz.setInfo(completer);
	module.setInfo(completer);
        module.moduleClass.setInfo(errorLoader);
        members.enter(clazz);
        members.enter(module);
	assert(clazz.linkedModule == module);
	assert(module.linkedClass == clazz);
      }

      val sources  = new HashMap[String, AbstractFile];
      val classes  = new HashMap[String, AbstractFile];
      val packages = new HashMap[String, AbstractFile];
      val it = directory.list();
      while (it.hasNext()) {
        val file = it.next().asInstanceOf[AbstractFile];
        val filename = file.getName();
        if (file.isDirectory()) {
          if (filename != "META_INF") {
            if (!packages.isDefinedAt(filename)) packages(filename) = file;
          }
        } else if (!root.isRoot && filename.endsWith(".class")) {
          val name = filename.substring(0, filename.length() - 6);
          if (!classes.isDefinedAt(name)) classes(name) = file;
        } else if (!root.isRoot && filename.endsWith(".scala")) {
          val name = filename.substring(0, filename.length() - 6);
          if (!sources.isDefinedAt(name)) sources(name) = file;
        }
      }
      for (val Pair(name, sfile) <- sources.elements) {
        classes.get(name) match {
          case Some(cfile) if (cfile.lastModified() > sfile.lastModified()) => {}
          case _ => enterClassAndModule(name, sourcefileLoader(sfile));
        }
      }
      for (val Pair(name, cfile) <- classes.elements) {
        sources.get(name) match {
          case Some(sfile) if (sfile.lastModified() > cfile.lastModified()) => {}
          case _ => enterClassAndModule(name, classfileLoader(cfile));
        }
      }
      for (val Pair(name, file) <- packages.elements) {
        if (!sources.contains(name) && !classes.contains(name))
          enterPackage(name, packageLoader(file));
      }

      "directory path '" + directory + "'"
    });

  private object classfileParser extends ClassfileParser {
    val global: SymbolLoaders.this.global.type = SymbolLoaders.this.global;
  }

  def classfileLoader(file: AbstractFile) =
    new SymbolLoader(root => {
      classfileParser.parse(file, root);
      "class file '" + file + "'";
    });

  def sourcefileLoader(file: AbstractFile) =
    new SymbolLoader(root => {
      global.compileLate(file);
      "source file '" + file + "'";
    });

  val errorLoader =
    new SymbolLoader(root => {
      throw new Error(" loading " + root + " without loading module first");
    });
}
