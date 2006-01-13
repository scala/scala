/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: $

package scala.tools.nsc.util;

import scala.tools.util.AbstractFile;
import scala.collection.mutable.ArrayBuffer;
import java.io.FileNotFoundException;
import java.io.File;
import java.util.StringTokenizer;

/** Richer classpath abstraction than files.
 *  Roughly based on Eclipse's classpath abstractions.
 *
 *  @author Sean McDirmid
 */
object ClassPath {

  class Source(val location: AbstractFile, val compile: Boolean);

  abstract class Entry {
    def location : AbstractFile;
    def source   : Source;
  }

  class Output(val location: AbstractFile, val sourceFile: AbstractFile) extends Entry {
    def source = new Source(sourceFile, true);
  }
  class Library(val location: AbstractFile) extends Entry {
    def doc : AbstractFile = null;
    def sourceFile : AbstractFile = null;
    def source = if (sourceFile == null) null else new Source(sourceFile, false);
  }

  class Context(val classes: List[AbstractFile], val sources: List[Source]) {
    def find(name: String, isDir: boolean) = {
      assert(isPackage);
      def find0(classes: List[AbstractFile], sources: List[Source]): Context = {
	assert(classes.length == sources.length);
	if (classes.isEmpty) new Context(Nil, Nil);
	else {
	  val ret = find0(classes.tail, sources.tail);

	  val clazz = if (classes.head == null) null; else
	    classes.head.lookupPath(name + (if (!isDir) ".class" else ""), isDir);

	  val source = {
	    if (sources.head == null) null;
	    else {
	      val source0 = sources.head.location.lookupPath(name + (if (!isDir) ".scala" else ""), isDir);
	      if (source0 != null) source0;
	      else if (clazz != null && !isDir) sources.head.location; // directory where we can find source.
	      else null;
	    }
	  }
	  if (clazz == null && source == null) ret;
	  else new Context(clazz :: ret.classes,
			   (if (source != null) new Source(source, sources.head.compile) else null)
			     :: ret.sources);
	}
      }
      find0(classes, sources)
    }

    def isPackage = {
      if (classes.head != null) classes.head.isDirectory();
      else sources.head.location.isDirectory();
    }

    def name = {
      val name = if (classes.head != null) classes.head.getName() else sources.head.location.getName();
      if (isPackage) name;
      else name.substring(0, name.length() - (".class").length());
    }

    override def toString(): String = toString(classes, sources);

    def toString(classes0: List[AbstractFile], sources0: List[Source]): String =
      if (classes0.isEmpty) "";
      else
	((if (classes0.head == null) "<none>" else classes0.head.toString()) + (if (sources0.head != null) {
	  "::" + sources0.head.location.toString();
	} else "")) + ":" + toString(classes0.tail, sources0.tail);

    def file = {
      assert(!isPackage);
      if (classes.isEmpty) null;
      else if (sources.head == null || sources.head.location.isDirectory() || !sources.head.compile) {
	assert(classes.head != null);
	classes.head;
      } else if (classes.head == null) {
	sources.head.location;
      } else if (sources.head.location.lastModified() >= classes.head.lastModified()) sources.head.location;
      else classes.head;
    }
    def isSourceFile = file.getName().endsWith(".scala");

    def sourceFile =
      if (sources.isEmpty || sources.head == null) null;
      else if (sources.head.location.isDirectory()) null;
      else sources.head.location;

    def sourcePath =
      if (sources.isEmpty || sources.head == null) null;
      else if (!sources.head.location.isDirectory()) null;
      else sources.head.location;

    def validPackage(name: String): Boolean =
      if (name.equals("META-INF")) false;
      else if (name.startsWith(".")) false;
      else true;

    def complete: List[Context] = {
      assert(isPackage);
      def complete0(classes: List[AbstractFile], sources: List[Source]): List[Context] =
      if (classes.isEmpty) Nil;
      else if (classes.head == null && sources.head == null) complete0(classes.tail, sources.tail);
      else {
	var ret: List[Context] = Nil;
	if (classes.head != null) {
	  val i = classes.head.list();
	  while (i.hasNext()) {
	    val file = i.next().asInstanceOf[AbstractFile];
	    if (!file.isDirectory() && file.getName().endsWith(".class")) {
	      val name = file.getName().substring(0, file.getName().length() - (".class").length());
	      ret = find(name, false) :: ret;
	    } else if (file.isDirectory() && validPackage(file.getName())) {
	      ret = (find(file.getName(), true) :: ret);
	      // System.err.println("FILE: " + file.getName() + " RET=" + ret.head);
	    }
	  }
	}
	if (sources.head != null && sources.head.compile) {
	  val j = sources.head.location.list();
	  while (j.hasNext()) {
	    val file = j.next().asInstanceOf[AbstractFile];
	    if (!file.isDirectory() && file.getName().endsWith(".scala")) {
	      val name = file.getName().substring(0, file.getName().length() - (".scala").length());
	      if (classes.head == null ||
		  classes.head.lookupPath(name + ".class", false) == null) ret = find(name, false) :: ret;
	    } else if (file.isDirectory() && validPackage(file.getName())) {
	      if (classes.head == null || classes.head.lookupPath(file.getName(), true) == null)
		ret = find(file.getName(), true) :: ret;
	    }
	  }
	}
	ret ::: complete0(classes.tail, sources.tail);
      }
      complete0(classes, sources);
    }

  } // class Context

  class Build(val output: Output) {
    val entries = new ArrayBuffer[Entry]

    def root = {
      val classes = for (val entry <- entries.toList) yield entry.location
      val sources = for (val entry <- entries.toList) yield entry.source
      new Context(classes, sources)
    }


    def this(classpath: String, source: String, output: String, boot: String, extdirs: String) = {
      this(new Output(AbstractFile.getDirectory(output), AbstractFile.getDirectory(source)));

      if (this.output.location == null)
        throw new FileNotFoundException("output location \"" + output + "\" not found");
      if (this.output.source   == null)
        throw new FileNotFoundException("source location \"" + source + "\" not found");

      addFilesInPath(boot);
      addArchivesInExtDirPath(extdirs);
      entries += this.output;
      addFilesInPath(classpath)
    }

    def library(classes: String, sources: String) = {
      assert(classes != null);
      val location = AbstractFile.getDirectory(classes);
      val sourceFile0 = (if (sources != null) AbstractFile.getDirectory(sources) else null);
      class Library0 extends Library(location) {
	override def sourceFile = sourceFile0;
      }
      entries += new Library0()
    }

    private def addFilesInPath(path: String) = {
      val strtok = new StringTokenizer(path, File.pathSeparator);
      while (strtok.hasMoreTokens()) {
	val file = AbstractFile.getDirectory(strtok.nextToken());
	if (file != null) entries += (new Library(file));
      }
    }

    private def addArchivesInExtDirPath(path: String) = {
      val strtok = new StringTokenizer(path, File.pathSeparator);
      while (strtok.hasMoreTokens()) {
	val file = AbstractFile.getDirectory(strtok.nextToken());
	val files = (if (file != null) file.list() else null);
	if (files != null) while(files.hasNext()) {
	  val file0 = files.next().asInstanceOf[AbstractFile];
	  val name = file0.getName();
	  if (name.endsWith(".jar") || name.endsWith(".zip")) {
	    val archive = AbstractFile.getDirectory(new File(file.getFile(), name));
	    if (archive != null) entries += (new Library(archive));
	  }
	}
      }
    }

  } // class Build

}
