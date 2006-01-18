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

  class Source(val location : AbstractFile, val compile : Boolean) {
	  assert(location != null);
	  override def toString() : String = "" + location + " " + compile;
  }

  abstract class Entry {
    def location : AbstractFile;
    def source   : Source;
  }

  class Output(val location : AbstractFile, val sourceFile : AbstractFile) extends Entry {
    def source = if (sourceFile != null) new Source(sourceFile, true); else null;
  }
  class Library(val location: AbstractFile) extends Entry {
    def doc : AbstractFile = null;
    def sourceFile : AbstractFile = null;
    def source = if (sourceFile == null) null else new Source(sourceFile, false);
  }


  class Context(val entries : List[Entry]) {
    def find(name : String, isDir : boolean) : Context = if (isPackage) {
      def find0(entries : List[Entry]) : Context = {
    	if (entries.isEmpty) new Context(Nil);
	else {
	  val ret = find0(entries.tail);
	  val head = entries.head;


	  val clazz = if (head.location == null) null; else
	    head.location.lookupPath(name + (if (!isDir) ".class" else ""), isDir);

	  val source0 = if (head.source == null) null; else {
	    val source1 = head.source.location.lookupPath(name + (if (isDir) "" else ".scala"), isDir);
 	    if (source1 == null && !isDir && clazz != null) head.source.location;
 	    else source1;
	  }
	  if (clazz == null && source0 == null) ret;
	  else {
	    object entry extends Entry {
	      override def location = clazz;
	      override def source =
		if (source0 == null) null;
		else new Source(source0, head.source.compile);
	    };
	    new Context(entry :: ret.entries);
	  }
	}
      }
      val ret = find0(entries);
      if (false && this.toString().indexOf("scala") != -1)
	System.err.println("FIND " + name + " in " + this + " => " + ret);
      ret;
    } else null;


    def isPackage = {
    	if (entries.isEmpty) false;
    	else if (entries.head.location != null) entries.head.location.isDirectory();
    	else entries.head.source.location.isDirectory();
    }

    def name = {
    	if (entries.isEmpty) "<none>";
    	else {
    		val head = entries.head;

	      val name = if (head.location != null) head.location.getName() else head.source.location.getName();
	      if (isPackage) name;
	      else name.substring(0, name.length() - (".class").length());
    	}
    }

    override def toString(): String = toString(entries);

    def toString(entry : Entry): String =
    	((if (entry.location == null) "<none>";
    	  else entry.location.toString()) +
    	 (if (entry.source == null) ""; else " with_source=" + entry.source.location.toString()));

    def toString(entries0: List[Entry]): String =
      if (entries0.isEmpty) "";
      else toString(entries0.head) + ":::" + toString(entries0.tail);

    def isSourceFile = {
      def head = entries.head;
      def clazz = head.location;
      def source = if (head.source == null) null else head.source.location;
      if (entries.isEmpty || entries.isEmpty || source == null || !head.source.compile || !source.getFile().isFile()) false;
      else if (clazz == null) true;
      else if (source.lastModified() > clazz.lastModified()) true;
      else false;
    }

    def sourceFile = if ( isSourceFile) entries.head.source.location else null;
    def  classFile = if (!isSourceFile) entries.head       .location else null;

    def sourcePath = if (!isSourceFile && entries.head.source != null) entries.head.source.location else null;

    def validPackage(name: String): Boolean =
      if (name.equals("META-INF")) false;
      else if (name.startsWith(".")) false;
      else true;
  }
  class Build {
    val entries = new ArrayBuffer[Entry];


    def root = new Context(entries.toList);


    def this(classpath : String, source : String, output : String, boot : String, extdirs : String) = {
      this();
      //System.err.println("BOOT: " + boot);
      //System.err.println("CLAS: " + classpath);

      addFilesInPath(boot);

      addArchivesInExtDirPath(extdirs);
      val strtok = new StringTokenizer(source, File.pathSeparator);
      while (strtok.hasMoreTokens()) {
		  	val output0 = (new Output(AbstractFile.getDirectory(output), AbstractFile.getDirectory(strtok.nextToken())));
		    if (output0.location          == null) throw new FileNotFoundException("output location \"" + output + "\" not found");
		    //System.err.println("OUTPUT: " + output);
  	    entries += output0;
      }
      addFilesInPath(classpath);
      //System.err.println("CLASSPATH: " + root);
    }
    def library(classes : String, sources : String) = {
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
