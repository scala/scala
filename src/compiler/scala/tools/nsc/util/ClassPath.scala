/* NSC -- new Scala compiler
 * Copyright 2006-2009 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc
package util

import java.io.File
import java.net.URL
import java.util.StringTokenizer

import scala.collection.mutable.ArrayBuffer
import scala.tools.nsc.io.AbstractFile

/** <p>
 *    This module provides star expansion of '-classpath' option arguments.
 *  </p>
 *
 *  @author Stepan Koltsov
 */
object ClassPath {
  /** Expand single path entry */
  private def expandStar(pattern: String): List[String] = {
    def nameMatchesStar(name: String) = name.toLowerCase().endsWith(".jar")

    /** Get all jars in directory */
    def lsJars(f: File) = {
      val list = f.listFiles()
      if (list eq null) Nil
      else list.filter(f => f.isFile() && nameMatchesStar(f.getName())).map(_.getPath()).toList
    }

    val suffix = File.separator + "*"

    if (pattern == "*") lsJars(new File("."))
    else if (pattern endsWith suffix) lsJars(new File(pattern.substring(0, pattern.length - suffix.length)))
    else pattern :: Nil
  }

  /** Split path using platform-dependent path separator */
  def splitPath(path: String): List[String] = {
    val strtok = new StringTokenizer(path, File.pathSeparator)
    val buf = new collection.mutable.ListBuffer[String]
    while (strtok.hasMoreTokens()) {
      buf + strtok.nextToken()
    }
    buf.toList
  }

  /** Expand path with expanding stars */
  def expandPath(path: String): List[String] = splitPath(path).flatMap(expandStar(_))

  def expandPath(path: String, expandStar: Boolean): List[String] =
    if (expandStar) expandPath(path)
    else splitPath(path)

}

/** <p>
 *    Richer classpath abstraction than files.
 *  </p>
 *  <p>
 *    Roughly based on Eclipse's classpath abstractions.
 *  </p>
 *
 *  @author Sean McDirmid
 */
class ClassPath(onlyPresentation: Boolean) {

  def this() = this(false)

  class Source(val location: AbstractFile, val compile: Boolean) {
    // assert(location           != null, "cannot find source location")
    // assert(location.getFile() != null, "cannot find source location " + " " + location + " " + location.getClass())
    override def toString(): String = "" + location + " " + compile
  }

  abstract class Entry(val location: AbstractFile) {
    // assert(location           != null, "cannot find classpath location")
    // assert(location.getFile() != null, "cannot find classpath location " + " " + location + " " + location.getClass())
    def source: Source
    override def toString() =
      (if (location == null) "<none>" else location.toString) +
        (if (source == null) "" else " source=" + source)
  }

  class Output(location0: AbstractFile, val sourceFile: AbstractFile) extends Entry(location0) {
    def source = if (sourceFile ne null) new Source(sourceFile, true) else null
  }

  class Library(location0: AbstractFile) extends Entry(location0) {
    def doc: AbstractFile = null
    def sourceFile: AbstractFile = null
    def source = if (sourceFile eq null) null else new Source(sourceFile, false)
  }

  class Context(val entries: List[Entry]) {
    def find(name: String, isDir: Boolean): Context = if (isPackage) {
      def find0(entries: List[Entry]): Context = {
        if (entries.isEmpty) new Context(Nil)
        else {
          val ret = find0(entries.tail)
          val head = entries.head;
          val name0 = name + (if (!isDir) ".class" else "")
          val clazz = if (head.location eq null) null
                      else head.location.lookupPath(name0, isDir)

          val source0 =
            if (head.source eq null) null
            else if ((clazz eq null) || isDir) {
              val source1 = head.source.location.lookupPath(
                name + (if (isDir) "" else ".scala"), isDir)
              if ((source1 eq null) && !isDir && (clazz ne null)) head.source.location
              else source1
            }
            else head.source.location

          if ((clazz eq null) && (source0 eq null)) ret
          else {
            val entry = new Entry(clazz) {
              override def source =
                if (source0 eq null) null
                else new Source(source0, head.source.compile)
            }
            try {
              //Console.err.println("this=" + this + "\nclazz=" + clazz + "\nsource0=" + source0 + "\n")

              if (!isDir) new Context(entry :: Nil)
              else new Context(entry :: ret.entries)
            } catch {
              case e: Error =>
              throw e
            }
          }
        }
      }

      val ret = find0(entries)
      if (ret.entries.isEmpty) {
        //Console.err.println("BAD_FILE: " + name + " in " + this)
        null
      } else ret
    } else null

    def isPackage: Boolean =
      if (entries.isEmpty) false
      else if (entries.head.location ne null) entries.head.location.isDirectory
      else entries.head.source.location.isDirectory

    def name =
      if (entries.isEmpty) "<none>"
      else {
        val head = entries.head
        val name = if (head.location ne null) head.location.name
                   else head.source.location.name
        if (isPackage) name
        else name.substring(0, name.length() - (".class").length())
      }

    override def toString(): String = toString(entries)

    def toString(entry: Entry): String =
      ((if (entry.location eq null) "<none>"
        else entry.location.toString()) +
       (if (entry.source eq null) ""
        else " with_source=" + entry.source.location.toString()))

    def toString(entries0: List[Entry]): String =
      if (entries0.isEmpty) ""
      else toString(entries0.head) + ":::" + toString(entries0.tail)

    def isSourceFile = {
      def head = entries.head
      def clazz = head.location
      def source = if (head.source eq null) null else head.source.location
      def isPredef = source.name.equals("Predef.scala") ||
                     source.path.startsWith("scala/runtime")

      if (entries.isEmpty || entries.isEmpty || (source eq null)) false
      else if (!onlyPresentation && !head.source.compile) false
      else if (source.isDirectory) false
      else if (clazz eq null) true
      else if (onlyPresentation && !isPredef) true
      else if (source.lastModified > clazz.lastModified) true
      else false
    }

    def sourceFile = if ((entries.head.source ne null) && !entries.head.source.location.isDirectory)
      entries.head.source.location else null

    def classFile = if (!isSourceFile) entries.head.location else null

    def sourcePath =
      if (!isSourceFile && !entries.isEmpty && (entries.head.source ne null)) {
        val ret = entries.head.source.location
        if ((ret ne null) && !ret.isDirectory) {
          Console.err.println("source path " + ret + " is not a directory")
          null
        } else ret
      }
      else null

    def validPackage(name: String): Boolean =
      ! (name.equals("META-INF") || name.startsWith("."))
  }

  class Build {
    val entries = new ArrayBuffer[Entry]

    def root = new Context(entries.toList)

    def this(classpath: String) {
      this()
      addFilesInPath(classpath)
    }

    def this(source: String, output: String) {
      this()
      addDirsInPath(source, output)
    }

    def this(classpath: String, source: String, output: String,
             boot: String, extdirs: String, codebase: String) {
      this()
      addFilesInPath(boot)
      addArchivesInExtDirPath(extdirs)
      addDirsInPath(source, output)
      addFilesInPath(classpath)
      addURLsInPath(codebase)
    }

    /**
     *  Lookup the given path in this classpath. Returns null if not found.
     *  Does not work with absolute paths (starting with '/').
     *
     *  @param path  Path to look up (if isDir is false, '.class' is appended!).
     *  @param isDir Whether to look for a directory or a file
     *  @return      The abstract file or null if path was not found
     */
    def lookupPath(path: String, isDir: Boolean): AbstractFile = {
      val ctx = root.find(path, isDir)
      if (ctx eq null) null
      else if (ctx.entries.isEmpty) null
      else if (ctx.entries.head eq null) null
      else ctx.entries.head.location
    }

    /**
     *  @param classes where the class files come from and are written to
     *  @param sources where the source files come from
     */
    def output(classes : String, sources : String) = {
      assert(classes ne null)
      assert(sources ne null)
      val location = AbstractFile.getDirectory(classes)
      val sources0 = AbstractFile.getDirectory(sources)
      class Output0 extends Output(location, sources0)
      entries += new Output0()
    }
    /**
     *  @param classes where the class files come from
     *  @param sources optional source file attachment, otherwise null
     */
    def library(classes: String, sources: String) {
      assert(classes ne null)
      val location = AbstractFile.getDirectory(classes)
      var sourceFile0 =
        if (sources ne null) AbstractFile.getDirectory(sources)
        else null
      if (sourceFile0 ne null) {
        val file00 = sourceFile0.lookupPath("src", true)
        if ((file00 ne null) && file00.isDirectory) {
          sourceFile0 = file00
        }
      }

      class Library0 extends Library(location) {
        override def sourceFile = sourceFile0
      }
      entries += new Library0()
    }

    private def addFilesInPath(path: String) {
      for (fileName <- ClassPath.expandPath(path)) {
        val file = AbstractFile.getDirectory(fileName)
        if (file ne null) entries += (new Library(file))
      }
    }

    private def addArchivesInExtDirPath(path: String) {
      for (fileName <- ClassPath.expandPath(path)) {
        val file = AbstractFile.getDirectory(fileName)
        if (file ne null) {
          for (file0 <- file) {
            val name = file0.name
            if (name.endsWith(".jar") || name.endsWith(".zip") || file0.isDirectory) {
              val archive = AbstractFile.getDirectory(new File(file.file, name))
              if (archive ne null) entries += (new Library(archive))
            }
          }
        }
      }
    }

    private def addDirsInPath(source: String, output: String) {
      val clazzes = AbstractFile.getDirectory(output)
      if (clazzes eq null)
        throw new FatalError("Output location \"" + output + "\" not found")
      val strtok = new StringTokenizer(source, File.pathSeparator)
      if (!strtok.hasMoreTokens()) {
        val output0 = (new Output(clazzes, null))
        entries += output0
      }
      else while (strtok.hasMoreTokens()) {
        val sources = AbstractFile.getDirectory(strtok.nextToken())
        val output0 = (new Output(clazzes, sources))
        entries += output0
      }
    }

    private val urlSeparator = " "
    private def addURLsInPath(codebase: String) {
      val strtok = new StringTokenizer(codebase, urlSeparator)
      while (strtok.hasMoreTokens()) {
        try {
          val url = new URL(strtok.nextToken())
          val archive = AbstractFile.getURL(url)
          if (archive ne null) entries += (new Library(archive))
        }
        catch {
          case e =>
            Console.println("error in addURLsInPath: " + e.getMessage)//debug
            throw e
        }
      }
    }

    override def toString() =
      entries.toList.mkString("", File.pathSeparator, "")
  } // class Build

}
