/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package backend.jvm

import java.io.{ DataOutputStream, FileOutputStream, OutputStream, File => JFile }
import scala.tools.nsc.io._
import scala.tools.nsc.util.ScalaClassLoader
import scala.tools.util.JavapClass
import java.util.jar.Attributes.Name
import scala.language.postfixOps

/** For the last mile: turning generated bytecode in memory into
 *  something you can use.  Has implementations for writing to class
 *  files, jars, and disassembled/javap output.
 */
trait BytecodeWriters {
  val global: Global
  import global._

  private def outputDirectory(sym: Symbol): AbstractFile = (
    settings.outputDirs.outputDirFor(enteringFlatten(sym.sourceFile))
  )
  /** Avoids assertions in AbstractFile in case where a directory is sought
   *  but a file exists instead. Remove the file (it's lying around in the
   *  output directory - removing it is reasonable) and make a directory.
   */
  private def ensureDirectory(dir: AbstractFile): AbstractFile = {
    if (dir.isDirectory) dir else {
      warning(s"Removing file $dir from output directory")
      val container = dir.container
      val name = dir.name
      dir.delete()
      container.subdirectoryNamed(name)
    }
  }
  private def getFile(base: AbstractFile, /*cls.getName()*/ clsName: String, suffix: String): AbstractFile = {
    val pathParts = clsName.split("[./]").toList
    var dir = base
    for (part <- pathParts.init)
      dir = ensureDirectory(dir) subdirectoryNamed part

    ensureDirectory(dir) fileNamed pathParts.last + suffix
  }
  private def getFile(sym: Symbol, clsName: String, suffix: String): AbstractFile =
    getFile(outputDirectory(sym), clsName, suffix)

  trait BytecodeWriter {
    def writeClass(label: String, jclassName: String, jclassBytes: Array[Byte], sym: Symbol): Unit
    def close(): Unit = ()
  }

  class DirectToJarfileWriter(jfile: JFile) extends BytecodeWriter {
    val jarMainAttrs = (
      if (settings.mainClass.isDefault) Nil
      else List(Name.MAIN_CLASS -> settings.mainClass.value)
    )
    val writer = new Jar(jfile).jarWriter(jarMainAttrs: _*)

    def writeClass(label: String, jclassName: String, jclassBytes: Array[Byte], sym: Symbol) {
      val path = jclassName + ".class"
      val out  = writer.newOutputStream(path)

      try out.write(jclassBytes, 0, jclassBytes.length)
      finally out.flush()

      informProgress("added " + label + path + " to jar")
    }
    override def close() = writer.close()
  }

  trait JavapBytecodeWriter extends BytecodeWriter {
    val baseDir = Directory(settings.Ygenjavap.value).createDirectory()

    def emitJavap(bytes: Array[Byte], javapFile: io.File) {
      val pw    = javapFile.printWriter()
      val javap = new JavapClass(ScalaClassLoader.appLoader, pw) {
        override def findBytes(path: String): Array[Byte] = bytes
      }

      try javap(Seq("-verbose", "dummy")) foreach (_.show())
      finally pw.close()
    }
    abstract override def writeClass(label: String, jclassName: String, jclassBytes: Array[Byte], sym: Symbol) {
      super.writeClass(label, jclassName, jclassBytes, sym)

      val bytes     = getFile(sym, jclassName, ".class").toByteArray
      val segments  = jclassName.split("[./]")
      val javapFile = segments.foldLeft(baseDir: Path)(_ / _) changeExtension "javap" toFile;

      javapFile.parent.createDirectory()
      emitJavap(bytes, javapFile)
    }
  }

  trait ClassBytecodeWriter extends BytecodeWriter {
    def writeClass(label: String, jclassName: String, jclassBytes: Array[Byte], sym: Symbol) {
      val outfile   = getFile(sym, jclassName, ".class")
      val outstream = new DataOutputStream(outfile.bufferedOutput)

      try outstream.write(jclassBytes, 0, jclassBytes.length)
      finally outstream.close()
      informProgress("wrote '" + label + "' to " + outfile)
    }
  }

  trait DumpBytecodeWriter extends BytecodeWriter {
    val baseDir = Directory(settings.Ydumpclasses.value).createDirectory()

    abstract override def writeClass(label: String, jclassName: String, jclassBytes: Array[Byte], sym: Symbol) {
      super.writeClass(label, jclassName, jclassBytes, sym)

      val pathName = jclassName
      val dumpFile = pathName.split("[./]").foldLeft(baseDir: Path) (_ / _) changeExtension "class" toFile;
      dumpFile.parent.createDirectory()
      val outstream = new DataOutputStream(new FileOutputStream(dumpFile.path))

      try outstream.write(jclassBytes, 0, jclassBytes.length)
      finally outstream.close()
    }
  }
}
