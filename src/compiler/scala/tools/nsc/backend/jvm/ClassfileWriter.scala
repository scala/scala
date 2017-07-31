package scala.tools.nsc.backend.jvm

import java.io.{DataOutputStream, IOException, PrintWriter, StringWriter}
import java.nio.file.Files
import java.util.jar.Attributes.Name

import scala.reflect.internal.util.{NoPosition, Statistics}
import scala.reflect.io._
import scala.tools.asm.ClassReader
import scala.tools.asm.tree.ClassNode
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.io.{AbstractFile, Jar, JarWriter}

class ClassfileWriter[BT <: BTypes](val bTypes: BT, backendReporting: BackendReporting, getEntryPoints: () => List[String]) {
  import bTypes._

  // if non-null, asm text files are written to this directory
  private val asmOutputDir: AbstractFile = getDirectoryOrNull(compilerSettings.Ygenasmp.valueSetByUser)

  // if non-null, classfiles are additionally written to this directory
  private val dumpOutputDir: AbstractFile = getDirectoryOrNull(compilerSettings.Ydumpclasses.valueSetByUser)

  // if non-null, classfiles are written to a jar instead of the output directory
  private val jarWriter: JarWriter = compilerSettings.outdir.outputDirs.getSingleOutput match {
    case Some(f) if f hasExtension "jar" =>
      // If no main class was specified, see if there's only one
      // entry point among the classes going into the jar.
      if (compilerSettings.mainClass.isDefault) {
        getEntryPoints() match {
          case Nil      =>
            backendReporting.log("No Main-Class designated or discovered.")
          case name :: Nil =>
            backendReporting.log(s"Unique entry point: setting Main-Class to $name")
            compilerSettings.mainClass.value = name
          case names =>
            backendReporting.log(s"No Main-Class due to multiple entry points:\n  ${names.mkString("\n  ")}")
        }
      }
      else backendReporting.log(s"Main-Class was specified: ${compilerSettings.mainClass.value}")

      val jarMainAttrs =
        if (compilerSettings.mainClass.isDefault) Nil
        else List(Name.MAIN_CLASS -> compilerSettings.mainClass.value)

      new Jar(f.file).jarWriter(jarMainAttrs: _*)

    case _ => null
  }

  private def getDirectoryOrNull(dir: Option[String]): AbstractFile =
    dir.map(d => new PlainDirectory(Directory(Path(d)))).orNull

  private def getFile(base: AbstractFile, clsName: String, suffix: String): AbstractFile = {
    if (base.file != null) {
      fastGetFile(base, clsName, suffix)
    } else {
      def ensureDirectory(dir: AbstractFile): AbstractFile =
        if (dir.isDirectory) dir
        else throw new FileConflictException(s"${base.path}/$clsName$suffix: ${dir.path} is not a directory", dir)
      var dir = base
      val pathParts = clsName.split("[./]").toList
      for (part <- pathParts.init) dir = ensureDirectory(dir) subdirectoryNamed part
      ensureDirectory(dir) fileNamed pathParts.last + suffix
    }
  }

  private def fastGetFile(base: AbstractFile, clsName: String, suffix: String) = {
    val index = clsName.lastIndexOf('/')
    val (packageName, simpleName) = if (index > 0) {
      (clsName.substring(0, index), clsName.substring(index + 1))
    } else ("", clsName)
    val directory = base.file.toPath.resolve(packageName)
    new PlainNioFile(directory.resolve(simpleName + suffix))
  }

  private def writeClassfile(outFile: AbstractFile, bytes: Array[Byte]): Unit = {
    if (outFile.file != null) {
      val outPath = outFile.file.toPath
      try Files.write(outPath, bytes)
      catch {
        case _: java.nio.file.NoSuchFileException =>
          Files.createDirectories(outPath.getParent)
          Files.write(outPath, bytes)
      }
    } else {
      val out = new DataOutputStream(outFile.bufferedOutput)
      try out.write(bytes, 0, bytes.length)
      finally out.close()
    }
  }

  private def writeAsmp(asmpFile: AbstractFile, bytes: Array[Byte]): Unit = {
    val pw = new PrintWriter(asmpFile.bufferedOutput)
    try {
      val cnode = new ClassNode()
      val cr = new ClassReader(bytes)
      cr.accept(cnode, 0)
      val trace = new scala.tools.asm.util.TraceClassVisitor(new PrintWriter(new StringWriter()))
      cnode.accept(trace)
      trace.p.print(pw)
    } finally pw.close()
  }

  def write(className: InternalName, bytes: Array[Byte], sourceFile: AbstractFile): Unit = try {
    val writeStart = Statistics.startTimer(BackendStats.bcodeWriteTimer)
    if (jarWriter == null) {
      val outFolder = compilerSettings.outdir.outputDirs.outputDirFor(sourceFile)
      val outFile = getFile(outFolder, className, ".class")
      writeClassfile(outFile, bytes)
    } else {
      val path = className + ".class"
      val out = jarWriter.newOutputStream(path)
      try out.write(bytes, 0, bytes.length)
      finally out.flush()
    }
    Statistics.stopTimer(BackendStats.bcodeWriteTimer, writeStart)

    if (asmOutputDir != null) {
      val asmpFile = getFile(asmOutputDir, className, ".asmp")
      writeAsmp(asmpFile, bytes)
    }

    if (dumpOutputDir != null) {
      val dumpFile = getFile(dumpOutputDir, className, ".class")
      writeClassfile(dumpFile, bytes)
    }
  } catch {
    case e: FileConflictException =>
      backendReporting.error(NoPosition, s"error writing $className: ${e.getMessage}")
    case e: java.nio.file.FileSystemException =>
      if (compilerSettings.debug.value)
        e.printStackTrace()
      backendReporting.error(NoPosition, s"error writing $className: ${e.getClass.getName} ${e.getMessage}")
  }

  def close(): Unit = {
    if (jarWriter != null) jarWriter.close()
  }

  abstract class ClassfileWriter {
    final def writeClass(label: String, jclassName: String, jclassBytes: Array[Byte], outfile: AbstractFile): Unit = {

    }

    def writeClassFile(): Unit
    def close(): Unit
  }
}

/** Can't output a file due to the state of the file system. */
class FileConflictException(msg: String, val file: AbstractFile) extends IOException(msg)
