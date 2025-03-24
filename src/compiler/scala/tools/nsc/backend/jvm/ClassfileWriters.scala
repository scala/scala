/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.backend.jvm

import java.io.{DataOutputStream, IOException}
import java.nio.ByteBuffer
import java.nio.channels.{ClosedByInterruptException, FileChannel}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file._
import java.nio.file.attribute.FileAttribute
import java.util
import java.util.concurrent.ConcurrentHashMap
import java.util.zip.{CRC32, Deflater, ZipEntry, ZipOutputStream}

import scala.reflect.internal.util.NoPosition
import scala.reflect.io.PlainNioFile
import scala.tools.nsc.Global
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.plugins.{OutputFileWriter, Plugin}
import scala.tools.nsc.util.JarFactory
import scala.util.chaining._

abstract class ClassfileWriters {
  val postProcessor: PostProcessor
  import postProcessor.bTypes.frontendAccess

  /**
   * The interface to writing classfiles. GeneratedClassHandler calls these methods to generate the
   * directory and files that are created, and eventually calls `close` when the writing is complete.
   *
   * The companion object is responsible for constructing a appropriate and optimal implementation for
   * the supplied settings.
   *
   * Operations are threadsafe.
   */
  sealed trait ClassfileWriter extends OutputFileWriter with AutoCloseable {
    /**
     * Write a classfile
     */
    def writeClass(name: InternalName, bytes: Array[Byte], sourceFile: AbstractFile): Unit

    /**
     * Close the writer. Behavior is undefined after a call to `close`.
     */
    def close(): Unit

    protected def classRelativePath(className: InternalName, suffix: String = ".class"): String =
      className.replace('.', '/') + suffix
  }

  object ClassfileWriter {
    private def getDirectory(dir: String): Path = Paths.get(dir)

    def apply(global: Global): ClassfileWriter = {
      //Note dont import global._ - its too easy to leak non threadsafe structures
      import global.{ cleanup, log, settings }
      def jarManifestMainClass: Option[String] = settings.mainClass.valueSetByUser.orElse {
        cleanup.getEntryPoints match {
          case List(name) => Some(name)
          case es =>
            if (es.isEmpty) log("No Main-Class designated or discovered.")
            else log(s"No Main-Class due to multiple entry points:\n  ${es.mkString("\n  ")}")
            None
        }
      }

      val basicClassWriter = settings.outputDirs.getSingleOutput match {
        case Some(dest) => new SingleClassWriter(FileWriter(global, dest, jarManifestMainClass))
        case None =>
          val distinctOutputs: Set[AbstractFile] = settings.outputDirs.outputs.iterator.map(_._2).toSet
          if (distinctOutputs.size == 1) new SingleClassWriter(FileWriter(global, distinctOutputs.head, jarManifestMainClass))
          else {
            val sourceToOutput: Map[AbstractFile, AbstractFile] = global.currentRun.units.map(unit => (unit.source.file, frontendAccess.compilerSettings.outputDirectory(unit.source.file))).toMap
            new MultiClassWriter(sourceToOutput, distinctOutputs.iterator.map { output: AbstractFile => output -> FileWriter(global, output, jarManifestMainClass) }.toMap)
          }
      }

      val withAdditionalFormats = {
        def maybeDir(dir: Option[String]): Option[Path] = dir.map(getDirectory).filter(path => Files.exists(path).tap(ok => if (!ok) frontendAccess.backendReporting.error(NoPosition, s"Output dir does not exist: $path")))
        def writer(out: Path) = FileWriter(global, new PlainNioFile(out), None)
        val List(asmp, dump) = List(settings.Ygenasmp, settings.Ydumpclasses).map(s => maybeDir(s.valueSetByUser).map(writer)): @unchecked
        if (asmp.isEmpty && dump.isEmpty) basicClassWriter
        else new DebugClassWriter(basicClassWriter, asmp, dump)
      }

      val enableStats = settings.areStatisticsEnabled && settings.YaddBackendThreads.value == 1
      if (enableStats) new WithStatsWriter(withAdditionalFormats) else withAdditionalFormats
    }

    /** Writes to the output directory corresponding to the source file, if multiple output directories are specified */
    private final class MultiClassWriter(sourceToOutput: Map[AbstractFile, AbstractFile], underlying: Map[AbstractFile, FileWriter]) extends ClassfileWriter {
      private def getUnderlying(sourceFile: AbstractFile, outputDir: AbstractFile) = underlying.getOrElse(outputDir, {
        throw new Exception(s"Cannot determine output directory for ${sourceFile} with output ${outputDir}. Configured outputs are ${underlying.keySet}")
      })
      private def getUnderlying(outputDir: AbstractFile) = underlying.getOrElse(outputDir, {
        throw new Exception(s"Cannot determine output for ${outputDir}. Configured outputs are ${underlying.keySet}")
      })

      override def writeClass(className: InternalName, bytes: Array[Byte], sourceFile: AbstractFile): Unit = {
        getUnderlying(sourceFile, sourceToOutput(sourceFile)).writeFile(classRelativePath(className), bytes)
      }

      override def writeFile(relativePath: String, data: Array[Byte], outputDir: AbstractFile): Unit = {
        getUnderlying(outputDir).writeFile(relativePath, data)
      }

      override def close(): Unit = underlying.values.foreach(_.close())
    }
    private final class SingleClassWriter(underlying: FileWriter) extends ClassfileWriter {
      override def writeClass(className: InternalName, bytes: Array[Byte], sourceFile: AbstractFile): Unit = {
        underlying.writeFile(classRelativePath(className), bytes)
      }

      override def writeFile(relativePath: String, data: Array[Byte], outputDir: AbstractFile): Unit = {
        underlying.writeFile(relativePath, data)
      }

      override def close(): Unit = underlying.close()
    }

    private final class DebugClassWriter(basic: ClassfileWriter, asmp: Option[FileWriter], dump: Option[FileWriter]) extends ClassfileWriter {
      override def writeClass(className: InternalName, bytes: Array[Byte], sourceFile: AbstractFile): Unit = {
        basic.writeClass(className, bytes, sourceFile)
        asmp.foreach { writer =>
          val asmBytes = AsmUtils.textify(AsmUtils.readClass(bytes)).getBytes(UTF_8)
          writer.writeFile(classRelativePath(className, ".asm"), asmBytes)
        }
        dump.foreach { writer =>
          writer.writeFile(classRelativePath(className), bytes)
        }
      }

      override def writeFile(relativePath: String, data: Array[Byte], outputDir: AbstractFile): Unit = {
        basic.writeFile(relativePath, data, outputDir)
      }

      override def close(): Unit = {
        basic.close()
        asmp.foreach(_.close())
        dump.foreach(_.close())
      }
    }

    private final class WithStatsWriter(underlying: ClassfileWriter) extends ClassfileWriter {
      override def writeClass(className: InternalName, bytes: Array[Byte], sourceFile: AbstractFile): Unit = {
        val statistics = frontendAccess.unsafeStatistics
        val snap = statistics.startTimer(statistics.bcodeWriteTimer)
        try underlying.writeClass(className, bytes, sourceFile)
        finally statistics.stopTimer(statistics.bcodeWriteTimer, snap)
      }

      override def writeFile(relativePath: String, data: Array[Byte], outputDir: AbstractFile): Unit = {
        underlying.writeFile(relativePath, data, outputDir)
      }

      override def close(): Unit = underlying.close()
    }
  }

  sealed trait FileWriter {
    def writeFile(relativePath: String, bytes: Array[Byte]): Unit
    def close(): Unit
  }

  object FileWriter {
    def apply(global: Global, file: AbstractFile, jarManifestMainClass: Option[String]): FileWriter =
      if (file.hasExtension("jar")) {
        val jarCompressionLevel = global.settings.YjarCompressionLevel.value
        val jarFactory =
          Class.forName(global.settings.YjarFactory.value)
            .asSubclass(classOf[JarFactory])
            .getDeclaredConstructor().newInstance()
        new JarEntryWriter(file, jarManifestMainClass, jarCompressionLevel, jarFactory, global.plugins)
      }
      else if (file.isVirtual) new VirtualFileWriter(file)
      else if (file.isDirectory) new DirEntryWriter(file.file.toPath)
      else throw new IllegalStateException(s"don't know how to handle an output of $file [${file.getClass}]")
  }

  private final class JarEntryWriter(file: AbstractFile, mainClass: Option[String], compressionLevel: Int, jarFactory: JarFactory, plugins: List[Plugin]) extends FileWriter {
    //keep these imports local - avoid confusion with scala naming
    import java.util.jar.Attributes.Name.{MANIFEST_VERSION, MAIN_CLASS}
    import java.util.jar.{JarOutputStream, Manifest}

    val storeOnly = compressionLevel == Deflater.NO_COMPRESSION

    val jarWriter: JarOutputStream = {
      import scala.util.Properties._
      val manifest = new Manifest
      val attrs = manifest.getMainAttributes
      attrs.put(MANIFEST_VERSION, "1.0")
      attrs.put(ScalaCompilerVersion, versionNumberString)
      mainClass.foreach(c => attrs.put(MAIN_CLASS, c))
      plugins.foreach(_.augmentManifest(file, manifest))

      val jar = jarFactory.createJarOutputStream(file, manifest)
      jar.setLevel(compressionLevel)
      if (storeOnly) jar.setMethod(ZipOutputStream.STORED)
      jar
    }

    lazy val crc = new CRC32

    override def writeFile(relativePath: String, bytes: Array[Byte]): Unit = this.synchronized {
      val entry = new ZipEntry(relativePath)
      if (storeOnly) {
        // When using compression method `STORED`, the ZIP spec requires the CRC and compressed/
        // uncompressed sizes to be written before the data. The JarOutputStream could compute the
        // values while writing the data, but not patch them into the stream after the fact. So we
        // need to pre-compute them here. The compressed size is taken from size.
        // https://stackoverflow.com/questions/1206970/how-to-create-uncompressed-zip-archive-in-java/5868403
        // With compression method `DEFLATED` JarOutputStream computes and sets the values.
        entry.setSize(bytes.length)
        crc.reset()
        crc.update(bytes)
        entry.setCrc(crc.getValue)
      }
      jarWriter.putNextEntry(entry)
      try jarWriter.write(bytes, 0, bytes.length)
      finally jarWriter.flush()
    }

    override def close(): Unit = this.synchronized(jarWriter.close())
  }

  private final class DirEntryWriter(base: Path) extends FileWriter {
    import scala.util.Properties.{isWin => isWindows}
    val builtPaths = new ConcurrentHashMap[Path, java.lang.Boolean]()
    val noAttributes = Array.empty[FileAttribute[_]]

    private def checkName(component: Path): Unit = if (isWindows) {
      val specials = raw"(?i)CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9]".r
      val name = component.toString
      def warnSpecial(): Unit = frontendAccess.backendReporting.warning(NoPosition, s"path component is special Windows device: ${name}")
      specials.findPrefixOf(name).foreach(prefix => if (prefix.length == name.length || name(prefix.length) == '.') warnSpecial())
    }

    def ensureDirForPath(baseDir: Path, filePath: Path): Unit = {
      import java.lang.Boolean.TRUE
      val parent = filePath.getParent
      if (!builtPaths.containsKey(parent)) {
        parent.iterator.forEachRemaining(checkName)
        try Files.createDirectories(parent, noAttributes: _*)
        catch {
          case e: FileAlreadyExistsException =>
            // `createDirectories` reports this exception if `parent` is an existing symlink to a directory
            // but that's fine for us (and common enough, `scalac -d /tmp` on mac targets symlink).
            if (!Files.isDirectory(parent))
              throw new FileConflictException(s"Can't create directory $parent; there is an existing (non-directory) file in its path", e)
        }
        builtPaths.put(baseDir, TRUE)
        var current = parent
        while ((current ne null) && (null ne builtPaths.put(current, TRUE))) {
          current = current.getParent
        }
      }
      checkName(filePath.getFileName())
    }

    // the common case is that we are creating a new file, and on MS Windows the create and truncate is expensive
    // because there is not an options in the windows API that corresponds to this so the truncate is applied as a separate call
    // even if the file is new.
    // as this is rare, its best to always try to create a new file, and it that fails, then open with truncate if that fails

    private val fastOpenOptions = util.EnumSet.of(StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)
    private val fallbackOpenOptions = util.EnumSet.of(StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)

    override def writeFile(relativePath: String, bytes: Array[Byte]): Unit = {
      val path = base.resolve(relativePath)
      try {
        ensureDirForPath(base, path)
        val os = if (isWindows) {
          try FileChannel.open(path, fastOpenOptions)
          catch {
            case _: FileAlreadyExistsException => FileChannel.open(path, fallbackOpenOptions)
          }
        } else FileChannel.open(path, fallbackOpenOptions)

        try {
          os.write(ByteBuffer.wrap(bytes), 0L)
        } catch {
          case ex: ClosedByInterruptException =>
            try {
              Files.deleteIfExists(path) // don't leave a empty of half-written classfile around after an interrupt
            } catch {
              case _: Throwable =>
            }
            throw ex
        }
        os.close()
      } catch {
        case e: FileConflictException =>
          frontendAccess.backendReporting.error(NoPosition, s"error writing $path: ${e.getMessage}")
        case e: java.nio.file.FileSystemException =>
          if (frontendAccess.compilerSettings.debug)
            e.printStackTrace()
          frontendAccess.backendReporting.error(NoPosition, s"error writing $path: ${e.getClass.getName} ${e.getMessage}")
      }
    }

    override def close(): Unit = ()
  }

  private final class VirtualFileWriter(base: AbstractFile) extends FileWriter {
    private def getFile(base: AbstractFile, path: String): AbstractFile = {
      def ensureDirectory(dir: AbstractFile): AbstractFile =
        if (dir.isDirectory) dir
        else throw new FileConflictException(s"${base.path}/${path}: ${dir.path} is not a directory")
      val components = path.split('/')
      var dir = base
      for (i <- 0 until components.length - 1) dir = ensureDirectory(dir) subdirectoryNamed components(i).toString
      ensureDirectory(dir) fileNamed components.last.toString
    }

    private def writeBytes(outFile: AbstractFile, bytes: Array[Byte]): Unit = {
      val out = new DataOutputStream(outFile.bufferedOutput)
      try out.write(bytes, 0, bytes.length)
      finally out.close()
    }

    override def writeFile(relativePath: String, bytes: Array[Byte]): Unit = {
      val outFile = getFile(base, relativePath)
      writeBytes(outFile, bytes)
    }
    override def close(): Unit = ()
  }

  /** Can't output a file due to the state of the file system. */
  class FileConflictException(msg: String, cause: Throwable = null) extends IOException(msg, cause)
}
