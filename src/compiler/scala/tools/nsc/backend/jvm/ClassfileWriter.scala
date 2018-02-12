package scala.tools.nsc.backend.jvm

import java.io.{BufferedOutputStream, DataOutputStream, FileOutputStream, IOException}
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.FileAttribute
import java.nio.file.{FileAlreadyExistsException, Files, Path, Paths, StandardOpenOption}
import java.util
import java.util.concurrent.ConcurrentHashMap
import java.util.zip.{CRC32, Deflater, ZipEntry, ZipOutputStream}

import scala.reflect.internal.util.{NoPosition, Statistics}
import scala.tools.nsc.Global
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.io.AbstractFile

/**
 * The interface to writing classfiles. GeneratedClassHandler calls these methods to generate the
 * directory and files that are created, and eventually calls `close` when the writing is complete.
 *
 * The companion object is responsible for constructing a appropriate and optimal implementation for
 * the supplied settings.
 *
 * Operations are threadsafe.
 */
sealed trait ClassfileWriter {
  /**
   * Write a classfile
   */
  def write(unit: SourceUnit, name: InternalName, bytes: Array[Byte])

  /**
   * Close the writer. Behavior is undefined after a call to `close`.
   */
  def close() : Unit
}

object ClassfileWriter {
  private def getDirectory(dir: String): Path = Paths.get(dir)

  def apply(global: Global): ClassfileWriter = {
    //Note dont import global._ - its too easy to leak non threadsafe structures
    import global.{cleanup, genBCode, log, settings, statistics}
    def jarManifestMainClass: Option[String] = settings.mainClass.valueSetByUser.orElse {
      cleanup.getEntryPoints match {
        case List(name) => Some(name)
        case es =>
          if (es.isEmpty) log("No Main-Class designated or discovered.")
          else log(s"No Main-Class due to multiple entry points:\n  ${es.mkString("\n  ")}")
          None
      }
    }

    def singleWriter(file: AbstractFile): UnderlyingClassfileWriter = {
      if (file hasExtension "jar") {
        new JarClassWriter(file, jarManifestMainClass, settings.YjarCompressionLevel.value)
      } else if (file.isVirtual) {
        new VirtualClassWriter()
      } else if (file.isDirectory) {
        new DirClassWriter(genBCode.postProcessorFrontendAccess)
      } else {
        throw new IllegalStateException(s"don't know how to handle an output of $file [${file.getClass}]")
      }
    }

    val basicClassWriter = settings.outputDirs.getSingleOutput match {
      case Some(dest) => singleWriter(dest)
      case None =>
        val distinctOutputs: Set[AbstractFile] = settings.outputDirs.outputs.map(_._2)(scala.collection.breakOut)
        if (distinctOutputs.size == 1) singleWriter(distinctOutputs.head)
        else new MultiClassWriter(distinctOutputs.map { output: AbstractFile => output -> singleWriter(output) }(scala.collection.breakOut))
    }

    val withAdditionalFormats = if (settings.Ygenasmp.valueSetByUser.isEmpty && settings.Ydumpclasses.valueSetByUser.isEmpty) basicClassWriter else {
      val asmp = settings.Ygenasmp.valueSetByUser map { dir: String => new AsmClassWriter(getDirectory(dir), genBCode.postProcessorFrontendAccess) }
      val dump = settings.Ydumpclasses.valueSetByUser map { dir: String => new DumpClassWriter(getDirectory(dir), genBCode.postProcessorFrontendAccess) }
      new AllClassWriter(basicClassWriter, asmp, dump)
    }

    if (statistics.enabled) new WithStatsWriter(statistics, withAdditionalFormats) else withAdditionalFormats
  }

  /**
   * a trait to specify the Classfilewriters that actually write, rather than layer functionality
   */
  sealed trait UnderlyingClassfileWriter extends ClassfileWriter

  private final class JarClassWriter(file: AbstractFile, mainClass: Option[String], compressionLevel: Int) extends UnderlyingClassfileWriter {
    //keep these imports local - avoid confusion with scala naming
    import java.util.jar.Attributes.Name
    import java.util.jar.{JarOutputStream, Manifest}
    val storeOnly = compressionLevel == Deflater.NO_COMPRESSION

    val jarWriter: JarOutputStream = {
      val manifest = new Manifest()
      mainClass foreach { c => manifest.getMainAttributes.put(Name.MAIN_CLASS, c) }
      val jar = new JarOutputStream(new BufferedOutputStream(new FileOutputStream(file.file), 64000), manifest)
      jar.setLevel(compressionLevel)
      if (storeOnly) jar.setMethod(ZipOutputStream.STORED)
      jar
    }

    lazy val crc = new CRC32

    override def write(unit: SourceUnit, className: InternalName, bytes: Array[Byte]): Unit = this.synchronized {
      val path = className + ".class"
      val entry = new ZipEntry(path)
      if (storeOnly) {
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

  private sealed class DirClassWriter(frontendAccess: PostProcessorFrontendAccess) extends UnderlyingClassfileWriter {
    val builtPaths = new ConcurrentHashMap[Path, java.lang.Boolean]()
    val noAttributes = Array.empty[FileAttribute[_]]

    def ensureDirForPath(baseDir: Path, filePath: Path): Unit = {
      import java.lang.Boolean.TRUE
      val parent = filePath.getParent
      if (!builtPaths.containsKey(parent)) {
        try Files.createDirectories(parent, noAttributes: _*)
        catch {
          case e: FileAlreadyExistsException =>
            throw new FileConflictException(s"Can't create directory $parent; there is an existing (non-directory) file in its path", e)
        }
        builtPaths.put(baseDir, TRUE)
        var current = parent
        while ((current ne null) && (null ne builtPaths.put(current, TRUE))) {
          current = current.getParent
        }
      }
    }

    protected def getPath(unit: SourceUnit, className: InternalName) = unit.outputPath.resolve(className + ".class")

    protected def formatData(rawBytes: Array[Byte]) = rawBytes

    protected def qualifier: String = ""

    // the common case is that we are are creating a new file, and on MS Windows the create and truncate is expensive
    // because there is not an options in the windows API that corresponds to this so the truncate is applied as a separate call
    // even if the file is new.
    // as this is rare, its best to always try to create a new file, and it that fails, then open with truncate if that fails

    private val fastOpenOptions = util.EnumSet.of(StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)
    private val fallbackOpenOptions = util.EnumSet.of(StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)

    override def write(unit: SourceUnit, className: InternalName, rawBytes: Array[Byte]): Unit = try {
      val path = getPath(unit, className)
      val bytes = formatData(rawBytes)
      ensureDirForPath(unit.outputPath, path)
      val os = try FileChannel.open(path, fastOpenOptions)
      catch {
        case _: FileAlreadyExistsException => FileChannel.open(path, fallbackOpenOptions)
      }

      os.write(ByteBuffer.wrap(bytes), 0L)
      os.close()
    } catch {
      case e: FileConflictException =>
        frontendAccess.backendReporting.error(NoPosition, s"error writing $className$qualifier: ${e.getMessage}")
      case e: java.nio.file.FileSystemException =>
        if (frontendAccess.compilerSettings.debug)
          e.printStackTrace()
        frontendAccess.backendReporting.error(NoPosition, s"error writing $className$qualifier: ${e.getClass.getName} ${e.getMessage}")

    }
    override def close(): Unit = ()
  }

  private final class AsmClassWriter(
      asmOutputPath: Path,
      frontendAccess: PostProcessorFrontendAccess)
    extends DirClassWriter(frontendAccess) {
    override protected def getPath(unit: SourceUnit, className: InternalName) = asmOutputPath.resolve(className + ".asmp")

    override protected def formatData(rawBytes: Array[Byte]) = AsmUtils.textify(AsmUtils.readClass(rawBytes)).getBytes(StandardCharsets.UTF_8)

    override protected def qualifier: String = " [for asmp]"
  }

  private final class DumpClassWriter(
      dumpOutputPath: Path,
      frontendAccess: PostProcessorFrontendAccess)
    extends DirClassWriter(frontendAccess) {
    override protected def getPath(unit: SourceUnit, className: InternalName) = dumpOutputPath.resolve(className + ".class")

    override protected def qualifier: String = " [for dump]"
  }

  private final class VirtualClassWriter() extends UnderlyingClassfileWriter {
    private def getFile(base: AbstractFile, clsName: String, suffix: String): AbstractFile = {
      def ensureDirectory(dir: AbstractFile): AbstractFile =
        if (dir.isDirectory) dir
        else throw new FileConflictException(s"${base.path}/$clsName$suffix: ${dir.path} is not a directory")

      var dir = base
      val pathParts = clsName.split("[./]").toList
      for (part <- pathParts.init) dir = ensureDirectory(dir) subdirectoryNamed part
      ensureDirectory(dir) fileNamed pathParts.last + suffix
    }

    private def writeBytes(outFile: AbstractFile, bytes: Array[Byte]): Unit = {
      val out = new DataOutputStream(outFile.bufferedOutput)
      try out.write(bytes, 0, bytes.length)
      finally out.close()
    }

    override def write(unit: SourceUnit, className: InternalName, bytes: Array[Byte]): Unit = {
      val outFile = getFile(unit.outputDir, className, ".class")
      writeBytes(outFile, bytes)
    }

    override def close(): Unit = ()
  }

  private final class MultiClassWriter(underlying: Map[AbstractFile, UnderlyingClassfileWriter]) extends ClassfileWriter {
    private def getUnderlying(unit: SourceUnit) = underlying.getOrElse(unit.outputDir, {
      throw new Exception(s"Cannot determine output directory for ${unit.sourceFile} with output ${unit.outputDir}. Configured outputs are ${underlying.keySet}")
    })

    override def write(unit: SourceUnit, className: InternalName, bytes: Array[Byte]): Unit = {
      getUnderlying(unit).write(unit, className, bytes)
    }

    override def close(): Unit = underlying.values.foreach(_.close())
  }

  private final class AllClassWriter(basic: ClassfileWriter, asmp: Option[UnderlyingClassfileWriter], dump: Option[UnderlyingClassfileWriter]) extends ClassfileWriter {
    override def write(unit: SourceUnit, className: InternalName, bytes: Array[Byte]): Unit = {
      basic.write(unit, className, bytes)
      asmp.foreach(_.write(unit, className, bytes))
      dump.foreach(_.write(unit, className, bytes))
    }

    override def close(): Unit = {
      basic.close()
      asmp.foreach(_.close())
      dump.foreach(_.close())
    }
  }

  private final class WithStatsWriter(statistics: Statistics with BackendStats, underlying: ClassfileWriter) extends ClassfileWriter {
    override def write(unit: SourceUnit, className: InternalName, bytes: Array[Byte]): Unit = {
      val snap = statistics.startTimer(statistics.bcodeWriteTimer)
      underlying.write(unit, className, bytes)
      statistics.stopTimer(statistics.bcodeWriteTimer, snap)
    }

    override def close(): Unit = underlying.close()
  }
}

/** Can't output a file due to the state of the file system. */
class FileConflictException(msg: String, cause:Throwable = null) extends IOException(msg, cause)
