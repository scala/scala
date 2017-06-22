package scala.tools.nsc.classpath

import java.io.{File, IOException, InputStream, OutputStream}
import java.net.URL
import java.nio.file._
import java.nio.file.attribute.{BasicFileAttributes, FileTime}
import java.util.zip.{ZipEntry, ZipFile}

import scala.collection.{Seq, mutable}
import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.io.AbstractFile
import scala.tools.nsc.classpath.ClassPathWatcher.{BaseChangeListener, DirectoryChangeListener, FileChangeListener}
import scala.tools.nsc.util.ClassPath


abstract class FileClassPath extends ClassPath {

  val rootFile: File

  assert(rootFile != null, "file cannot be null")

  override final def asURLs: Seq[URL] = Seq(rootFile.toURI.toURL)

  override final def asClassPathStrings: Seq[String] = Seq(rootFile.getPath)

}

abstract class BasicClassPath extends FileClassPath {
  override private[nsc] final def packages(inPackage: String) = list(inPackage).packages
}

abstract class CommonClassPath[FileEntryType <: SingleClassRepresentation] extends BasicClassPath {
  self: TypedClassPath[FileEntryType] =>

  protected type ContentType >: Null <: ClassPathContent[FileEntryType]
  protected var content: ContentType = _

  /** Allows to get entries for packages and classes merged with sources possibly in one pass. */
  override private[nsc] def list(inPackage: String) = content.data(inPackage).list

  override private[nsc] def hasPackage(pkg: String) = content.data(pkg).isEmpty

  private[nsc] def files(inPackage: String): Seq[FileEntryType] = content.data(inPackage).files

  //liveness checking and the forwarderes
  protected val livenessChecker : LivenessChecker


  final override def startInUse(executionContext: ExecutionContext, proactive: Boolean) =
    livenessChecker.startInUse(proactive)

  final override def endInUse(): Unit = livenessChecker.endInUse()

  final override def makeCacheValid(executionContext: ExecutionContext, proactive: Boolean): Long =
    livenessChecker.validateAndLastModificationTime()

  // liveness checking callabcks
  // callback are only called with a lock managed by the livenessChecker
  // and the livenessChecker ensure that content is not null when the classpath is in use
  private[classpath] def contents:Option[ContentType] = Option(content)

  private[classpath] def markInvalid() = {
    content = null
  }

  private[classpath] def ensureContent(proactive: Boolean):ContentType = {
    contents.getOrElse{
      content = newContent()
      if (proactive) {
        content.reOpen()
        //TODO - should we move the executors to the policy
        //should be Ok as long as we can pick up the time in instrumentation
        // TODO content.startScan()
      }
      content
    }
  }
  protected def newContent():ContentType

}

abstract class CommonZipClassPath[FileEntryType <: SingleClassRepresentation](
               override val rootFile: File, policy:LivenessChecker.ForFile) extends CommonClassPath[FileEntryType] {
  self: TypedClassPath[FileEntryType] =>
  type ContentType = ZipArchiveContent[FileEntryType]

  val livenessChecker = policy(this)

  override protected def newContent(): ZipArchiveContent[FileEntryType] = new ZipArchiveContent(rootFile, isValidFilename, toRepr)
}


abstract class CommonDirClassPath[FileEntryType <: SingleClassRepresentation](
               override val rootFile: File, policy:LivenessChecker.ForDir)
  extends CommonClassPath[FileEntryType] {

  self: TypedClassPath[FileEntryType] =>
  type ContentType = DirArchiveContent[FileEntryType]

  val livenessChecker:LivenessChecker = policy(this)

  override protected def newContent(): DirArchiveContent[FileEntryType] =
    new DirArchiveContent(rootFile, isValidFilename, toRepr, livenessChecker)
}

class RawZipClassesPath(rootFile: File, policy:LivenessChecker.ForFile)
  extends CommonZipClassPath[ClassFileEntry](rootFile, policy) with CommonNoSourcesClassPath

class RawZipSourcesPath(rootFile: File, policy:LivenessChecker.ForFile)
  extends CommonZipClassPath[SourceFileEntry](rootFile, policy) with CommonNoClassesClassPath

class RawDirClassesPath(rootFile: File, policy:LivenessChecker.ForDir)
  extends CommonDirClassPath[ClassFileEntry](rootFile, policy) with CommonNoSourcesClassPath

class RawDirSourcesPath(rootFile: File, policy:LivenessChecker.ForDir)
  extends CommonDirClassPath[SourceFileEntry](rootFile, policy) with CommonNoClassesClassPath

abstract class ClassPathContent[FileEntryType <: SingleClassRepresentation](file: File,
                                                                            isValid: String => Boolean,
                                                                            toRepr: AbstractFile => FileEntryType) {

  def startScan(executionContext: ExecutionContext): Unit = {
    Future(data)(executionContext)
  }

  final lazy val data: Map[String, PackageInfo] = buildData()

  protected def buildData(): Map[String, PackageInfo]

  class PackageInfo(val packageName: String, val list: ClassPathEntries, val files: Seq[FileEntryType], val packages: Seq[PackageEntryImpl]) {
    lazy val filesByName: Map[String, AbstractFile] = files.map {
      file => file.name -> file.file
    }(collection.breakOut)

    def isEmpty :Boolean = list.classesAndSources.isEmpty
  }

  def reOpen(): Unit

  def close(): Unit

}

class ZipArchiveContent[FileEntryType <: SingleClassRepresentation](zipFile: File,
                                                                    isValid: String => Boolean,
                                                                    toRepr: AbstractFile => FileEntryType) extends ClassPathContent(zipFile, isValid, toRepr) {
  //TODO async
  override def reOpen():Unit = withOpenZipFile

  def withOpenZipFile() :ZipFile = this.synchronized {
    zip.getOrElse {
      val newZip = new ZipFile(zipFile)
      zip = Some(newZip)
      newZip
    }
  }

  private var zip: Option[ZipFile] = Some(new ZipFile(zipFile))

  //TODO async
  override def close() = this.synchronized {
    zip.foreach { z =>
      z.close()
    }
    zip = None
  }


  protected def buildData(): Map[String, PackageInfo] = {
    //TODO use ser format
    class MutablePackageInfo(packageName: String) {
      val packages = Vector.newBuilder[PackageEntryImpl]
      val files = Vector.newBuilder[FileEntryType]

      def result(): PackageInfo = {
        val packagesResult = packages.result()
        val filesResult = files.result()
        val entries = new ClassPathEntries(packagesResult, filesResult)
        new PackageInfo(packageName, entries, filesResult, packagesResult)
      }
    }
    val staging = mutable.Map[String, MutablePackageInfo]()

    def getPackageInfo(packageName: String): MutablePackageInfo = {
      staging.get(packageName) match {
        case Some(res) => res
        case None =>
          val res = new MutablePackageInfo(packageName)
          staging.update(packageName, res)
          if (packageName != "") {
            val parentPackageName = packageName.take(packageName.lastIndexOf('.'))
            getPackageInfo(parentPackageName).packages += PackageEntryImpl(packageName)
          }
          res
      }
    }

    this.synchronized {
      val toClose = if (zip isEmpty) Some(new ZipFile(zipFile)) else None
      val myZip = zip.orElse(toClose).get
      try {
        val entries = myZip.entries()
        while (entries.hasMoreElements) {
          val entry = entries.nextElement()
          val name = entry.getName
          if (isValid(name)) {
            val dirSeparator = name.lastIndexOf('/')
            val packageName = name.take(dirSeparator).replace('/', '.')
            val fileName = name.substring(dirSeparator + 1, name.lastIndexOf('.'))
            getPackageInfo(packageName).files += toRepr(new ZipEntryFile(entry))
          }
        }
      } finally toClose foreach (_.close())
    }
    val res: Map[String, PackageInfo] = staging.map {
      case (k, v) => (k -> v.result())
    }(collection.breakOut)
    res.withDefaultValue(new PackageInfo("", ClassPathEntries.empty, Nil, Nil))
  }

  private class ZipEntryFile(private val entry: ZipEntry) extends AbstractFile {

    /** Returns the name of this abstract file. */
    override val name: String = entry.getName.substring(entry.getName.lastIndexOf('/') + 1)

    /** returns an input stream so the file can be read */
    override def input: InputStream = try {
      //      println(s"reading ${entry.getName}")
      withOpenZipFile().getInputStream(entry)
    } catch {
      case t: Throwable =>
        println(s"failed to read ${ZipArchiveContent.this.zipFile}")
        t.printStackTrace()
        throw t
    }

    override def lastModified: Long = entry.getLastModifiedTime.toMillis

    /** size of this file if it is a concrete file. */
    //needed to make toByteArray efficient
    override def sizeOption = Some(entry.getSize.toInt)


    //the rest is minimal support from AbstractFile API
    //used by toString()
    override def path: String = entry.getName

    override def absolute: AbstractFile = notImplemented

    override def container: AbstractFile = notImplemented

    override def file: File = notImplemented

    override def create(): Unit = notImplemented

    override def delete(): Unit = notImplemented

    override def isDirectory: Boolean = notImplemented

    override def output: OutputStream = notImplemented

    override def iterator: Iterator[AbstractFile] = notImplemented

    override def lookupName(name: String, directory: Boolean): AbstractFile = notImplemented

    override def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = notImplemented

    private def notImplemented = ???

  }

}

class DirArchiveContent[FileEntryType <: SingleClassRepresentation](rootDir: File,
                                                                    isValid: String => Boolean,
                                                                    toRepr: AbstractFile => FileEntryType,
                                                                    livenessChecker: LivenessChecker)
  extends ClassPathContent(rootDir, isValid, toRepr) {
  override def reOpen() = ()

  override def close() = ()

  protected def buildData(): Map[String, PackageInfo] = {
    livenessChecker.removeAllWatches()
    //TODO use ser format
    class MutablePackageInfo(val packageName: String, val parent: MutablePackageInfo) {
      val packages = Vector.newBuilder[PackageEntryImpl]
      val files = Vector.newBuilder[FileEntryType]

      def result(): PackageInfo = {
        val packagesResult = packages.result()
        val filesResult = files.result()
        val entries = new ClassPathEntries(packagesResult, filesResult)
        new PackageInfo(packageName, entries, filesResult, packagesResult)
      }
    }
    val staging = mutable.Map[String, MutablePackageInfo]()

    class DirVisitor extends FileVisitor[Path] {
      val rootPath = rootDir.toPath
      var current: MutablePackageInfo = null
      val resultsBuilder = Map.newBuilder[String, PackageInfo]

      override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
        if (exc ne null) throw exc
        val result = current.result()
        current = current.parent
        if (!result.isEmpty) {
          if (current ne null)
            current.packages += PackageEntryImpl(current.packageName + "." + dir.getFileName.toString)
          resultsBuilder += ((result.packageName, result))
        }
        livenessChecker.addDirWatch(dir)
        FileVisitResult.CONTINUE
      }


      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        if (isValid(file.getFileName.toString)) {
          val name = file.relativize(rootPath).toString
          val dirSeparator = name.lastIndexOf('/')
          val packageName = name.take(dirSeparator).replace('/', '.')
          val fileName = name.substring(dirSeparator + 1, name.lastIndexOf('.'))
          current.files += toRepr(new PathFile(file, attrs))
        }
        FileVisitResult.CONTINUE
      }

      override def visitFileFailed(file: Path, exc: IOException): FileVisitResult = throw exc

      override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
        val name = dir.getFileName.toString
        if (name.startsWith(".") || name == "META-INF") FileVisitResult.SKIP_SUBTREE
        else {
          current = new MutablePackageInfo("", current)
          FileVisitResult.CONTINUE
        }
      }
    }
    this.synchronized {
      val visitor = new DirVisitor
      Files.walkFileTree(visitor.rootPath, visitor)
      val res: Map[String, PackageInfo] = staging.map {
        case (k, v) => (k -> v.result())
      }(collection.breakOut)
      res.withDefaultValue(new PackageInfo("", ClassPathEntries.empty, Nil, Nil))
    }
  }

  private class PathFile(private val jPath: Path, attr: BasicFileAttributes) extends AbstractFile {

    /** Returns the name of this abstract file. */
    override val name: String = jPath.getFileSystem.toString

    /** returns an input stream so the file can be read */
    override def input: InputStream = Files.newInputStream(jPath)

    override def lastModified: Long = attr.lastAccessTime().toMillis


    /** size of this file if it is a concrete file. */
    //needed to make toByteArray efficient
    override def sizeOption = Some(attr.size().toInt)

    override def absolute: AbstractFile = notImplemented

    override def container: AbstractFile = notImplemented

    override def file: File = notImplemented

    //the rest is minimal support from AbstractFile API
    //used by toString()
    override def path: String = jPath.toString

    override def create(): Unit = notImplemented

    override def delete(): Unit = notImplemented

    override def isDirectory: Boolean = notImplemented

    override def output: OutputStream = notImplemented

    override def iterator: Iterator[AbstractFile] = notImplemented

    override def lookupName(name: String, directory: Boolean): AbstractFile = notImplemented

    override def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = notImplemented

    private def notImplemented = ???

  }

}
