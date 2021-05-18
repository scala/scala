/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect
package io

import java.net.URL
import java.io.{ByteArrayInputStream, FilterInputStream, IOException, InputStream}
import java.io.{File => JFile}
import java.util.concurrent.{ArrayBlockingQueue, TimeUnit}
import java.util.zip.{ZipEntry, ZipFile, ZipInputStream}
import java.util.jar.Manifest
import scala.collection.mutable
import scala.collection.JavaConverters._
import scala.annotation.tailrec
import scala.reflect.internal.JDK9Reflectors
import ZipArchive._

/** An abstraction for zip files and streams.  Everything is written the way
 *  it is for performance: we come through here a lot on every run.  Be careful
 *  about changing it.
 *
 *  @author  Philippe Altherr (original version)
 *  @author  Paul Phillips (this one)
 *  @version 2.0,
 *
 *  ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
object ZipArchive {
  private[io] val closeZipFile = sys.props.get("scala.classpath.closeZip").map(_.toBoolean).getOrElse(false)

  private[io] final val RootEntry = "/"

  /**
   * @param   file  a File
   * @return  A ZipArchive if `file` is a readable zip file, otherwise null.
   */
  def fromFile(file: File): FileZipArchive = fromFile(file.jfile)
  def fromFile(file: JFile): FileZipArchive =
    try   { new FileZipArchive(file) }
    catch { case _: IOException => null }

  /**
   * @param   url  the url of a zip file
   * @return  A ZipArchive backed by the given url.
   */
  def fromURL(url: URL): URLZipArchive = new URLZipArchive(url)

  def fromManifestURL(url: URL): AbstractFile = new ManifestResources(url)

  private def dirName(path: String)  = splitPath(path, front = true)
  private def baseName(path: String) = splitPath(path, front = false)
  private def splitPath(path0: String, front: Boolean): String = {
    val isDir = path0.charAt(path0.length - 1) == '/'
    val path  = if (isDir) path0.substring(0, path0.length - 1) else path0
    val idx   = path.lastIndexOf('/')

    if (idx < 0)
      if (front) RootEntry
      else path
    else
      if (front) path.substring(0, idx + 1)
      else path.substring(idx + 1)
  }
}
/** ''Note:  This library is considered experimental and should not be used unless you know what you are doing.'' */
abstract class ZipArchive(override val file: JFile, release: Option[String]) extends AbstractFile with Equals {
  self =>
  def this(file: JFile) = this(file, None)

  override lazy val canonicalPath = super.canonicalPath

  override def underlyingSource = Some(this)
  def isDirectory = true
  def lookupName(name: String, directory: Boolean) = unsupported()
  def lookupNameUnchecked(name: String, directory: Boolean) = unsupported()
  def create()  = unsupported()
  def delete()  = unsupported()
  def output    = unsupported()
  def container = unsupported()
  def absolute  = unsupported()

  /** ''Note:  This library is considered experimental and should not be used unless you know what you are doing.'' */
  sealed abstract class Entry(path: String) extends VirtualFile(baseName(path), path) {
    // have to keep this name for compat with sbt's compiler-interface
    def getArchive: ZipFile = null
    override def underlyingSource = Some(self)
    override def toString = self.path + "(" + path + ")"
    override def unsafeToByteArray: Array[Byte] = toByteArray
  }

  /** ''Note:  This library is considered experimental and should not be used unless you know what you are doing.'' */
  class DirEntry(path: String) extends Entry(path) {
    val entries = mutable.HashMap[String, Entry]()

    override def isDirectory = true
    override def iterator: Iterator[Entry] = entries.valuesIterator
    override def lookupName(name: String, directory: Boolean): Entry = {
      if (directory) entries.get(name + "/").orNull
      else entries.get(name).orNull
    }
  }

  private def ensureDir(dirs: java.util.Map[String, DirEntry], path: String): DirEntry = {
    dirs get path match {
      case null =>
        val parent = ensureDir(dirs, dirName(path))
        val dir = new DirEntry(path)
        parent.entries(baseName(path)) = dir
        dirs.put(path, dir)
        dir
      case v => v
    }
  }

  @volatile private[this] var lastDirName: String = RootEntry
  private def dirNameUsingLast(name: String): String = {
    val last = lastDirName
    if (name.length > last.length + 1 && name.startsWith(last) && name.charAt(last.length) == '/' && name.indexOf('/', last.length + 1) == -1) {
      // OPT: Avoid string allocation when reading successive entries in a zip index from the same directory.
      lastDirName
    } else {
      val result = dirName(name)
      lastDirName = result
      result
    }
  }
  protected def getDir(dirs: java.util.Map[String, DirEntry], entry: ZipEntry): DirEntry = {
    if (entry.isDirectory) ensureDir(dirs, entry.getName)
    else ensureDir(dirs, dirNameUsingLast(entry.getName))
  }

  def close(): Unit
}
/** ''Note:  This library is considered experimental and should not be used unless you know what you are doing.'' */
final class FileZipArchive(file: JFile, release: Option[String]) extends ZipArchive(file, release) {
  def this(file: JFile) = this(file, None)
  private object zipFilePool {
    private[this] val zipFiles = new ArrayBlockingQueue[ZipFile](Runtime.getRuntime.availableProcessors())

    def acquire: ZipFile = {
      val zf = zipFiles.poll(0, TimeUnit.MILLISECONDS)
      zf match {
        case null =>
          openZipFile()
        case _ =>
          zf
      }
    }

    def release(zf: ZipFile): Unit = {
      if (!zipFiles.offer(zf, 0, TimeUnit.MILLISECONDS))
        zf.close()
    }

    def close(): Unit = {
      val zipFilesToClose = new java.util.ArrayList[ZipFile]
      zipFiles.drainTo(zipFilesToClose)
      zipFilesToClose.iterator().forEachRemaining(_.close())
    }
  }

  private[this] def openZipFile(): ZipFile = try {
    release match {
      case Some(r) if file.getName.endsWith(".jar") =>
        val releaseVersion = JDK9Reflectors.runtimeVersionParse(r)
        JDK9Reflectors.newJarFile(file, true, ZipFile.OPEN_READ, releaseVersion)
      case _ =>
        new ZipFile(file)
    }
  } catch {
    case ioe: IOException => throw new IOException("Error accessing " + file.getPath, ioe)
  }

  private[this] class LazyEntry(
    name: String,
    time: Long,
    size: Int
  ) extends Entry(name) {
    override def lastModified: Long = time // could be stale
    override def input: InputStream = {
      val zipFile  = openZipFile()
      val entry    = zipFile.getEntry(name) // with `-release`, returns the correct version under META-INF/versions
      val delegate = zipFile.getInputStream(entry)
      new FilterInputStream(delegate) {
        override def close(): Unit = { zipFile.close() }
      }
    }
    override def sizeOption: Option[Int] = Some(size) // could be stale
  }

  // keeps file handle(s) open to ZipFile in the pool this.zipFiles,
  // which forbids file mutation on Windows, and leaks memory on all OS (typically by stopping
  // classloaders from being garbage collected). But is slightly faster than LazyEntry.
  //
  // Note: scala/scala#7366 / scala/scala#7644, LeakyEntry _does_ close the file when `Global.close` is called,
  // or after a short delay specified by FileBasedCache.deferCloseMs if classpath caching is enabled.
  // So the file handle "leak" is far less a problem than it used do be.
  private[this] class LeakyEntry(
    name: String,
    time: Long,
    size: Int
  ) extends Entry(name) {
    override def lastModified: Long = time // could be stale
    override def input: InputStream = {
      val zipFile  = zipFilePool.acquire
      val entry    = zipFile.getEntry(name) // with `-release`, returns the correct version under META-INF/versions
      val delegate = zipFile.getInputStream(entry)
      new FilterInputStream(delegate) {
        override def close(): Unit = { zipFilePool.release(zipFile) }
      }
    }
    override def sizeOption: Option[Int] = Some(size) // could be stale
  }

  private[this] val dirs = new java.util.HashMap[String, DirEntry]()
  lazy val root: DirEntry = {
    val root = new DirEntry(RootEntry)
    dirs.put(RootEntry, root)
    val zipFile = openZipFile()
    val enum    = zipFile.entries()

    try {
      while (enum.hasMoreElements) {
        val zipEntry = enum.nextElement
        if (!zipEntry.getName.startsWith("META-INF/versions/")) {
          if (!zipEntry.isDirectory) {
            val dir = getDir(dirs, zipEntry)
            val f =
              if (ZipArchive.closeZipFile)
                new LazyEntry(
                  zipEntry.getName,
                  zipEntry.getTime,
                  zipEntry.getSize.toInt)
              else
                new LeakyEntry(zipEntry.getName,
                               zipEntry.getTime,
                               zipEntry.getSize.toInt)

            dir.entries(f.name) = f
          }
        }
      }
    } finally {
      if (!ZipArchive.closeZipFile)
        zipFilePool.release(zipFile)
    }
    root
  }

  lazy val allDirs: java.util.Map[String, DirEntry] = { root; dirs }

  def iterator: Iterator[Entry] = root.iterator

  def name         = file.getName
  def path         = file.getPath
  def input        = File(file).inputStream()
  def lastModified = file.lastModified

  override def sizeOption = Some(file.length.toInt)
  override def canEqual(other: Any) = other.isInstanceOf[FileZipArchive]
  override def hashCode() = file.hashCode
  override def equals(that: Any) = that match {
    case x: FileZipArchive => file.getAbsoluteFile == x.file.getAbsoluteFile
    case _                 => false
  }
  override def close(): Unit = {
    zipFilePool.close()
  }
}
/** ''Note:  This library is considered experimental and should not be used unless you know what you are doing.'' */
final class URLZipArchive(val url: URL) extends ZipArchive(null) {
  def iterator: Iterator[Entry] = {
    val root     = new DirEntry(RootEntry)
    val dirs     = new java.util.HashMap[String, DirEntry]()
    dirs.put(RootEntry, root)
    val in       = new ZipInputStream(new ByteArrayInputStream(Streamable.bytes(input)))
    closeables ::= in

    @tailrec def loop() {
      val zipEntry = in.getNextEntry()
      class EmptyFileEntry() extends Entry(zipEntry.getName) {
        override def toByteArray: Array[Byte] = null
        override def sizeOption = Some(0)
      }
      class FileEntry() extends Entry(zipEntry.getName) {
        override val toByteArray: Array[Byte] = {
          val len    = zipEntry.getSize().toInt
          val arr    = if (len == 0) Array.emptyByteArray else new Array[Byte](len)
          var offset = 0

          def loop() {
            if (offset < len) {
              val read = in.read(arr, offset, len - offset)
              if (read >= 0) {
                offset += read
                loop()
              }
            }
          }
          loop()

          if (offset == arr.length) arr
          else throw new IOException("Input stream truncated: read %d of %d bytes".format(offset, len))
        }
        override def sizeOption = Some(zipEntry.getSize().toInt)
      }

      if (zipEntry != null) {
        val dir = getDir(dirs, zipEntry)
        if (zipEntry.isDirectory)
          dir
        else {
          val f = if (zipEntry.getSize() == 0) new EmptyFileEntry() else new FileEntry()
          dir.entries(f.name) = f
        }
        in.closeEntry()
        loop()
      }
    }

    loop()
    try root.iterator
    finally dirs.clear()
  }

  def name  = url.getFile()
  def path  = url.getPath()
  def input = url.openStream()
  def lastModified =
    try url.openConnection().getLastModified()
    catch { case _: IOException => 0 }

  override def canEqual(other: Any) = other.isInstanceOf[URLZipArchive]
  override def hashCode() = url.hashCode
  override def equals(that: Any) = that match {
    case x: URLZipArchive => url == x.url
    case _                => false
  }
  private[this] var closeables: List[java.io.Closeable] = Nil
  def close(): Unit = {
    closeables.foreach(_.close())
  }
}

final class ManifestResources(val url: URL) extends ZipArchive(null) {
  def iterator = {
    val root     = new DirEntry(RootEntry)
    val dirs     = new java.util.HashMap[String, DirEntry]
    dirs.put(RootEntry, root)
    val manifest = new Manifest(input)
    closeables ::= input

    val iter     = manifest.getEntries().keySet().iterator().asScala.filter(_.endsWith(".class")).map(new ZipEntry(_))

    for (zipEntry <- iter) {
      val dir = getDir(dirs, zipEntry)
      if (!zipEntry.isDirectory) {
        class FileEntry() extends Entry(zipEntry.getName) {
          override def lastModified = zipEntry.getTime()
          override def input        = resourceInputStream(path)
          override def sizeOption   = None
        }
        val f = new FileEntry()
        dir.entries(f.name) = f
      }
    }

    try root.iterator
    finally dirs.clear()
  }

  def name  = path
  def path: String = {
    val s = url.getPath
    val n = s.lastIndexOf('!')
    s.substring(0, n)
  }
  def input = url.openStream()
  def lastModified =
    try url.openConnection().getLastModified()
    catch { case _: IOException => 0 }

  override def canEqual(other: Any) = other.isInstanceOf[ManifestResources]
  override def hashCode() = url.hashCode
  override def equals(that: Any) = that match {
    case x: ManifestResources => url == x.url
    case _                => false
  }

  private def resourceInputStream(path: String): InputStream = {
    new FilterInputStream(null) {
      override def read(): Int = {
        if(in == null) in = Thread.currentThread().getContextClassLoader().getResourceAsStream(path)
        if(in == null) throw new RuntimeException(path + " not found")
        super.read()
      }

      override def close(): Unit = {
        super.close()
        in = null
      }
    }
  }
  private[this] var closeables: List[java.io.Closeable] = Nil
  override def close(): Unit = {
    closeables.foreach(_.close())
  }
}
