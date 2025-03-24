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

package scala
package reflect
package io

import java.nio.ByteBuffer
import java.nio.file.StandardOpenOption
import java.util

/** ''Note:  This library is considered experimental and should not be used unless you know what you are doing.'' */
class PlainDirectory(givenPath: Directory) extends PlainFile(givenPath) {
  override def isDirectory = true
  override def iterator = givenPath.list filter (_.exists) map (x => new PlainFile(x))
  override def delete(): Unit = givenPath.deleteRecursively()
}

/** This class implements an abstract file backed by a File.
 *
 * ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
class PlainFile(val givenPath: Path) extends AbstractFile {
  assert(path ne null)

  val file = givenPath.jfile

  override lazy val canonicalPath = super.canonicalPath

  override def underlyingSource = Some(this)

  private val fpath = givenPath.toAbsolute

  /** Returns the name of this abstract file. */
  def name = givenPath.name

  /** Returns the path of this abstract file. */
  def path = givenPath.path

  /** The absolute file. */
  def absolute = new PlainFile(givenPath.toAbsolute)

  override def container: AbstractFile = new PlainFile(givenPath.parent)
  override def input = givenPath.toFile.inputStream()
  override def output = givenPath.toFile.outputStream()
  override def sizeOption = Some(givenPath.length.toInt)
  override def toByteBuffer: ByteBuffer = {
    val chan = java.nio.file.Files.newByteChannel(file.toPath, util.EnumSet.of(StandardOpenOption.READ))
    try {
      import java.nio.ByteBuffer
      val buffer: ByteBuffer = ByteBuffer.allocate(chan.size.toInt)
      var endOfInput = false
      while (!endOfInput ) {
        endOfInput = chan.read(buffer) < 0
        buffer.compact()
      }
      buffer.flip()
      buffer
    } finally chan.close()
  }

  override def hashCode(): Int = fpath.hashCode()
  override def equals(that: Any): Boolean = that match {
    case x: PlainFile => fpath == x.fpath
    case _            => false
  }

  /** Is this abstract file a directory? */
  def isDirectory: Boolean = givenPath.isDirectory

  /** Returns the time that this abstract file was last modified. */
  def lastModified: Long = givenPath.lastModified

  /** Returns all abstract subfiles of this abstract directory. */
  def iterator: Iterator[AbstractFile] = {
    // Optimization: Assume that the file was not deleted and did not have permissions changed
    // between the call to `list` and the iteration. This saves a call to `exists`.
    def existsFast(path: Path) = path match {
      case (_: Directory | _: io.File) => true
      case _                           => path.exists
    }
    if (!isDirectory) Iterator.empty
    else givenPath.toDirectory.list filter existsFast map (new PlainFile(_))
  }

  /**
   * Returns the abstract file in this abstract directory with the
   * specified name. If there is no such file, returns null. The
   * argument "directory" tells whether to look for a directory or
   * or a regular file.
   */
  def lookupName(name: String, directory: Boolean): AbstractFile = {
    val child = givenPath / name
    if ((child.isDirectory && directory) || (child.isFile && !directory)) new PlainFile(child)
    else null
  }

  /** Does this abstract file denote an existing file? */
  def create(): Unit = if (!exists) givenPath.createFile()

  /** Delete the underlying file or directory (recursively). */
  def delete(): Unit =
    if (givenPath.isFile) givenPath.delete()
    else if (givenPath.isDirectory) givenPath.toDirectory.deleteRecursively()

  /** Returns a plain file with the given name. It does not
   *  check that it exists.
   */
  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile =
    new PlainFile(givenPath / name)
}

final class PlainNioFile(val nioPath: java.nio.file.Path) extends AbstractFile {
  import java.nio.file._

  assert(nioPath ne null)

  /** Returns the underlying File if any and null otherwise. */
  override def file: java.io.File = try {
    nioPath.toFile
  } catch {
    case _: UnsupportedOperationException => null
  }

  override lazy val canonicalPath = super.canonicalPath

  override def underlyingSource  = {
    val fileSystem = nioPath.getFileSystem
    fileSystem.provider().getScheme match {
      case "jar" =>
        val fileStores = fileSystem.getFileStores.iterator()
        if (fileStores.hasNext) {
          val jarPath = fileStores.next().name
          try {
            Some(new PlainNioFile(Paths.get(jarPath.stripSuffix(fileSystem.getSeparator))))
          } catch {
            case _: InvalidPathException =>
              None
          }
        } else None
      case "jrt" =>
        if (nioPath.getNameCount > 2 && nioPath.startsWith("/modules")) {
          // TODO limit this to OpenJDK based JVMs?
          val moduleName = nioPath.getName(1)
          Some(new PlainNioFile(Paths.get(System.getProperty("java.home"), "jmods", moduleName.toString + ".jmod")))
        } else None
      case _ => None
    }
  }

  private val fpath = nioPath.toAbsolutePath.toString

  /** Returns the name of this abstract file. */
  def name = nioPath.getFileName.toString

  /** Returns the path of this abstract file. */
  def path = nioPath.toString

  /** The absolute file. */
  def absolute = new PlainNioFile(nioPath.toAbsolutePath)

  override def container: AbstractFile = new PlainNioFile(nioPath.getParent)
  override def input = Files.newInputStream(nioPath)
  override def output = Files.newOutputStream(nioPath)
  override def sizeOption = Some(Files.size(nioPath).toInt)
  override def hashCode(): Int = fpath.hashCode()
  override def equals(that: Any): Boolean = that match {
    case x: PlainNioFile => fpath == x.fpath
    case _               => false
  }

  /** Is this abstract file a directory? */
  def isDirectory: Boolean = Files.isDirectory(nioPath)

  /** Returns the time that this abstract file was last modified. */
  def lastModified: Long = Files.getLastModifiedTime(nioPath).toMillis

  /** Returns all abstract subfiles of this abstract directory. */
  def iterator: Iterator[AbstractFile] = {
    try {
      import scala.jdk.CollectionConverters._
      val it = Files.newDirectoryStream(nioPath).iterator
      it.asScala.map(new PlainNioFile(_))
    } catch {
      case _: NotDirectoryException => Iterator.empty
    }
  }

  /**
    * Returns the abstract file in this abstract directory with the
    * specified name. If there is no such file, returns null. The
    * argument "directory" tells whether to look for a directory or
    * or a regular file.
    */
  def lookupName(name: String, directory: Boolean): AbstractFile = {
    val child = nioPath.resolve(name)
    if ((Files.isDirectory(child) && directory) || (Files.isRegularFile(child) && !directory)) new PlainNioFile(child)
    else null
  }

  /** Does this abstract file denote an existing file? */
  def create(): Unit = if (!exists)  Files.createFile(nioPath)

  /** Delete the underlying file or directory (recursively). */
  def delete(): Unit =
    if (Files.isRegularFile(nioPath)) Files.deleteIfExists(nioPath)
    else if (Files.isDirectory(nioPath)) new Directory(nioPath.toFile).deleteRecursively()

  /** Returns a plain file with the given name. It does not
    *  check that it exists.
    */
  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile =
    new PlainNioFile(nioPath.resolve(name))
}
