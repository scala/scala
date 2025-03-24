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

package scala.tools.testkit

import java.io.{IOException, File}
import java.nio.file.{Files, FileVisitResult, SimpleFileVisitor, Path}, FileVisitResult.{CONTINUE => Continue}
import java.nio.file.attribute._

import scala.util.Properties
import scala.util.Using.Releasable

object TempDir {
  final val TEMP_DIR_ATTEMPTS = 10000
  def createTempDir(): File = {
    val baseDir = new File(System.getProperty("java.io.tmpdir"))
    val baseName = s"${System.currentTimeMillis}-"
    var c = 0
    while (c < TEMP_DIR_ATTEMPTS) {
      val tempDir = new File(baseDir, baseName + c)
      if (tempDir.mkdir()) return tempDir
      c += 1
    }
    throw new IOException(s"Failed to create directory")
  }
}

/* Turn a path into a temp file for purposes of Using it as a resource.
 * On Windows, avoid "file is in use" errors by not attempting to delete it.
 */
case class ForDeletion(path: Path)
object ForDeletion {
  implicit val deleteOnRelease: Releasable[ForDeletion] = new Releasable[ForDeletion] {
    override def release(releasee: ForDeletion) = if (!Properties.isWin) Files.delete(releasee.path)
  }
}
object ReleasablePath {
  // On release of a path, delete the file it represents or recursively remove the directory.
  implicit val deleteOnRelease: Releasable[Path] = new Releasable[Path] {
    override def release(releasee: Path) = if (!Properties.isWin) remove(releasee)
  }
  // Delete a File on relese.
  implicit val deleteOnRelease2: Releasable[File] = new Releasable[File] {
    override def release(releasee: File) = if (!Properties.isWin) releasee.delete()
  }

  private def remove(path: Path): Unit = if (Files.isDirectory(path)) removeRecursively(path) else Files.delete(path)

  private def removeRecursively(path: Path): Unit = Files.walkFileTree(path, new ZappingFileVisitor)

  private class ZappingFileVisitor extends SimpleFileVisitor[Path] {
    private def zap(path: Path) = { Files.delete(path) ; Continue }
    override def postVisitDirectory(path: Path, e: IOException): FileVisitResult = if (e != null) throw e else zap(path)
    override def visitFile(path: Path, attrs: BasicFileAttributes): FileVisitResult = zap(path)
  }
}

/* Things that MiMa won't let us make Autocloseable.
 */
object Releasables {
  import scala.reflect.io.ZipArchive
  implicit val closeZipOnRelease: Releasable[ZipArchive] = new Releasable[ZipArchive] {
    override def release(releasee: ZipArchive) = releasee.close()
  }
}
