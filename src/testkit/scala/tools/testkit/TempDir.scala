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

package scala.tools.testkit

import java.io.{IOException, File}
import java.nio.file.{Files, Path}

import scala.util.Properties

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
  import scala.util.Using.Releasable
  implicit val deleteOnRelease: Releasable[ForDeletion] = new Releasable[ForDeletion] {
    override def release(releasee: ForDeletion) = if (!Properties.isWin) Files.delete(releasee.path)
  }
}

/* Things that MiMa won't let us make Autocloseable.
 */
object Releasables {
  import scala.reflect.io.ZipArchive
  import scala.util.Using.Releasable
  implicit val closeZipOnRelease: Releasable[ZipArchive] = new Releasable[ZipArchive] {
    override def release(releasee: ZipArchive) = releasee.close()
  }
}
