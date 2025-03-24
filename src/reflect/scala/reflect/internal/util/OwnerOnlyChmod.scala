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

package scala.reflect.internal.util

import java.nio.ByteBuffer
import java.nio.file.StandardOpenOption.{CREATE, TRUNCATE_EXISTING, WRITE}
import java.nio.file.attribute.PosixFilePermission.{OWNER_EXECUTE, OWNER_READ, OWNER_WRITE}
import java.nio.file.attribute._
import java.nio.file.{Files, Path}
import java.util.EnumSet


object OwnerOnlyChmod {
  // @requires Files.exists(path)
  private def canPosix(path: Path) =
    Files.getFileStore(path).supportsFileAttributeView(classOf[PosixFileAttributeView])

  private[this] val posixDir  = EnumSet.of(OWNER_READ, OWNER_WRITE, OWNER_EXECUTE)
  private[this] val posixFile = EnumSet.of(OWNER_READ, OWNER_WRITE)

  /** Remove group/other permissions for `file`, it if exists, and if the runtime environment supports modifying permissions. */
  def chmod(path: Path): Unit = {
    if (canPosix(path)) Files.setPosixFilePermissions(path, if (Files.isDirectory(path)) posixDir else posixFile)
    else {
      // if getting this view fails, we fail
      val view = Files.getFileAttributeView(path, classOf[AclFileAttributeView])
      if (view == null) throw new UnsupportedOperationException(s"Cannot get file attribute view for $path")

      val acls = {
        val builder = AclEntry.newBuilder
        builder.setPrincipal(view.getOwner())
        builder.setPermissions(AclEntryPermission.values(): _*)
        builder.setType(AclEntryType.ALLOW)
        val entry = builder.build
        java.util.Collections.singletonList(entry)
      }

      view.setAcl(acls)
    }
  }

  def chmodFileOrCreateEmpty(path: Path): Unit = {
    Files.newByteChannel(path, EnumSet.of(WRITE, CREATE)).close() // make sure it exists
    chmod(path)
  }

  def chmodFileAndWrite(path: Path, contents: Array[Byte]): Unit = {
    val sbc = Files.newByteChannel(path, EnumSet.of(WRITE, CREATE, TRUNCATE_EXISTING))
    try sbc.write(ByteBuffer.wrap(contents)) finally sbc.close()
    chmod(path)
  }
}

