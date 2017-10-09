/* NSC -- new Scala compiler
 * Copyright 2017 LAMP/EPFL
 * @author  Martin Odersky
 */
package scala.reflect.internal.util

import java.nio.ByteBuffer
import java.nio.file.StandardOpenOption.{CREATE, TRUNCATE_EXISTING, WRITE}
import java.nio.file.attribute.PosixFilePermission.{OWNER_EXECUTE, OWNER_READ, OWNER_WRITE}
import java.nio.file.attribute.PosixFilePermissions.asFileAttribute
import java.nio.file.attribute._
import java.nio.file.{Files, Path}
import java.util.EnumSet


object OwnerOnlyChmod {
  private def canPosix(path: Path) =
    Files.getFileStore(path).supportsFileAttributeView(classOf[PosixFileAttributeView])

  private val posixDir  = EnumSet.of(OWNER_READ, OWNER_WRITE, OWNER_EXECUTE)
  private val posixFile = EnumSet.of(OWNER_READ, OWNER_WRITE)
  private def fileAttributes(path: Path) =
    if (canPosix(path)) Array(asFileAttribute(posixFile)) else Array.empty[FileAttribute[_]]

  /** Remove group/other permissions for `file`, it if exists, and if the runtime environment supports modifying permissions. */
  def chmod(path: Path): Unit = {
    if (canPosix(path)) Files.setPosixFilePermissions(path, if (Files.isDirectory(path)) posixDir else posixFile)
    else {
      // if getting this view fails, we fail
      val view = Files.getFileAttributeView(path, classOf[AclFileAttributeView])
      if (view == null) throw new UnsupportedOperationException(s"Cannot get file attribute view for $path")

      val acls = {
        val builder = AclEntry.newBuilder
        builder.setPrincipal(view.getOwner)
        builder.setPermissions(AclEntryPermission.values(): _*)
        builder.setType(AclEntryType.ALLOW)
        val entry = builder.build
        java.util.Collections.singletonList(entry)
      }

      view.setAcl(acls)
    }
  }

  def chmodFileOrCreateEmpty(path: Path): Unit = {
    // Create new file if none existed, with appropriate permissions via the fileAttributes attributes (if supported).
    Files.newByteChannel(path, EnumSet.of(WRITE, CREATE), fileAttributes(path): _*).close()
    // Change (if needed -- either because the file already existed, or the FS needs a separate call to set the ACL)
    chmod(path)
  }

  def chmodFileAndWrite(path: Path, contents: Array[Byte]): Unit = {
    val sbc = Files.newByteChannel(path, EnumSet.of(WRITE, TRUNCATE_EXISTING), fileAttributes(path): _*)
    try sbc.write(ByteBuffer.wrap(contents)) finally sbc.close()
    chmod(path) // for acl-based FS
  }
}

