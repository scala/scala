/* NSC -- new Scala compiler
 * Copyright 2017 LAMP/EPFL
 * @author  Martin Odersky
 */
package scala.reflect.internal.util

import java.io.{File, FileOutputStream, IOException}


trait OwnerOnlyChmod {
  /** Remove group/other permissions for `file`, it if exists */
  def chmod(file: java.io.File): Unit

  /** Delete `file` if it exists, recreate it with no group/other permissions, and write `contents` */
  final def chmodAndWrite(file: File, contents: Array[Byte]): Unit = {
    file.delete()
    val fos = new FileOutputStream(file)
    fos.close()
    chmod(file)
    val fos2 = new FileOutputStream(file)
    try {
      fos2.write(contents)
    } finally {
      fos2.close()
    }
  }

  // TODO: use appropriate NIO call instead of two-step exists?/create!
  final def chmodOrCreateEmpty(file: File): Unit =
    if (!file.exists()) chmodAndWrite(file, Array[Byte]()) else chmod(file)

}

object OwnerOnlyChmod {
  def apply(): OwnerOnlyChmod = {
    if (!util.Properties.isWin) Java6UnixChmod
    else if (util.Properties.isJavaAtLeast("7")) new NioAclChmodReflective
    else NoOpOwnerOnlyChmod
  }
}

object NoOpOwnerOnlyChmod extends OwnerOnlyChmod {
  override def chmod(file: File): Unit = ()
}


/** Adjust permissions with `File.{setReadable, setWritable}` */
object Java6UnixChmod extends OwnerOnlyChmod {

  def chmod(file: File): Unit = if (file.exists()) {
    def clearAndSetOwnerOnly(f: (Boolean, Boolean) => Boolean): Unit = {
      def fail() = throw new IOException("Unable to modify permissions of " + file)
      // attribute = false, ownerOnly = false
      if (!f(false, false)) fail()
      // attribute = true, ownerOnly = true
      if (!f(true, true)) fail()
    }
    if (file.isDirectory) {
      clearAndSetOwnerOnly(file.setExecutable)
    }
    clearAndSetOwnerOnly(file.setReadable)
    clearAndSetOwnerOnly(file.setWritable)
  }
}


object NioAclChmodReflective {
  val file_toPath = classOf[java.io.File].getMethod("toPath")
  val files = Class.forName("java.nio.file.Files")
  val path_class = Class.forName("java.nio.file.Path")
  val getFileAttributeView = files.getMethod("getFileAttributeView", path_class, classOf[Class[_]], Class.forName("[Ljava.nio.file.LinkOption;"))
  val linkOptionEmptyArray = java.lang.reflect.Array.newInstance(Class.forName("java.nio.file.LinkOption"), 0)
  val aclFileAttributeView_class = Class.forName("java.nio.file.attribute.AclFileAttributeView")
  val aclEntry_class = Class.forName("java.nio.file.attribute.AclEntry")
  val aclEntryBuilder_class = Class.forName("java.nio.file.attribute.AclEntry$Builder")
  val newBuilder = aclEntry_class.getMethod("newBuilder")
  val aclEntryBuilder_build = aclEntryBuilder_class.getMethod("build")
  val userPrinciple_class = Class.forName("java.nio.file.attribute.UserPrincipal")
  val setPrincipal = aclEntryBuilder_class.getMethod("setPrincipal", userPrinciple_class)
  val setPermissions = aclEntryBuilder_class.getMethod("setPermissions", Class.forName("[Ljava.nio.file.attribute.AclEntryPermission;"))
  val aclEntryType_class = Class.forName("java.nio.file.attribute.AclEntryType")
  val setType = aclEntryBuilder_class.getMethod("setType", aclEntryType_class)
  val aclEntryPermission_class = Class.forName("java.nio.file.attribute.AclEntryPermission")
  val aclEntryPermissionValues = aclEntryPermission_class.getDeclaredMethod("values")
  val aclEntryType_ALLOW = aclEntryType_class.getDeclaredField("ALLOW")
}

/** Reflective version of `NioAclChmod` */
final class NioAclChmodReflective extends OwnerOnlyChmod {
  import NioAclChmodReflective._
  def chmod(file: java.io.File): Unit = {
    val path = file_toPath.invoke(file)
    val view = getFileAttributeView.invoke(null, path, aclFileAttributeView_class, linkOptionEmptyArray)
    val setAcl = aclFileAttributeView_class.getMethod("setAcl", classOf[java.util.List[_]])
    val getOwner = aclFileAttributeView_class.getMethod("getOwner")
    val owner = getOwner.invoke(view)
    setAcl.invoke(view, acls(owner))
  }

  private def acls(owner: Object) = {
    val builder = newBuilder.invoke(null)
    setPrincipal.invoke(builder, owner)
    setPermissions.invoke(builder, aclEntryPermissionValues.invoke(null))
    setType.invoke(builder, aclEntryType_ALLOW.get(null))
    java.util.Collections.singletonList(aclEntryBuilder_build.invoke(builder))
  }
}
