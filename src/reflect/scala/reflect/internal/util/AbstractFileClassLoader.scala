/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 */

package scala
package reflect.internal.util

import scala.collection.mutable
import scala.reflect.io.AbstractFile
import java.net.{ URL, URLConnection, URLStreamHandler }
import java.security.cert.Certificate
import java.security.{ ProtectionDomain, CodeSource }
import java.util.{ Collections => JCollections, Enumeration => JEnumeration }

/** A class loader that loads files from a [[scala.reflect.io.AbstractFile]].
 *
 *  @author Lex Spoon
 */
class AbstractFileClassLoader(val root: AbstractFile, parent: ClassLoader)
    extends ClassLoader(parent)
    with ScalaClassLoader
{
  protected def classNameToPath(name: String): String =
    if (name endsWith ".class") name
    else s"${name.replace('.', '/')}.class"

  protected def findAbstractFile(name: String): AbstractFile = {
    var file: AbstractFile = root
    val pathParts          = name split '/'

    for (dirPart <- pathParts.init) {
      file = file.lookupName(dirPart, directory = true)
      if (file == null)
        return null
    }

    file.lookupName(pathParts.last, directory = false) match {
      case null   => null
      case file   => file
    }
  }

  protected def dirNameToPath(name: String): String =
    name.replace('.', '/')

  protected def findAbstractDir(name: String): AbstractFile = {
    var file: AbstractFile = root
    val pathParts          = dirNameToPath(name) split '/'

    for (dirPart <- pathParts) {
      file = file.lookupName(dirPart, directory = true)
      if (file == null)
        return null
    }

    file
  }

  override protected def findClass(name: String): Class[_] = {
    val bytes = classBytes(name)
    if (bytes.length == 0)
      throw new ClassNotFoundException(name)
    else
      defineClass(name, bytes, 0, bytes.length, protectionDomain)
  }
  override protected def findResource(name: String): URL = findAbstractFile(name) match {
    case null => null
    case file => new URL(null, s"memory:${file.path}", new URLStreamHandler {
      override def openConnection(url: URL): URLConnection = new URLConnection(url) {
        override def connect() = ()
        override def getInputStream = file.input
      }
    })
  }
  override protected def findResources(name: String): JEnumeration[URL] = findResource(name) match {
    case null => JCollections.enumeration(JCollections.emptyList[URL])  //JCollections.emptyEnumeration[URL]
    case url  => JCollections.enumeration(JCollections.singleton(url))
  }

  lazy val protectionDomain = {
    val cl = Thread.currentThread().getContextClassLoader()
    val resource = cl.getResource("scala/runtime/package.class")
    if (resource == null || resource.getProtocol != "jar") null else {
      val s = resource.getPath
      val n = s.lastIndexOf('!')
      if (n < 0) null else {
        val path = s.substring(0, n)
        new ProtectionDomain(new CodeSource(new URL(path), null.asInstanceOf[Array[Certificate]]), null, this, null)
      }
    }
  }

  private val packages = mutable.Map[String, Package]()

  override def definePackage(name: String, specTitle: String, specVersion: String, specVendor: String, implTitle: String, implVersion: String, implVendor: String, sealBase: URL): Package = {
    throw new UnsupportedOperationException()
  }

  override def getPackage(name: String): Package = findAbstractDir(name) match {
    case null => super.getPackage(name)
    case file => packages.getOrElseUpdate(name, {
      val ctor = classOf[Package].getDeclaredConstructor(classOf[String], classOf[String], classOf[String], classOf[String], classOf[String], classOf[String], classOf[String], classOf[URL], classOf[ClassLoader])
      ctor.setAccessible(true)
      ctor.newInstance(name, null, null, null, null, null, null, null, this)
    })
  }

  override def getPackages(): Array[Package] =
    root.iterator.filter(_.isDirectory).map(dir => getPackage(dir.name)).toArray
}
