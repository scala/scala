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
package reflect.internal.util

import scala.collection.mutable
import scala.collection.immutable.ArraySeq
import scala.reflect.io.AbstractFile
import java.net.{URI, URL, URLConnection, URLStreamHandler}
import java.security.cert.Certificate
import java.security.{CodeSource, ProtectionDomain}
import java.util.{Collections => JCollections, Enumeration => JEnumeration}

import scala.annotation.nowarn

object AbstractFileClassLoader {
  // should be a method on AbstractFile, but adding in `internal.util._` for now as we're in a minor release
  @nowarn("cat=lint-nonlocal-return")
  private[scala] final def lookupPath(base: AbstractFile)(pathParts: Seq[String], directory: Boolean): AbstractFile = {
    var file: AbstractFile = base
    for (dirPart <- pathParts.init) {
      file = file.lookupName(dirPart, directory = true)
      if (file == null)
        return null
    }

    file.lookupName(pathParts.last, directory = directory)
  }
}

/** A class loader that loads files from a [[scala.reflect.io.AbstractFile]].
 */
class AbstractFileClassLoader(val root: AbstractFile, parent: ClassLoader)
    extends ClassLoader(parent)
    with ScalaClassLoader
{
  protected def classNameToPath(name: String): String =
    if (name endsWith ".class") name
    else s"${name.replace('.', '/')}.class"

  protected def findAbstractFile(name: String): AbstractFile = {
    AbstractFileClassLoader.lookupPath(root)(ArraySeq.unsafeWrapArray(name split '/'), directory = false)
  }

  protected def dirNameToPath(name: String): String =
    name.replace('.', '/')

  @nowarn("cat=lint-nonlocal-return")
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

  // on JDK 20 the URL constructor we're using is deprecated, but the recommended
  // replacement, URL.of, doesn't exist on JDK 8
  @annotation.nowarn("cat=deprecation")
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
        new ProtectionDomain(new CodeSource(new URI(path).toURL, null.asInstanceOf[Array[Certificate]]), null, this, null)
      }
    }
  }

  private[this] val packages = mutable.Map[String, Package]()

  override def definePackage(name: String, specTitle: String, specVersion: String, specVendor: String, implTitle: String, implVersion: String, implVendor: String, sealBase: URL): Package = {
    throw new UnsupportedOperationException()
  }

  // TODO: `getPackage` is deprecated in JDK 9+ - what should be overridden instead?
  override def getPackage(name: String): Package = findAbstractDir(name) match {
    case null => super.getPackage(name): @nowarn("cat=deprecation")
    case file => packages.getOrElseUpdate(name, {
      val ctor = classOf[Package].getDeclaredConstructor(classOf[String], classOf[String], classOf[String], classOf[String], classOf[String], classOf[String], classOf[String], classOf[URL], classOf[ClassLoader])
      ctor.setAccessible(true)
      ctor.newInstance(name, null, null, null, null, null, null, null, this)
    })
  }

  override def getPackages(): Array[Package] =
    root.iterator.filter(_.isDirectory).map(dir => getPackage(dir.name)).toArray
}
