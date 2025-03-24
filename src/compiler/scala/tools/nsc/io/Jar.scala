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

package scala.tools.nsc
package io

import java.io.{DataOutputStream, InputStream, OutputStream}
import java.util.jar._

import scala.jdk.CollectionConverters._
import Attributes.Name

import scala.annotation.tailrec
import scala.collection.AbstractIterable

// Attributes.Name instances:
//
// static Attributes.Name   CLASS_PATH
// static Attributes.Name   CONTENT_TYPE
// static Attributes.Name   EXTENSION_INSTALLATION
// static Attributes.Name   EXTENSION_LIST
// static Attributes.Name   EXTENSION_NAME
// static Attributes.Name   IMPLEMENTATION_TITLE
// static Attributes.Name   IMPLEMENTATION_URL
// static Attributes.Name   IMPLEMENTATION_VENDOR
// static Attributes.Name   IMPLEMENTATION_VENDOR_ID
// static Attributes.Name   IMPLEMENTATION_VERSION
// static Attributes.Name   MAIN_CLASS
// static Attributes.Name   MANIFEST_VERSION
// static Attributes.Name   SEALED
// static Attributes.Name   SIGNATURE_VERSION
// static Attributes.Name   SPECIFICATION_TITLE
// static Attributes.Name   SPECIFICATION_VENDOR
// static Attributes.Name   SPECIFICATION_VERSION

class Jar(file: File) extends AbstractIterable[JarEntry] {
  def this(jfile: JFile) = this(File(jfile))
  def this(path: String) = this(File(path))

  lazy val manifest = withJarInput(s => Option(s.getManifest))

  def mainClass     = manifest map (f => f(Name.MAIN_CLASS))
  /** The manifest-defined classpath String if available. */
  def classPathString: Option[String] =
    for (m <- manifest ; cp <- m.attrs.get(Name.CLASS_PATH)) yield cp
  def classPathElements: List[String] = classPathString match {
    case Some(s)  => s.split("\\s+").toList
    case _        => Nil
  }

  /** Invoke f with input for named jar entry (or None). */
  def withEntryStream[A](name: String)(f: Option[InputStream] => A) = {
    val jarFile = new JarFile(file.jfile)
    def apply() =
      jarFile getEntry name match {
        case null   => f(None)
        case entry  =>
          val in = Some(jarFile.getInputStream(entry))
          try f(in)
          finally in.foreach(_.close())
      }
    try apply() finally jarFile.close()
  }

  def withJarInput[T](f: JarInputStream => T): T = {
    val in = new JarInputStream(file.inputStream())
    try f(in)
    finally in.close()
  }
  def jarWriter(mainAttrs: (Attributes.Name, String)*) = {
    new JarWriter(file, Jar.WManifest(mainAttrs: _*).underlying)
  }

  override def foreach[U](f: JarEntry => U): Unit = withJarInput { in =>
    Iterator continually in.getNextJarEntry() takeWhile (_ != null) foreach f
  }
  override def iterator: Iterator[JarEntry] = this.toList.iterator
  override def isEmpty: Boolean = iterator.isEmpty
  override def toString = "" + file
}

class JarWriter(val file: File, val manifest: Manifest) {
  private lazy val out = new JarOutputStream(file.outputStream(), manifest)

  /** Adds a jar entry for the given path and returns an output
   *  stream to which the data should immediately be written.
   *  This unusual interface exists to work with fjbg.
   */
  def newOutputStream(path: String): DataOutputStream = {
    val entry = new JarEntry(path)
    out putNextEntry entry
    new DataOutputStream(out)
  }

  def writeAllFrom(dir: Directory): Unit = {
    try dir.list foreach (x => addEntry(x, ""))
    finally out.close()
  }
  def addStream(entry: JarEntry, in: InputStream): Unit = {
    out putNextEntry entry
    try transfer(in, out)
    finally out.closeEntry()
  }
  def addFile(file: File, prefix: String): Unit = {
    val entry = new JarEntry(prefix + file.name)
    addStream(entry, file.inputStream())
  }
  def addEntry(entry: Path, prefix: String): Unit = {
    if (entry.isFile) addFile(entry.toFile, prefix)
    else addDirectory(entry.toDirectory, prefix + entry.name + "/")
  }
  def addDirectory(entry: Directory, prefix: String): Unit = {
    entry.list foreach (p => addEntry(p, prefix))
  }

  private def transfer(in: InputStream, out: OutputStream) = {
    val buf = new Array[Byte](10240)
    @tailrec
    def loop(): Unit = in.read(buf, 0, buf.length) match {
      case -1 => in.close()
      case n  => out.write(buf, 0, n) ; loop()
    }
    loop()
  }

  def close() = out.close()
}

object Jar {
  type AttributeMap = java.util.Map[Attributes.Name, String]

  object WManifest {
    def apply(mainAttrs: (Attributes.Name, String)*): WManifest = {
      val m = WManifest(new JManifest)
      for ((k, v) <- mainAttrs)
        m(k) = v

      m
    }
    def apply(manifest: JManifest): WManifest = new WManifest(manifest)
  }
  class WManifest(manifest: JManifest) {
    for ((k, v) <- initialMainAttrs)
      this(k) = v

    def underlying = manifest
    def attrs = manifest.getMainAttributes().asInstanceOf[AttributeMap].asScala withDefaultValue null
    def initialMainAttrs: Map[Attributes.Name, String] = {
      import scala.util.Properties._
      Map(
        Name.MANIFEST_VERSION -> "1.0",
        ScalaCompilerVersion  -> versionNumberString
      )
    }

    def apply(name: Attributes.Name): String        = attrs(name)
    def update(key: Attributes.Name, value: String) = attrs.put(key, value)
  }

  // See https://docs.oracle.com/javase/8/docs/api/java/nio/file/Path.html
  // for some ideas.
  private val ZipMagicNumber = List[Byte](80, 75, 3, 4)
  private def magicNumberIsZip(f: Path) = f.toFile.bytes().take(4).toList == ZipMagicNumber

  // file exists and either has name.jar or magic number
  def isJarOrZip(f: Path): Boolean = f.isFile && (Path.isExtensionJarOrZip(f.name) || magicNumberIsZip(f))

  def create(file: File, sourceDir: Directory, mainClass: String): Unit = {
    val writer = new Jar(file).jarWriter(Name.MAIN_CLASS -> mainClass)
    writer writeAllFrom sourceDir
  }
}
