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

import java.io.{
  FileInputStream, FileOutputStream, BufferedWriter, OutputStreamWriter,
  BufferedOutputStream, IOException, PrintWriter, File => JFile
}

import scala.io.Codec
/**
 * ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
object File {
  def pathSeparator = java.io.File.pathSeparator
  def separator     = java.io.File.separator
  def apply(path: Path)(implicit codec: Codec) = new File(path.jfile)(codec)

  // Create a temporary file, which will be deleted upon jvm exit.
  def makeTemp(prefix: String = Path.randomPrefix, suffix: String = null, dir: JFile = null) = {
    val jfile = java.io.File.createTempFile(prefix, suffix, dir)
    jfile.deleteOnExit()
    apply(jfile)
  }
}

/** An abstraction for files.  For character data, a Codec
 *  can be supplied at either creation time or when a method
 *  involving character data is called (with the latter taking
 *  precedence if supplied.) If neither is available, the value
 *  of scala.io.Codec.default is used.
 *
 *  ''Note:  This is library is considered experimental and should not be used unless you know what you are doing.''
 */
class File(jfile: JFile)(implicit constructorCodec: Codec) extends Path(jfile) with Streamable.Chars {
  override val creationCodec = constructorCodec

  override def addExtension(ext: String): File = super.addExtension(ext).toFile
  override def toAbsolute: File = if (isAbsolute) this else super.toAbsolute.toFile
  override def toDirectory: Directory = new Directory(jfile)
  override def toFile: File = this
  override def normalize: File = super.normalize.toFile
  override def length = super[Path].length
  override def walkFilter(cond: Path => Boolean): Iterator[Path] =
    if (cond(this)) Iterator.single(this) else Iterator.empty

  /** Obtains an InputStream. */
  def inputStream() = new FileInputStream(jfile)

  /** Obtains a OutputStream. */
  def outputStream(append: Boolean = false) = new FileOutputStream(jfile, append)
  def bufferedOutput(append: Boolean = false) = new BufferedOutputStream(outputStream(append))

  /** Obtains an OutputStreamWriter wrapped around a FileOutputStream.
   *  This should behave like a less broken version of java.io.FileWriter,
   *  in that unlike the java version you can specify the encoding.
   */
  def writer(append: Boolean, codec: Codec): OutputStreamWriter =
    new OutputStreamWriter(outputStream(append), codec.charSet)

  /** Wraps a BufferedWriter around the result of writer().
   */
  def bufferedWriter(): BufferedWriter = bufferedWriter(append = false)
  def bufferedWriter(append: Boolean): BufferedWriter = bufferedWriter(append, creationCodec)
  def bufferedWriter(append: Boolean, codec: Codec): BufferedWriter =
    new BufferedWriter(writer(append, codec))

  def printWriter(): PrintWriter = new PrintWriter(bufferedWriter(), true)

  /** Creates a new file and writes all the Strings to it. */
  def writeAll(strings: String*): Unit = {
    val out = bufferedWriter()
    try strings foreach (out write _)
    finally out.close()
  }

  def appendAll(strings: String*): Unit = {
    val out = bufferedWriter(append = true)
    try strings foreach (out write _)
    finally out.close()
  }

  /** Calls println on each string (so it adds a newline in the PrintWriter fashion.) */
  def printlnAll(strings: String*): Unit = {
    val out = printWriter()
    try strings foreach (out println _)
    finally out.close()
  }

  def safeSlurp(): Option[String] =
    try Some(slurp())
    catch { case _: IOException => None }

  /** Ignores SecurityException.
   */
  def setExecutable(executable: Boolean, ownerOnly: Boolean = true): Boolean =
    try jfile.setExecutable(executable, ownerOnly)
    catch { case _: SecurityException => false }
}
