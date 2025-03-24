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

package scala.tools.nsc.interpreter
package shell

import java.io.CharArrayWriter
import java.net.URI
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path
import java.util.Locale
import java.util.concurrent.ConcurrentLinkedQueue
import javax.tools._, JavaFileManager.Location, StandardLocation._, JavaFileObject.Kind, Kind._
import scala.collection.mutable.Clearable
import scala.jdk.CollectionConverters._
import scala.reflect.io.AbstractFile
import scala.util.chaining._

import System.lineSeparator

class JavacTool private (tool: JavaCompiler, dir: AbstractFile, loader: ClassLoader) {
  private val out = new CharArrayWriter
  def written = {
    out.flush()
    val w = out.toString
    out.reset()
    w
  }
  val listener = new JavaReporter
  val locale = Locale.getDefault
  val fileManager = new JavaToolFileManager(dir, loader)(tool.getStandardFileManager(listener, locale, UTF_8))

  def compile(label: String, code: String): Option[String] = {
    val options = (
      "-encoding" ::
      UTF_8.name() ::
      Nil
    ).asJava
    val classes: java.lang.Iterable[String] = null
    val units = List(StringFileObject(label, code)).asJava
    val task = tool.getTask(out, fileManager, listener, options, classes, units)
    val success = task.call()
    if (success) None else Some(listener.reported(locale))
  }
}
object JavacTool {
  def apply(dir: AbstractFile, loader: ClassLoader): JavacTool = new JavacTool(ToolProvider.getSystemJavaCompiler, dir, loader)
  def apply(dir: Path, loader: ClassLoader)        : JavacTool = apply(AbstractFile.getURL(dir.toUri().toURL()), loader)
}

// use `dir` for output, `loader` for inputs
class JavaToolFileManager(dir: AbstractFile, loader: ClassLoader)(delegate: JavaFileManager) extends ForwardingJavaFileManager[JavaFileManager](delegate) {
  override def getJavaFileForOutput(location: Location, className: String, kind: Kind, sibling: FileObject): JavaFileObject = {
    require(location == CLASS_OUTPUT, s"$location is not CLASS_OUTPUT")
    require(kind == CLASS, s"$kind is not CLASS")
    AbstractFileObject(dir, className, kind)
  }
}

class AbstractFileObject(file: AbstractFile, uri0: URI, kind0: Kind) extends SimpleJavaFileObject(uri0, kind0) {
  override def delete()           = { file.delete() ; true }
  override def openInputStream()  = file.input
  override def openOutputStream() = file.output
}
object AbstractFileObject {
  def apply(dir: AbstractFile, path: String, kind: Kind) = {
    val segments = path.replace(".", "/").split("/")
    val parts    = segments.init
    val name     = segments.last
    val subdir   = parts.foldLeft(dir)((vd, n) => vd.subdirectoryNamed(n))
    val file     = subdir.fileNamed(s"${name}${kind.extension}")
    val uri      = file.file.toURI
    new AbstractFileObject(file, uri, kind)
  }
}

// name is the URI path
//
class StringFileObject(uri0: URI, code: String) extends SimpleJavaFileObject(uri0, SOURCE) {
  override def getCharContent(ignoreEncodingErrors: Boolean) = code
}
object StringFileObject {
  def apply(label: String, code: String): StringFileObject =
    new StringFileObject(URI.create(s"string:///${label.replace('.','/')}${SOURCE.extension}"), code)
}

// A clearable diagnostic collector.
//
class JavaReporter extends DiagnosticListener[JavaFileObject] with Clearable {
  type D = Diagnostic[_ <: JavaFileObject]
  val diagnostics = new ConcurrentLinkedQueue[D]
  private def messagesIterator(implicit locale: Locale) = diagnostics.iterator.asScala.map(_.getMessage(locale))
  override def report(d: Diagnostic[_ <: JavaFileObject]) = diagnostics.add(d)
  override def clear() = diagnostics.clear()
  /** All diagnostic messages.
   *  @param locale Locale for diagnostic messages, null by default.
   */
  def messages(implicit locale: Locale = null) = messagesIterator.toList

  def reported(implicit locale: Locale = null): String = 
    if (diagnostics.isEmpty) ""
    else
      messages
        .mkString("", lineSeparator, lineSeparator)
        .tap(_ => clear())
}
