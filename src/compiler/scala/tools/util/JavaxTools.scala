/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools
package util

import java.lang.{ ClassLoader => JavaClassLoader, Iterable => JIterable }
import java.util.{ Iterator => JIterator, Locale, Set => JSet }
import javax.tools.{ FileObject, JavaFileManager, JavaFileObject }
import javax.tools.{ Diagnostic, DiagnosticCollector, DiagnosticListener }

/** Degenerate implementation, by analogy to [[javax.tools.SimpleJavaFileObject]]. */
abstract class SimpleJavaFileManager extends JavaFileManager {
  import JavaFileObject.Kind
  import JavaFileManager.Location

  @inline private final def ??! = throw new UnsupportedOperationException

  /** Does nothing. */
  override def close() { }
  /** Does nothing. */
  override def flush() { }
  /** Unsupported. */
  override def getClassLoader(location: Location): JavaClassLoader = ??!
  /** Unsupported. */
  override def getFileForInput(location: Location, packageName: String, relativeName: String): FileObject = ??!
  /** Unsupported. */
  override def getFileForOutput(location: Location, packageName: String, relativeName: String, sibling: FileObject): FileObject = ??!
  /** Unsupported. */
  override def getJavaFileForInput(location: Location, className: String, kind: Kind): JavaFileObject = ??!
  /** Unsupported. */
  override def getJavaFileForOutput(location: Location, className: String, kind: Kind, sibling: FileObject): JavaFileObject = ??!
  /** False. */
  override def handleOption(current: String, remaining: JIterator[String]): Boolean = false
  /** False. */
  override def hasLocation(location: Location): Boolean = false
      /*
      location match {
        case s: StandardLocation =>
          s match {
            case ANNOTATION_PROCESSOR_PATH => false
            case CLASS_OUTPUT => false
            case CLASS_PATH => false
            case PLATFORM_CLASS_PATH => false
            case SOURCE_OUTPUT => false
            case SOURCE_PATH => false
          }
        case _ => false
      }
      */
  /** Unsupported. */
  override def inferBinaryName(location: Location, file: JavaFileObject): String = ??!
  /** Unsupported. */
  override def isSameFile(a: FileObject, b: FileObject): Boolean = ??!
  /** Unsupported. */
  override def list(location: Location, packageName: String, kinds: JSet[Kind], recurse: Boolean): JIterable[JavaFileObject] = ??!
  final val NotSupported = -1
  /** Always -1 (option not supported). */
  override def isSupportedOption(option: String): Int = NotSupported
}

/** A rich [[javax.tools.DiagnosticCollector]]. */
class JavaReporter extends DiagnosticListener[JavaFileObject] {
  import Diagnostic.Kind.ERROR
  import scala.collection.JavaConverters.iterableAsScalaIterableConverter
  val collector = new DiagnosticCollector[JavaFileObject]
  override def report(diagnostic: Diagnostic[_ <: JavaFileObject]) { collector report diagnostic }
  /** All diagnostics in the collector. */
  def diagnostics: Iterable[Diagnostic[_ <: JavaFileObject]] = collector.getDiagnostics.asScala
  /** Locale for diagnostic messages, null by default. */
  def locale: Locale = null
  /** All diagnostic messages. */
  def messages = (diagnostics map (_ getMessage locale)).toList
  /** Count the errors. */
  def errorCount: Int = (0 /: diagnostics)((sum, d) => sum + (if (d.getKind == ERROR) 1 else 0))
  /** Error diagnostics in the collector. */
  def errors = (diagnostics filter (_.getKind == ERROR)).toList
}
