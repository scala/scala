/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package util

import io.{ Fileish, Sources }
import Exceptional._

class FrameContext(frame: JavaStackFrame, codeSources: Sources) {
  val sourceFile = locateSource(codeSources, frame)
  import frame._

  def windowWidth = 3
  def windowSize = windowWidth * 2 + 1

  lazy val context = sourceFile collect {
    case f if line > 0 =>
      val start = math.max(0, line - windowWidth)
      f.lines().toList.slice(start, start + windowSize)
  } getOrElse Nil

  protected def fallbackContext = "%s (%s:%s)".format(tag, fileName, line)

  private def linestr(index: Int) = {
    val current = line - windowWidth + index
    val marker  = if (current == line) "*" else " "
    marker + current
  }
  private def contextLines = context.zipWithIndex map {
    case (l, i) => linestr(i) + ": " + l + "\n"
  }
  override def toString =
    if (context.isEmpty) fallbackContext
    else contextLines.mkString(tag + "\n", "", "")
}

object FrameContext {
  def apply(elem: StackTraceElement): FrameContext = apply(new JavaStackFrame(elem))
  def apply(frame: JavaStackFrame): FrameContext = new FrameContext(frame, Sources())
}

class JavaStackFrame(val elem: StackTraceElement) {
  def className: String  = elem.getClassName()
  def methodName: String = elem.getMethodName()
  def fileName: String   = elem.getFileName()
  def line: Int          = elem.getLineNumber()

  private def segs = className takeWhile (ch => ch != '$' && ch != '(') split '.' toList ;
  lazy val pkgName      = segs.init mkString "."
  lazy val shortName    = segs.last
  lazy val shortestName = if (fileName startsWith (shortName + ".")) "<--" else shortName

  private def standardString(which: String) =
    "%s.%s(%s:%s)".format(which, methodName, fileName, line)

  def locationString  = fileName + ":" + line
  def javaString      = standardString(className)
  def shortNameString = standardString(shortName)
  def tag             = "[%s.%s]".format(shortName, methodName)

  override def toString = shortNameString
}

object JavaStackFrame {
  def apply(elem: StackTraceElement) = new JavaStackFrame(elem)
  def frames(xs: Array[StackTraceElement]): Array[JavaStackFrame] = xs map (x => new JavaStackFrame(x))
  def frames(t: Throwable): Array[JavaStackFrame]                 = frames(Exceptional.unwrap(t).getStackTrace)
}
