/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc

import language.implicitConversions

/** The main REPL related classes and values are as follows.
 *  In addition to standard compiler classes Global and Settings, there are:
 *
 *  History: an interface for session history.
 *  Completion: an interface for tab completion.
 *  ILoop (formerly InterpreterLoop): The umbrella class for a session.
 *  IMain (formerly Interpreter): Handles the evolving state of the session
 *    and handles submitting code to the compiler and handling the output.
 *  InteractiveReader: how ILoop obtains input.
 *  History: an interface for session history.
 *  Completion: an interface for tab completion.
 *  Power: a repository for more advanced/experimental features.
 *
 *  ILoop contains { in: InteractiveReader, intp: IMain, settings: Settings, power: Power }
 *  InteractiveReader contains { history: History, completion: Completion }
 *  IMain contains { global: Global }
 */
package object interpreter extends ReplConfig with ReplStrings {
  type JFile          = java.io.File
  type JClass         = java.lang.Class[_]
  type JList[T]       = java.util.List[T]
  type JCollection[T] = java.util.Collection[T]
  type JPrintWriter   = java.io.PrintWriter
  type InputStream    = java.io.InputStream
  type OutputStream   = java.io.OutputStream

  val IR = Results

  implicit def postfixOps = language.postfixOps // make all postfix ops in this package compile without warning

  private[interpreter] implicit def javaCharSeqCollectionToScala(xs: JCollection[_ <: CharSequence]): List[String] = {
    import collection.JavaConverters._
    xs.asScala.toList map ("" + _)
  }

  private[nsc] implicit def enrichClass[T](clazz: Class[T]) = new RichClass[T](clazz)
  private[nsc] implicit def enrichAnyRefWithTap[T](x: T) = new TapMaker(x)
  private[nsc] def tracing[T](msg: String)(x: T): T = x.tapTrace(msg)
  private[nsc] def debugging[T](msg: String)(x: T) = x.tapDebug(msg)
}
