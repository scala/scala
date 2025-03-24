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

import java.net.URL
import util.Exceptional.rootCause
import scala.reflect.internal.util.ScalaClassLoader
import scala.util.control.NonFatal

trait CommonRunner {
  /** Run a given object, specified by name, using a
   *  specified classpath and argument list.
   *
   *  @throws java.lang.ClassNotFoundException
   *  @throws java.lang.NoSuchMethodException
   *  @throws java.lang.reflect.InvocationTargetException
   */
  def run(urls: Seq[URL], objectName: String, arguments: Seq[String]): Unit = {
    import scala.reflect.internal.util.RichClassLoader._
    ScalaClassLoader.fromURLsParallelCapable(urls).run(objectName, arguments)
  }

  /** Catches any non-fatal exception thrown by run (in the case of InvocationTargetException,
   *  unwrapping it) and returns it in an Option.
   */
  def runAndCatch(urls: Seq[URL], objectName: String, arguments: Seq[String]): Option[Throwable] =
    try   { run(urls, objectName, arguments) ; None }
    catch { case NonFatal(e) => Some(rootCause(e)) }
}

/** An object that runs another object specified by name.
 *
 *  @author  Lex Spoon
 */
object ObjectRunner extends CommonRunner
