/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc

import java.net.URL
import util.Exceptional.unwrap
import scala.reflect.internal.util.ScalaClassLoader

trait CommonRunner {
  /** Run a given object, specified by name, using a
   *  specified classpath and argument list.
   *
   *  @throws ClassNotFoundException
   *  @throws NoSuchMethodException
   *  @throws InvocationTargetException
   */
  def run(urls: Seq[URL], objectName: String, arguments: Seq[String]) {
    import scala.reflect.internal.util.RichClassLoader._
    ScalaClassLoader.fromURLsParallelCapable(urls).run(objectName, arguments)
  }

  /** Catches exceptions enumerated by run (in the case of InvocationTargetException,
   *  unwrapping it) and returns it any thrown in Left(x).
   */
  def runAndCatch(urls: Seq[URL], objectName: String, arguments: Seq[String]): Either[Throwable, Boolean] = {
    try   { run(urls, objectName, arguments) ; Right(true) }
    catch { case e: Throwable => Left(unwrap(e)) }
  }
}

/** An object that runs another object specified by name.
 *
 *  @author  Lex Spoon
 *  @version 1.1, 2007/7/13
 */
object ObjectRunner extends CommonRunner { }
