/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Lex Spoon
 */

// $Id$

package scala.tools.nsc

import java.net.URL
import util.ScalaClassLoader

/** An object that runs another object specified by name.
 *
 *  @author  Lex Spoon
 *  @version 1.1, 2007/7/13
 */
object ObjectRunner
{
  /** Check whether a class with the specified name
   *  exists on the specified class path. */
  def classExists(urls: List[URL], objectName: String): Boolean =
    ScalaClassLoader.classExists(urls, objectName)

  /** Run a given object, specified by name, using a
   *  specified classpath and argument list.
   *
   *  @throws ClassNotFoundException
   *  @throws NoSuchMethodError
   *  @throws InvocationTargetException
   */
  def run(urls: List[URL], objectName: String, arguments: Seq[String]) {
    (ScalaClassLoader fromURLs urls).run(objectName, arguments)
  }
}
