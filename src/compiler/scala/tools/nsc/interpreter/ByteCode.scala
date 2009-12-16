/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import java.io.File
import java.util.jar.{ JarEntry, JarFile }
import java.util.concurrent.ConcurrentHashMap
import util.ScalaClassLoader.getSystemLoader

object ByteCode {
  /** Until I figure out why I can't get scalap onto the classpath such
   *  that the compiler will bootstrap, we have to use reflection.
   */
  private lazy val DECODE: Option[String => Option[Map[String, String]]] =
    for (clazz <- getSystemLoader.tryToLoadClass[AnyRef]("scala.tools.scalap.Decode$")) yield {
      val module = clazz.getField("MODULE$").get()
      val method = clazz.getMethod("typeAliases", classOf[String])
      val map = method.invoke(module, _: String).asInstanceOf[Option[Map[String, String]]]
      map
    }

  def aliasesForPackage(pkg: String) = DECODE flatMap (_(pkg))

  /** Use scalap to look through type aliases */
  def aliasForType(path: String): Option[String] = {
    val (pkg, name) = (path lastIndexOf '.') match {
      case -1   => return None
      case idx  => (path take idx, path drop (idx + 1))
    }
    aliasesForPackage(pkg) flatMap (_ get name)
  }
}
