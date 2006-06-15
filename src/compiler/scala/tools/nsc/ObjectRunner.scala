/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Lex Spoon
 */

// $Id$

package scala.tools.nsc

import java.io.File
import java.lang.reflect.{Method,Modifier}
import java.net.URLClassLoader

/** An object that runs another object specified by name. */
object ObjectRunner {

  def isMainMethod(meth: Method): Boolean = {
    def paramsOK(params: Array[Class]): Boolean =
      params.length == 1 &&
      params(0) == classOf[Array[String]]
    meth.getName == "main" &&
    Modifier.isStatic(meth.getModifiers) &&
    paramsOK(meth.getParameterTypes)
  }

  def run(
      classpath: List[String],
      objectName: String,
      arguments: Seq[String]): Unit =
  {
    val classpathURLs = classpath.map(s => new File(s).toURL).toArray
    val mainLoader = new URLClassLoader(classpathURLs, null)
    val clsToRun = Class.forName(objectName, true, mainLoader)

    val method = clsToRun.getMethods.find(isMainMethod) match {
      case Some(meth) =>
        meth
      case None => {
        throw new Error("no main method in object " + objectName)
      }
    }
    val res = method.invoke(null, List(arguments.toArray).toArray)
    ()
  }
}
