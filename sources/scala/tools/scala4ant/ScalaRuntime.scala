/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.tools.scala4ant;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.types.Path;


/**
 * The <code>ScalaRuntime</code> object provides informations
 * about the Scala runtime environment.
 *
 * @author  Stephane Micheloud
 * @version 1.0
 */

object ScalaRuntime {

  private val SCALA_RUNTIME_LIB     = "lib";
  private val SCALA_RUNTIME_SOURCES = "sources";

  private val SCALA_JAR_SOME_CLASS  = "scala.ScalaObject";
  private val TOOLS_JAR_SOME_CLASS  = getClass().getName(); // myself !
  private val FJBG_JAR_SOME_CLASS   = "ch.epfl.lamp.fjbg.JFieldOrMethod";

  check(SCALA_JAR_SOME_CLASS, "scala.jar");
  check(TOOLS_JAR_SOME_CLASS, "tools.jar");
  check(FJBG_JAR_SOME_CLASS, "fjbg.jar");

  val home: Path = {
    val p = Path.systemClasspath.createPath();
    val s = getJarFileName(SCALA_JAR_SOME_CLASS).split(SCALA_RUNTIME_LIB);
    p.setPath(s(0));
    p
  }

  val classpath: Path = {
    def getJarPath(classname: String) = {
      val name = getJarFileName(classname);
      if (name != null) {
        val p = Path.systemClasspath.createPath();
        p.setPath(name);
        p
      }
      else
        null
    }
    val scalaPath = getJarPath(SCALA_JAR_SOME_CLASS);
    val toolsPath = getJarPath(TOOLS_JAR_SOME_CLASS);
    val fjbgPath = getJarPath(FJBG_JAR_SOME_CLASS);
    val cp = if (scalaPath != null && toolsPath != null && fjbgPath != null) {
      scalaPath.append(toolsPath);
      scalaPath.append(fjbgPath);
      scalaPath
    }
    else
      Path.systemClasspath;
    cp
  }

  val sourcepath = {
    val p = Path.systemClasspath.createPath();
    val s = home.toString() + java.io.File.pathSeparator + SCALA_RUNTIME_SOURCES;
    p.setPath(s);
    p
  }

  val bootclasspath = {
    val p = classpath;
    p.append(sourcepath);
    p
  }

  /**
   * Check if the required libraries are present.
   */
  private def check(classname: String, jarname: String) = try {
    Class.forName(classname)
  } catch {
    case e: ClassNotFoundException =>
      throw new BuildException("Cannot run scala4ant.\n"
        + "It seems " + jarname + " is not in your CLASSPATH.");
  }

  /**
   * Return the full path string of the the jar file containing
   * the class <code>classname</code>.
   *
   * @param classname
   * @return
   */
  private def getJarFileName(classname: String): String = {
    def asResourceName(resource: String) = {
      val name =
        if (! resource.startsWith("/")) "/" + resource else resource;
      name.replace('.', '/') + ".class"
    }
    def findClass(className: String) =
	  getClass().getResource(asResourceName(className));
    val url = findClass(classname);
    if (url != null) {
      val s = url.getFile();
      assert(s.startsWith("file:"));
      s.substring(s.indexOf("file:") + 5, s.indexOf("!"))
    }
    else
      null
  }

}
