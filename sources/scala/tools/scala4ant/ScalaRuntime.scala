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
 * @version 1.1
 */

object ScalaRuntime {

  private val SCALA_RUNTIME_LIB     = "lib";
  private val SCALA_RUNTIME_SOURCES = "sources";
  private val SCALA_RUNTIME_CLASSES = "classes";

  private val SCALA_JAR_SOME_CLASS  = "scala.ScalaObject";
  private val TOOLS_JAR_SOME_CLASS  = getClass().getName(); // myself !
  private val FJBG_JAR_SOME_CLASS   = "ch.epfl.lamp.fjbg.JFieldOrMethod";

  check(SCALA_JAR_SOME_CLASS, "scala.jar");
  check(TOOLS_JAR_SOME_CLASS, "tools.jar");
  check(FJBG_JAR_SOME_CLASS,  "fjbg.jar");

  val home: Path = {
    val p = Path.systemClasspath.createPath();
    val name = getResourceLocation(SCALA_JAR_SOME_CLASS);
    val i = Math.max(name.lastIndexOf(SCALA_RUNTIME_LIB),
                     name.lastIndexOf(SCALA_RUNTIME_CLASSES));
    p.setPath(if (i > 0) name.substring(0, i - 1) else name);
    p
  }

  val sourcepath: Path = {
    val p = Path.systemClasspath.createPath();
    val s = home.toString() + java.io.File.separator + SCALA_RUNTIME_SOURCES;
    p.setPath(s);
    p
  }

  val scalapath: Path =
    getResourcePath(SCALA_JAR_SOME_CLASS);

  val classpath: Path = {
    val toolsPath = getResourcePath(TOOLS_JAR_SOME_CLASS);
    val fjbgPath  = getResourcePath(FJBG_JAR_SOME_CLASS);
    val p = Path.systemClasspath.createPath();
    p.append(scalapath);
    p.append(toolsPath);
    p.append(fjbgPath);
    p
  }

  val bootclasspath: Path = {
    val p = Path.systemClasspath.createPath();
    p.append(scalapath);
    p.append(sourcepath);
    p
  }

  /**
   * Check if the required Scala libraries are present.
   */
  private def check(className: String, jarName: String) = try {
    Class.forName(className)
  } catch {
    case e: ClassNotFoundException =>
      throw new BuildException("Cannot run scala4ant.\n"
        + "It seems " + jarName + " is not in your CLASSPATH.");
  }

  /**
   * Return the full path string of the the jar file or the
   * directory containing the class <code>className</code>.
   *
   * @param classname
   * @return
   */
  private def getResourceLocation(className: String): String = {
    def asResourceName(resource: String) = {
      val name =
        if (! resource.startsWith("/")) "/" + resource else resource;
      name.replace('.', '/') + ".class"
    }
    val rsrcName = asResourceName(className);
    val url = getClass().getResource(rsrcName);
    if (url != null) {
      val fn = url.getFile();
      val name = if (fn.startsWith("file:")) fn.substring(5) else fn;
      val inx = name.lastIndexOf('!');
      val end = if (inx > 0) inx else name.lastIndexOf(rsrcName);
      name.substring(0, end);
    }
    else
      throw new BuildException("Cannot run scala4ant.\n"
        + "Scala installation directory cannot be found.");
  }

  /**
   * Return the Ant path of the class <code>className</code>.
   *
   * @param classname
   * @return
   */
  private def getResourcePath(className: String) = {
    val name = getResourceLocation(className);
    val p = Path.systemClasspath.createPath();
    p.setPath(name);
    p
  }

  // for testing
  def main(args: Array[String]): Unit = {
    System.out.println("ScalaRuntime.home = " + ScalaRuntime.home);
    System.out.println("ScalaRuntime.classpath = " + ScalaRuntime.classpath);
    System.out.println("ScalaRuntime.bootclasspath = " + ScalaRuntime.bootclasspath);
    System.out.println("ScalaRuntime.sourcepath = " + ScalaRuntime.sourcepath);
    System.out.println("ScalaRuntime.scalapath = " + ScalaRuntime.scalapath);
  }
}
