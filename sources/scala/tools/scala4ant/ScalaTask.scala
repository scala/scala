/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.tools.scala4ant;

import org.apache.tools.ant.taskdefs.Java;
import org.apache.tools.ant.types.Path;


/**
 * The <code>ScalaTask</code> class provides an Ant task for
 * the <code>scala</code> command.
 * i.e.<pre>
 *  &lt;scala classpath="${build}" classname="examples.sort"/&gt;
 * </pre>
 *
 * @author  Stephane Micheloud
 * @version 1.0
 */

class ScalaTask extends Java {

  private val PRODUCT = "scala";

  override def getTaskName(): String = PRODUCT;

  override def setClasspath(s: Path): Unit = {
    val cp = ScalaRuntime.classpath;
    cp.append(s);
    super.setClasspath(cp)
  }

}
