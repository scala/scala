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
import org.apache.tools.ant.Task;
import org.apache.tools.ant.types.Commandline;
import org.apache.tools.ant.types.Path;


/**
 * The <code>ScalapTask</code> class provides an Ant task for
 * for the <code>scalap</code> command.
 * i.e.<pre>
 *  &lt;scalap classpath="${build}" classname="examples.sort" private="true"/&gt;
 * </pre>
 *
 * @author  Stephane Micheloud
 * @version 1.0
 */

class ScalapTask extends Task {

  private val PRODUCT = "scalap";

  private var classpath = ScalaRuntime.classpath;
  private var classname = "";

  private var showPrivateDefs = false;
  private var isVerbose       = false;

  override def getTaskName(): String = PRODUCT;

  def setClasspath(s: Path): Unit = classpath.append(s);

  def setCp(s: Path) = setClasspath(s);

  def setClassname(name: String): Unit = classname = name;

  def setPrivate(show: Boolean): Unit = showPrivateDefs = show;

  def setVerbose(verbose: Boolean): Unit = isVerbose = verbose;

  override def execute() = try {
    System.setProperty("scala.home", ScalaRuntime.home.toString());
    System.setProperty("scala.product", PRODUCT);
    System.setProperty("scala.version", scala.tools.scalap.Main.VERSION);
    System.setProperty("scala.class.path", ".");
    System.setProperty("scala.boot.class.path", ScalaRuntime.bootclasspath.toString());

    scala.tools.scalap.Main.main(getArgs());
  }
  catch {
    case e =>
      throw new BuildException("exception occurred:" + e.getClass());
  }

  private def getArgs() = {
    val cmd = new Commandline();
    cmd.createArgument().setValue("-classpath");
    cmd.createArgument().setPath(classpath);
    if (showPrivateDefs) cmd.createArgument().setValue("-private");
    if (isVerbose) cmd.createArgument().setValue("-verbose");
    cmd.createArgument().setValue(classname);
    cmd.getArguments()
  }
}
