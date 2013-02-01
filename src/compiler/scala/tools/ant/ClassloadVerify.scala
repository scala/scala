/*                     __                                               *\
**     ________ ___   / /  ___     Scala Ant Tasks                      **
**    / __/ __// _ | / /  / _ |    (c) 2005-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools.ant

import org.apache.tools.ant.Project
import org.apache.tools.ant.types.{Path, Reference}
import scala.collection.JavaConverters._
import scala.tools.util.VerifyClass

class ClassloadVerify extends ScalaMatchingTask {

  /** The class path to use for this compilation. */
  protected var classpath: Option[Path] = None

  /** Sets the `classpath` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value of `classpath`. */
  def setClasspath(input: Path) {
    classpath = Some(input)
  }

  def setClasspathref(input: Reference) {
    val p = new Path(getProject())
    p.setRefid(input)
    classpath = Some(p)
  }

  private def getClasspath: Array[String] = classpath match {
    case None     => buildError("Member 'classpath' is empty.")
    case Some(x)  => x.list.toArray
  }

  override def execute(): Unit = {
    val results = VerifyClass.run(getClasspath).asScala
    results foreach (r => log("Checking: " + r, Project.MSG_DEBUG))
    val errors = for((name, error) <- results; if error != null) yield (name,error)
    if(errors.isEmpty) {
      // TODO - Log success
      log("Classload verification succeeded with " + results.size + " classes.", Project.MSG_INFO)
    } else {
      for((name, error) <- errors) {
         log(name + " failed verification with: " + error, Project.MSG_ERR)
      }
      buildError(errors.size + " classload verification errors on " + results.size + " classes.")
    }
  }

}
