/*                     __                                               *\
**     ________ ___   / /  ___     Scala Ant Tasks                      **
**    / __/ __// _ | / /  / _ |    (c) 2005-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools.ant

import java.io.File

/** An Ant task that lists a jar's classes into its manifest.
 *
 *  This task can take the following parameters as attributes:
 *  - `file` (mandatory).
 *
 * @author  Raphael Jolly
 * @version 1.0
 */
class JarLister extends ScalaMatchingTask {

/*============================================================================*\
**                             Ant user-properties                            **
\*============================================================================*/

  /** The path to the jar file. */
  private var file: Option[File] = None

  /** The path to the output file. */
  private var output: Option[File] = None

/*============================================================================*\
**                             Properties setters                             **
\*============================================================================*/

  /** Sets the file attribute. */
  def setFile(input: File) =
    file = Some(input)

  /** Sets the output attribute. */
  def setOutput(input: File) =
    output = Some(input)

/*============================================================================*\
**                           The big execute method                           **
\*============================================================================*/

  /** Performs the jar listing. */
  override def execute() = {
    // Tests if all mandatory attributes are set and valid.
    if (file.isEmpty) buildError("Attribute 'file' is not set.")

    scala.tools.nsc.JarLister.process(file.get.getPath, if (output.isEmpty) "" else output.get.getPath)
  }

}
