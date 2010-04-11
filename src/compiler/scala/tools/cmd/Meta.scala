/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package cmd

import nsc.io.File
import Interpolation._

/** Meta-options for command line tools.  We could have all kinds
 *  of additional goodness here, but for now it's completion and script
 *  generation.  See Demo for example usage.
 */
object Meta {
  trait Opt {
    def name: String
    def action: () => Unit
  }

  trait StdOpts {
    self: Spec with Interpolation =>

                          Bash.name     --> runAndExit(Bash.action())
    val runnerFileName  = Runner.name   --| ;

    if (runnerFileName.isDefined)
      runAndExit(Runner.action())

    /** I think we're as close as we can get to bundling completion with
     *  the program given the constraints imposed by bash.  This outputs
     *  the completion function to a tempfile and echoes ". /path/to/file"
     *  to the console.  Place it inside backtickes like `partest --bash`
     *  and voila, you have absorbed command completion.
     */
    object Bash extends Opt {
      val name    = "bash"
      val action  = () => {
        val file = File.makeTemp("scala.cmd.bash")
        file writeAll interpolate(bashTemplate)

        // Would be nice to print something like this but comments are
        // not always comments in bash, and breaking it is worse.
        // Console println ("# Run the following line, or issue the --bash command in `backticks`.")
        Console println (". " + file.normalize.path)
      }
    }

    /** A very basic runner script.
     */
    object Runner extends Opt {
      val name    = "generate-runner"
      val action  = () => {
        val file = File(runnerFileName.get)
        file writeAll interpolate(runnerTemplate)
        file setExecutable true
        ()
      }
    }
  }
}
