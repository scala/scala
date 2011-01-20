/* NSC -- new Scala compiler
 * Copyright 2002-2011 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package reporters

import scala.tools.util.AbstractTimer

/**
 * This class implements a timer that uses a Reporter to issue
 * timings.
 */
class ReporterTimer(reporter: Reporter) extends AbstractTimer {

  def issue(msg: String, duration: Long) =
    reporter.info(null, "[" + msg + " in " + duration + "ms]", false)

}
