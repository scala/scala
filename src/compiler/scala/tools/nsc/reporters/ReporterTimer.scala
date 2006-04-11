/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.nsc.reporters;

import scala.tools.util.AbstractTimer;

/**
 * This class implements a timer that uses a Reporter to issue
 * timings.
 */
class ReporterTimer(reporter : Reporter) extends AbstractTimer {

  def issue(msg : String, duration : Long) =
    reporter.info(null, "[" + msg + " in " + duration + "ms]", false);

}
