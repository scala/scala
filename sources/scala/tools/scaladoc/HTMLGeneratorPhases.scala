/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import scalac.PhaseDescriptor;
import scalac.util.Debug;

package scala.tools.scaladoc {

/**
 * The class <code>HTMLGeneratorPhases</code> defines the set
 * of processing phases belonging the the HTML generator tool.
 */
class HTMLGeneratorPhases extends scalac.CompilerPhases {

  //##########################################################################
  // Public Fields

  /** The phase descriptor of the HTML generator. */
  val HTMLGENERATOR =
    try {
      new PhaseDescriptor(
        "html generator",
        "generate html documentation",
        "generating html",
        Class.forName("scala.tools.scaladoc.HTMLGeneratorPhase$class"));
    } catch {
      case exception =>
        throw Debug.abort(exception);
    }

  /** Instance initializer */
  {
    val pos = insertAfter(HTMLGENERATOR, ANALYZER);
    val array = phases();
    // we skip all phases between HTMLGENERATOR and TERMINAL.
    var i = pos + 1;
    while (i < array.length - 1) {
      array(i).addSkipFlag();
      i = i + 1;
    }
  }

  //##########################################################################
}
}
