/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.scalac;

/**
 * This class defines all compiler phases and maintains a list of
 * active phases.
 */
class CompilerPhases extends scalac.CompilerPhases {

  protected override def PARSER_PHASE(): Class =
    Class.forName("scala.tools.scalac.ast.parser.ParserPhase$class");
  protected override def ANALYZER_PHASE(): Class =
    Class.forName("scala.tools.scalac.typechecker.AnalyzerPhase$class");

}
