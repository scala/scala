/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import scalac.{CompilerPhases => scalac_CompilerPhases}

package scala.tools.scalac {

/**
 * This class defines all compiler phases and maintains a list of
 * active phases.
 */
class CompilerPhases extends scalac_CompilerPhases {

  protected override def PARSER_PHASE(): Class =
    Class.forName("scala.tools.scalac.ast.parser.ParserPhase$class");
  protected override def ANALYZER_PHASE(): Class =
    Class.forName("scala.tools.scalac.typechecker.AnalyzerPhase$class");
  protected override def REFCHECK_PHASE(): Class =
    Class.forName("scala.tools.scalac.typechecker.RefCheckPhase$class");
  protected override def UNCURRY_PHASE(): Class =
    Class.forName("scala.tools.scalac.transformer.UnCurryPhase$class");
  protected override def TRANSMATCH_PHASE(): Class =
    Class.forName("scala.tools.scalac.transformer.TransMatchPhase$class");
  protected override def WHOLEPROG_PHASE(): Class =
    Class.forName("scala.tools.scalac.wholeprog.WholeProgPhase$class");
  protected override def ICODE_PHASE(): Class =
    Class.forName("scala.tools.scalac.icode.ICodePhase$class");
  protected override def GENJVMFROMICODE_PHASE(): Class =
    Class.forName("scala.tools.scalac.backend.GenJVMFromICodePhase$class");

  }
}
