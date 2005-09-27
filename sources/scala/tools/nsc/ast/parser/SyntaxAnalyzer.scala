/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$
package scala.tools.nsc.ast.parser;

/** An nsc sub-component.
 */
abstract class SyntaxAnalyzer extends SubComponent with Parsers with Scanners {
  val phaseName = "parser";
  def newPhase(prev: Phase): StdPhase = new ParserPhase(prev);
  class ParserPhase(prev: scala.tools.nsc.Phase) extends StdPhase(prev) {
    def apply(unit: global.CompilationUnit): unit = {
      global.informProgress("parsing " + unit);
      unit.body = new Parser(unit).parse();
    }
  }
  //Moez addition. I wished not to add/modify here, but the fact that Parsers
  // are NOT accessible (because of Parsers' self type) except in SyntaxAnalyzer
  // had bitten me, and thus I had to add the following code here.
  def interpreterParse(unit: global.CompilationUnit): List[global.Tree] =
    new Parser(unit).templateStatSeq()
}

