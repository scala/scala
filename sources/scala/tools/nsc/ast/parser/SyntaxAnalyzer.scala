/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$
package scala.tools.nsc.ast.parser;

/** An nsc sub-component.
 */
abstract class SyntaxAnalyzer extends SubComponent with Parsers with Scanners {
  class ParserPhase(prev: scala.tools.nsc.Phase) extends StdPhase(prev) {
    def name = "parser";
    def apply(unit: global.CompilationUnit): unit = {
      global.informProgress("parsing " + unit);
      unit.body = new Parser(unit).parse();
    }
  }
}

