/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$

package scala.tools.nsc
package ast.parser

import javac._

/** An nsc sub-component.
 */
abstract class SyntaxAnalyzer extends SubComponent with Parsers with MarkupParsers with Scanners with JavaParsers with JavaScanners {

  val phaseName = "parser"

  def newPhase(prev: Phase): StdPhase = new ParserPhase(prev)

  class ParserPhase(prev: scala.tools.nsc.Phase) extends StdPhase(prev) {
    override val checkable = false
    def apply(unit: global.CompilationUnit) {
      global.informProgress("parsing " + unit)
      unit.body =
        if (unit.source.file.name.endsWith(".java")) new JavaUnitParser(unit).parse()
        else if (!global.reporter.incompleteHandled) new UnitParser(unit).smartParse()
        else new UnitParser(unit).parse()
      if (global.settings.Yrangepos.value && !global.reporter.hasErrors) global.validatePositions(unit.body)
    }
  }
}

