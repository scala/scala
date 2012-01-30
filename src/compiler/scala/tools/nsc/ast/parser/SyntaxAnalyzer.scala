/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Martin Odersky
 */

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
    override val keepsTypeParams = false

    def apply(unit: global.CompilationUnit) {
      import global._
      informProgress("parsing " + unit)
      unit.body =
        if (unit.isJava) new JavaUnitParser(unit).parse()
        else if (reporter.incompleteHandled) new UnitParser(unit).parse()
        else new UnitParser(unit).smartParse()

      if (settings.Yrangepos.value && !reporter.hasErrors)
        validatePositions(unit.body)
    }
  }
}

