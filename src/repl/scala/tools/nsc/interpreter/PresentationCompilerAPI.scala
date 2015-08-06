/* NSC -- new Scala compiler
 * Copyright 2005-2015 LAMP/EPFL
 * @author Martin Odersky
 */
package scala.tools.nsc.interpreter

import scala.reflect.internal.util.RangePosition
import scala.tools.nsc.typechecker.TypeStrings

class PresentationCompilerAPI(interpreter: IMain) {

  def typeAt(code: String, selectionStart: Int, selectionEnd: Int): Option[String] =
    interpreter.presentationCompile(code) match {
      case Left(_)       => None
      case Right(result) =>
        val start = selectionStart + result.preambleLength
        val end   = selectionEnd + result.preambleLength
        val pos   = new RangePosition(result.unit.source, start, start, end)
        val tree  = result.compiler.typedTreeAt(pos)
        Some(interpreter.global.exitingTyper(tree.tpe.toString))
    }
}
