/* NSC -- new Scala compiler
 * Copyright 2005-2015 LAMP/EPFL
 * @author Martin Odersky
 */
package scala.tools.nsc.interpreter

import scala.reflect.internal.util.RangePosition
import scala.tools.nsc.typechecker.TypeStrings

class PresentationCompilerAPI(interpreter: IMain) {
  /**
   * Index the top-level classes in the the provided packages for the `not found: Class` error
   * message.
   */
  def scanPackagesForClassNotFoundMessage(packages: Set[String]): Unit = {
    interpreter.global.platform.addForceIndexPackages(packages)
  }

  def typeAt(code: String, selectionStart: Int, selectionEnd: Int): Option[String] = {
    interpreter.presentationCompile(code) match {
      case Left(_) => None
      case Right(result) =>
        val start = selectionStart + result.preambleLength
        val end = selectionEnd + result.preambleLength
        val pos = new RangePosition(result.unit.source, start, start, end)
        val tree = result.compiler.typedTreeAt(pos)
        Some(interpreter.global.exitingTyper(tree.tpe.toString))
    }
  }
}
