/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Lex Spoon
 */

package scala.tools.nsc

import interpreter._

@deprecated("Use a class in the scala.tools.nsc.interpreter package.")
object MainInterpreter {
  def main(args: Array[String]): Unit = new ILoop main args
}
