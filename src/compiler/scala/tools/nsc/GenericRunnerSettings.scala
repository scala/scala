/* NSC -- new Scala compiler
 * Copyright 2006 LAMP/EPFL
 * @author  Lex Spoon
 */

// $Id$

package scala.tools.nsc

class GenericRunnerSettings(error: String => Unit)
extends Settings(error) {
  val howtorun =
    ChoiceSetting(
      "howtorun",
      "how to run the specified code",
      List("guess", "object", "script"),
      "guess")
}
