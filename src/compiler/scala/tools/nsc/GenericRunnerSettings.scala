/* NSC -- new Scala compiler
 * Copyright 2006-2010 LAMP/EPFL
 * @author  Lex Spoon
 */

// $Id$

package scala.tools.nsc

class GenericRunnerSettings(error: String => Unit)
extends Settings(error) {
  val howtorun =
    ChoiceSetting(
      "-howtorun",
      "how to run the specified code",
      List("guess", "object", "script"),
      "guess")

  val loadfiles =
    MultiStringSetting(
        "-i",
        "file",
        "load a file (assumes the code is given interactively)")

  val execute =
    StringSetting(
        "-e",
        "string",
        "execute a single command",
        "")

  val savecompiled =
    BooleanSetting(
        "-savecompiled",
        "save the compiled script (assumes the code is a script)")

  val nocompdaemon =
    BooleanSetting(
        "-nocompdaemon",
        "do not use the fsc compilation daemon")
}
