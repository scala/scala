/* NSC -- new Scala compiler
 * Copyright 2006-2011 LAMP/EPFL
 * @author  Lex Spoon
 */


package scala.tools.nsc

class GenericRunnerSettings(error: String => Unit)
extends Settings(error) {
  val jarfile =
    StringSetting(
      "-jar",
      "jar",
      "Specify the jarfile in which to look for the main class",
      "")

  val howtorun =
    ChoiceSetting(
      "-howtorun",
      "how",
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
