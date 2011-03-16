/* NSC -- new Scala compiler
 * Copyright 2006-2011 LAMP/EPFL
 * @author  Lex Spoon
 */

package scala.tools.nsc

class GenericRunnerSettings(error: String => Unit) extends Settings(error) {
  // A -jar option means the remainder of the command line should be
  // passed to the main program in the jar.  If -jar is given, jarfile
  // will be set, but we also need to prepend the jar to the classpath
  // since it may not be there.
  val jarfile =
    StringSetting(
      "-jar",
      "jar",
      "Specify the jarfile in which to look for the main class",
      "").stopProcessing() withPostSetHook (classpath prepend _.value)

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

  val nc = BooleanSetting(
      "-nc",
      "do not use the fsc compilation daemon") withAbbreviation "-nocompdaemon"

  @deprecated("Use `nc` instead") def nocompdaemon = nc
}
