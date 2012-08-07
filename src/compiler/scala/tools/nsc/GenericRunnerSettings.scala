/* NSC -- new Scala compiler
 * Copyright 2006-2012 LAMP/EPFL
 * @author  Lex Spoon
 */

package scala.tools.nsc

import scala.tools.util.PathResolver

class GenericRunnerSettings(error: String => Unit) extends Settings(error) {
  def classpathURLs = new PathResolver(this).asURLs

  val howtorun =
    ChoiceSetting(
      "-howtorun",
      "how",
      "how to run the specified code",
      List("object", "script", "jar", "guess"),
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

  val save =
    BooleanSetting(
      "-save",
      "save the compiled script (assumes the code is a script)") withAbbreviation "-savecompiled"

  val nc = BooleanSetting(
      "-nc",
      "do not use the fsc compilation daemon") withAbbreviation "-nocompdaemon"

  @deprecated("Use `nc` instead", "2.9.0") def nocompdaemon = nc
  @deprecated("Use `save` instead", "2.9.0") def savecompiled = save
}
