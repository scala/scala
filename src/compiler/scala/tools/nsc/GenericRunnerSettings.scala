/* NSC -- new Scala compiler
 * Copyright 2006-2013 LAMP/EPFL
 * @author  Lex Spoon
 */

package scala.tools.nsc

import java.net.URL
import scala.tools.util.PathResolver

class GenericRunnerSettings(error: String => Unit) extends Settings(error) {
  lazy val classpathURLs: Seq[URL] = new PathResolver(this).resultAsURLs

  val howtorun =
    ChoiceSetting(
      "-howtorun",
      "how",
      "how to run the specified code",
      List("object", "script", "jar", "repl", "guess"),
      "guess") withAbbreviation "--how-to-run"

  val loadfiles =
    MultiStringSetting(
      "-I",
      "file",
      "load a file line-by-line")

  val pastefiles =
    MultiStringSetting(
      "-i",
      "file",
      "paste a file")

  val execute =
    StringSetting(
      "-e",
      "string",
      "execute a single command",
      "")

  val save =
    BooleanSetting(
      "-save",
      "save the compiled script (assumes the code is a script)") withAbbreviation "-savecompiled" withAbbreviation "--save"

  val nc = BooleanSetting(
      "-nc",
      "do not use the legacy fsc compilation daemon").withAbbreviation("-nocompdaemon").withAbbreviation("--no-compilation-daemon")
      .withDeprecationMessage("scripts use cold compilation by default; use -Yscriptrunner for custom behavior")
      .withPostSetHook { x: BooleanSetting => Yscriptrunner.value = if (x) "default" else "resident" }
}
