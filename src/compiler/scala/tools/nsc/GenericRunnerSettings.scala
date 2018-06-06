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
      List("object", "script", "jar", "repl", "fsc", "guess"),
      "guess")
    .withPostSetHook { s: ChoiceSetting => if (s.value == "fsc") { _useCompDaemon = true ; s.value = "script" } }

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
      "save the compiled script (assumes the code is a script)") withAbbreviation "-savecompiled"

  val nc = BooleanSetting(
      "-nc",
      "do not use the fsc compilation daemon and shut it down if it is running").withAbbreviation("-nocompdaemon")
      //.withDeprecationMessage("scripts use cold compilation by default; use -howtorun:fsc to start the compilation daemon")
      .withPostSetHook { x: BooleanSetting => _useCompDaemon = !x }

  private[this] var _useCompDaemon = false
  def useCompDaemon: Boolean = _useCompDaemon
}
