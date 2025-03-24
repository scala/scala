/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc

import java.net.URL

import scala.tools.nsc.settings.{DefaultPathFactory, PathFactory}
import scala.tools.util.PathResolver

class GenericRunnerSettings(error: String => Unit, pathFactory: PathFactory) extends Settings(error, pathFactory) {
  def this(error: String => Unit) = this(error, DefaultPathFactory)
  lazy val classpathURLs: Seq[URL] = {
    val registry = new CloseableRegistry
    try {
      new PathResolver(this, registry).resultAsURLs
    } finally {
      registry.close()
    }
  }

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

  @deprecated("check Yscriptrunner instead", since="2.13.0")
  val nc = BooleanSetting(
      "-nc",
      "do not use the legacy fsc compilation daemon").withAbbreviation("-nocompdaemon").withAbbreviation("--no-compilation-daemon")
      .withDeprecationMessage("scripts use cold compilation by default; use -Yscriptrunner for custom behavior")
      .withPostSetHook { x: BooleanSetting => Yscriptrunner.value = if (x.value) "default" else "resident" }
}
