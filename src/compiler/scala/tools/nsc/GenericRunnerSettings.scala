/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
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
      "guess")

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
      "do not use the fsc compilation daemon") withAbbreviation "-nocompdaemon" withPostSetHook((x: BooleanSetting) => {_useCompDaemon = !x.value })


  private[this] var _useCompDaemon = true
  def useCompDaemon: Boolean = _useCompDaemon
}
