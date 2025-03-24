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

import scala.tools.nsc.io.Path

/** Loads `compiler.properties` from the jar archive file.
 */
object Properties extends scala.util.PropertiesTrait {
  protected def propCategory   = "compiler"
  protected def pickJarBasedOn = classOf[Global]

  // settings based on jar properties, falling back to System prefixed by "scala."

  // messages to display at startup or prompt, format string with string parameters
  // Scala version, Java version, JVM name
  def residentPromptString = scalaPropOrElse("resident.prompt", "\nnsc> ")
  def shellPromptString    = scalaPropOrElse("shell.prompt", "%nscala> ")
  def shellWelcomeString   = scalaPropOrElse("shell.welcome",
    """Welcome to Scala %1$#s (%3$s, Java %2$s).
      |Type in expressions for evaluation. Or try :help.""".stripMargin
  )
  def shellBannerString   = scalaPropOrElse("shell.banner", shellWelcomeString)

  // message to display at EOF (which by default ends with
  // a newline so as not to break the user's terminal)
  def shellInterruptedString = scalaPropOrElse("shell.interrupted", f":quit$lineSeparator")

  // Where we keep fsc's state (ports/redirection)
  lazy val scalacDir = (Path(Properties.userHome) / ".scalac").createDirectory(force = false)
}
