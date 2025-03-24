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

package scala.tools.nsc.fsc

import scala.tools.nsc.CompilerCommand
import scala.reflect.io.Directory
import scala.util.Properties.isWin

/** A compiler command for the offline compiler.
 *
 * @author Martin Odersky and Lex Spoon
 */
class OfflineCompilerCommand(arguments: List[String], settings: FscSettings) extends CompilerCommand(arguments, settings) {
  import settings.currentDir
  def extraFscArgs = List(currentDir.name, currentDir.value)

  locally {
    // if -current-dir is unset, we're on the client and need to obtain it.
    if (currentDir.isDefault) {
      // Prefer env variable PWD to system property user.dir because the former
      // deals better with paths not rooted at / (filesystem mounts.)
      // ... except on windows, because under cygwin PWD involves "/cygdrive"
      // instead of whatever it's supposed to be doing.
      val baseDirectory = {
        val pwd = System.getenv("PWD")
        if (pwd == null || isWin) Directory.Current getOrElse Directory("/")
        else Directory(pwd)
      }
      currentDir.value = baseDirectory.path
    }
    else {
      // Otherwise we're on the server and will use it to absolutize the paths.
      settings.absolutize()
    }
  }

  override def cmdName = "fsc"
  override def usageMsg = (
    createUsageMsg("where possible fsc", explain = false)(x => x.isStandard && settings.isFscSpecific(x.name)) +
    "\n\nStandard scalac options also available:" +
    optionsMessage(x => x.isStandard && !settings.isFscSpecific(x.name))
  )
}
