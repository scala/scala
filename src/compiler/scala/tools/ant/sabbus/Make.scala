/*                     __                                               *\
**     ________ ___   / /  ___     Scala Ant Tasks                      **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools.ant.sabbus

import java.net.URL
import java.io.File
import org.apache.tools.ant.Task
import org.apache.tools.ant.types.{Path, Reference}

class Make extends Task with TaskArgs {
  override def execute: Unit = {
    if (id.isEmpty) error("Mandatory attribute 'id' is not set.")
    if (compilerPath.isEmpty) error("Mandatory attribute 'compilerpath' is not set.")
    val settings = new Settings
    if (!destinationDir.isEmpty) settings.d = destinationDir.get
    if (!compTarget.isEmpty) settings.target = compTarget.get
    if (!compilationPath.isEmpty) settings.classpath = compilationPath.get
    if (!sourcePath.isEmpty) settings.sourcepath = sourcePath.get
    if (!params.isEmpty) settings.more = params.get
    Compilers.make(id.get, (compilerPath.get.list.map{ path => new File(path).toURL }), settings)
  }
}
