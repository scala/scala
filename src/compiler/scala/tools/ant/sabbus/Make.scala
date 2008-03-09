/*                     __                                               *\
**     ________ ___   / /  ___     Scala Ant Tasks                      **
**    / __/ __// _ | / /  / _ |    (c) 2005-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools.ant.sabbus

import java.net.URL
import java.io.File
import org.apache.tools.ant.Task
import org.apache.tools.ant.types.{Path, Reference}

class Make extends Task {

  def setId(input: String): Unit = {
    id = Some(input)
  }

  def setParams(input: String): Unit = {
    params = params match {
      case None => Some(input)
      case Some(ps) => Some(ps + " " + input)
    }
  }

  def setCompilationPath(input: Path): Unit = {
    if (compilationPath.isEmpty) compilationPath = Some(input)
    else compilationPath.get.append(input)
  }

  def createCompilationPath: Path = {
    if (compilationPath.isEmpty) compilationPath = Some(new Path(getProject()))
    compilationPath.get.createPath()
  }

  def setCompilationPathRef(input: Reference): Unit = {
    createCompilationPath.setRefid(input)
  }

  def setSrcPath(input: Path): Unit = {
    if (sourcePath.isEmpty) sourcePath = Some(input)
    else sourcePath.get.append(input)
  }

  def createSrcPath: Path = {
    if (sourcePath.isEmpty) sourcePath = Some(new Path(getProject()))
    sourcePath.get.createPath()
  }

  def setSrcPathRef(input: Reference): Unit = {
    createSrcPath.setRefid(input)
  }

  def setCompilerPath(input: Path): Unit = {
    if (compilerPath.isEmpty) compilerPath = Some(input)
    else compilerPath.get.append(input)
  }

  def createCompilerPath: Path = {
    if (compilerPath.isEmpty) compilerPath = Some(new Path(getProject()))
    compilerPath.get.createPath()
  }

  def setCompilerPathRef(input: Reference): Unit = {
    createCompilerPath.setRefid(input)
  }

  def setDestdir(input: File): Unit = {
    destinationDir = Some(input)
  }

  private var id: Option[String] = None
  private var params: Option[String] = None
  private var compilationPath: Option[Path] = None
  private var sourcePath: Option[Path] = None
  private var compilerPath: Option[Path] = None
  private var destinationDir: Option[File] = None

  override def execute: Unit = {
    if (id.isEmpty) error("Mandatory attribute 'id' is not set.")
    if (compilerPath.isEmpty) error("Mandatory attribute 'compilerpath' is not set.")
    val settings = new Settings
    if (!destinationDir.isEmpty) settings.d = destinationDir.get
    if (!compilationPath.isEmpty) settings.classpath = compilationPath.get
    if (!sourcePath.isEmpty) settings.sourcepath = sourcePath.get
    if (!params.isEmpty) settings.more = params.get
    Compilers.make(id.get, (compilerPath.get.list.map{ path => new File(path).toURL }), settings)
  }

}
