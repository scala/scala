package scala.tools.ant.sabbus

import java.io.File
import org.apache.tools.ant.Task
import org.apache.tools.ant.types.{Path, Reference}

trait TaskArgs { this: Task =>

  def setId(input: String): Unit = {
    id = Some(input)
  }

  def setParams(input: String): Unit = {
    params = params match {
      case None => Some(input)
      case Some(ps) => Some(ps + " " + input)
    }
  }

  def setTarget(input: String): Unit = {
    compTarget = Some(input)
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

  protected var id: Option[String] = None
  protected var params: Option[String] = None
  protected var compTarget: Option[String] = None
  protected var compilationPath: Option[Path] = None
  protected var sourcePath: Option[Path] = None
  protected var compilerPath: Option[Path] = None
  protected var destinationDir: Option[File] = None
}
