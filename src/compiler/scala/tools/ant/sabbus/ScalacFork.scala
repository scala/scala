/*                     __                                               *\
**     ________ ___   / /  ___     Scala Ant Tasks                      **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.tools.ant.sabbus

import java.io.File
import java.io.FileWriter
import org.apache.tools.ant.Project
import org.apache.tools.ant.taskdefs.{MatchingTask, Java}
import org.apache.tools.ant.util.{GlobPatternMapper, SourceFileScanner}

class ScalacFork extends MatchingTask with TaskArgs {
  def setSrcdir(input: File) {
    sourceDir = Some(input)
  }

  def setFailOnError(input: Boolean) {
    failOnError = input
  }

  def setTimeout(input: Long) {
    timeout = Some(input)
  }

  def setJvmArgs(input: String) {
    jvmArgs = Some(input)
  }

  def setArgfile(input: File) {
    argfile = Some(input)
  }

  private var sourceDir: Option[File] = None
  private var failOnError: Boolean = true
  private var timeout: Option[Long] = None
  private var jvmArgs: Option[String] = None
  private var argfile: Option[File] = None

  override def execute() {
    if (compilerPath.isEmpty) error("Mandatory attribute 'compilerpath' is not set.")
    if (sourceDir.isEmpty) error("Mandatory attribute 'srcdir' is not set.")
    if (destinationDir.isEmpty) error("Mandatory attribute 'destdir' is not set.")

    val settings = new Settings
    settings.d = destinationDir.get
    if (!compTarget.isEmpty) settings.target = compTarget.get
    if (!compilationPath.isEmpty) settings.classpath = compilationPath.get
    if (!sourcePath.isEmpty) settings.sourcepath = sourcePath.get
    if (compTarget.isDefined && compTarget.get == "msil") settings.sourcedir = sourceDir.get
    if (!params.isEmpty) settings.more = params.get

    // not yet used: compilerPath, sourcedir (used in mapper), failonerror, timeout

    val mapper = new GlobPatternMapper()
    if (compTarget.isDefined && compTarget.get == "msil")
      mapper.setTo("*.msil")
    else
      mapper.setTo("*.class")
    mapper.setFrom("*.scala")
    val includedFiles: Array[File] =
      new SourceFileScanner(this).restrict(
        getDirectoryScanner(sourceDir.get).getIncludedFiles,
        sourceDir.get,
        destinationDir.get,
        mapper
      ) map (new File(sourceDir.get, _))
    if (includedFiles.size > 0 || argfile.isDefined) {
      if (includedFiles.size > 0)
        log("Compiling "+ includedFiles.size +" file"+
            (if (includedFiles.size > 1) "s" else "") +" to "+ destinationDir.get)
      if (argfile.isDefined)
        log("Using argument file: @"+ argfile.get)

      val java = new Java(this) // set this as owner
      java.setFork(true)
      // using 'setLine' creates multiple arguments out of a space-separated string
      if (!jvmArgs.isEmpty) java.createJvmarg().setLine(jvmArgs.get)
      java.setClasspath(compilerPath.get)
      java.setClassname("scala.tools.nsc.Main")
      if (!timeout.isEmpty) java.setTimeout(timeout.get)

      //dump the arguments to a file and do  "java @file"
      val tempArgFile = File.createTempFile("scalacfork","")
      val outf = new FileWriter(tempArgFile)
      for (arg <- settings.toArgs)
        { outf.write(arg) ; outf.write(" ") }
      for (file <- includedFiles)
        { outf.write(file.getPath) ; outf.write(" ") }
      outf.close

      java.createArg().setValue("@"+ tempArgFile.getAbsolutePath)
      if (argfile.isDefined)
        java.createArg().setValue("@"+ argfile.get)

      log(java.getCommandLine.getCommandline.mkString("", " ", ""), Project.MSG_VERBOSE)
      val res = java.executeJava()
      if (failOnError && res != 0)
        error("Compilation failed because of an internal compiler error;"+
              " see the error output for details.")
    }
  }
}
