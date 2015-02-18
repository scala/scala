/*                     __                                               *\
**     ________ ___   / /  ___     Scala Ant Tasks                      **
**    / __/ __// _ | / /  / _ |    (c) 2005-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools.ant

import org.apache.tools.ant.{AntClassLoader, Project}
import org.apache.tools.ant.taskdefs.Java
import org.apache.tools.ant.types.Path

import scala.tools.nsc.Settings
import scala.tools.nsc.io.File
import scala.tools.nsc.settings.FscSettings
import scala.reflect.internal.util.ScalaClassLoader

/** An Ant task to compile with the fast Scala compiler (`fsc`).
 *
 *  In addition to the attributes shared with the `Scalac` task, this task
 *  also accepts the following attributes:
 *  - `reset`
 *  - `server`
 *  - `shutdown`
 *  - `ipv4`
 *  - `maxIdle`
 *
 *  @author Stephane Micheloud
 */
class FastScalac extends Scalac {

  private var resetCaches: Boolean = false

  private var serverAddr: Option[String] = None

  private var shutdownServer: Boolean = false

  private var useIPv4: Boolean = false

  private var idleMinutes: Option[Int] = None

/*============================================================================*\
**                             Properties setters                             **
\*============================================================================*/

  /** Sets the `reset` attribute. Used by [[http://ant.apache.org Ant]].
   *
   *  @param input The value for `reset`.
   */
  def setReset(input: Boolean) { resetCaches = input }

  /** Sets the `server` attribute. Used by [[http://ant.apache.org Ant]].
   *
   *  @param input The value for `server`.
   */
  def setServer(input: String) { serverAddr = Some(input) }

  /** Sets the `shutdown` attribute. Used by [[http://ant.apache.org Ant]].
   *
   *  @param input The value for `shutdown`.
   */
  def setShutdown(input: Boolean) { shutdownServer = input }

  /** Sets the `ipv4` attribute. Used by [[http://ant.apache.org Ant]].
   *
   *  @param input The value for `ipv4`.
   */
  def setIPv4(input: Boolean) { useIPv4 = input }

  /** Sets the `maxIdle` attribute. Used by [[http://ant.apache.org Ant]].
   *
   *  @param input The value for `maxIdle`.
   */
  def setMaxIdle(input: Int) { if (0 <= input) idleMinutes = Some(input) }

/*============================================================================*\
**                             The execute method                             **
\*============================================================================*/

  override protected def newSettings(error: String=>Unit): Settings =
    new FscSettings(error)

  /** Performs the compilation. */
  override def execute() {
    val (settings, sourceFiles, javaOnly) = initialize
    if (sourceFiles.isEmpty || javaOnly)
      return

    // initialize fsc specific settings
    val s = settings.asInstanceOf[FscSettings] // safe (newSettings)
    s.reset.value = resetCaches
    if (!serverAddr.isEmpty) s.server.value = serverAddr.get
    s.shutdown.value = shutdownServer
    s.preferIPv4.value = useIPv4
    if (!idleMinutes.isEmpty) s.idleMins.value = idleMinutes.get

    val stringSettings =
      List(
        /*scalac*/
        s.bootclasspath, s.classpath, s.extdirs, s.dependencyfile, s.encoding,
        s.outdir, s.sourcepath,
        /*fsc*/
        s.server
      ) filter (_.value != "") flatMap (x => List(x.name, x.value))

    val choiceSettings =
      List(
        /*scalac*/
        s.debuginfo, s.target
      ) filter (x => x.value != x.default) map (x => "%s:%s".format(x.name, x.value))

    val booleanSettings =
      List(
        /*scalac*/
        s.debug, s.deprecation, s.explaintypes, s.nospecialization, s.nowarn,
        s.optimise, s.unchecked, s.usejavacp, s.verbose,
        /*fsc*/
        s.preferIPv4, s.reset, s.shutdown
      ) filter (_.value) map (_.name)

    val intSettings =
      List(
        /*fsc*/
        s.idleMins
      ) filter (x => x.value != x.default) flatMap (x => List(x.name, x.value.toString))

    val phaseSetting = {
      val s = settings.log
      if (s.value.isEmpty) Nil
      else List("%s:%s".format(s.name, s.value.mkString(",")))
    }

    val fscOptions =
      stringSettings ::: choiceSettings ::: booleanSettings ::: intSettings ::: phaseSetting

    val java = new Java(this)
    java setFork true
    // use same default memory options as in fsc script
    java.createJvmarg() setValue "-Xmx256M"
    java.createJvmarg() setValue "-Xms32M"
    val scalacPath: Path = {
      val path = new Path(getProject)
      if (compilerPath.isDefined) path add compilerPath.get
      else getClass.getClassLoader match {
        case cl: AntClassLoader =>
          path add new Path(getProject, cl.getClasspath)
        case _ =>
          buildError("Compilation failed because of an internal compiler error;"+
                     " see the error output for details.")
      }
      path
    }
    java.createJvmarg() setValue ("-Xbootclasspath/a:"+scalacPath)
    s.jvmargs.value foreach (java.createJvmarg() setValue _)

    val scalaHome: String = try {
      val url = ScalaClassLoader.originOfClass(classOf[FastScalac]).get
      File(url.getFile).jfile.getParentFile.getParentFile.getAbsolutePath
    } catch {
      case _: Throwable =>
        buildError("Compilation failed because of an internal compiler error;"+
                   " couldn't determine value for -Dscala.home=<value>")
    }
    java.createJvmarg() setValue "-Dscala.usejavacp=true"
    java.createJvmarg() setValue ("-Dscala.home="+scalaHome)
    s.defines.value foreach (java.createJvmarg() setValue _)

    java setClassname "scala.tools.nsc.MainGenericRunner"
    java.createArg() setValue "scala.tools.nsc.CompileClient"

    // Encode scalac/javac args for use in a file to be read back via "@file.txt"
    def encodeScalacArgsFile(t: Traversable[String]) = t map { s =>
      if(s.find(c => c <= ' ' || "\"'\\".contains(c)).isDefined)
        "\"" + s.flatMap(c => (if(c == '"' || c == '\\') "\\" else "") + c ) + "\""
      else s
    } mkString "\n"

    // dump the arguments to a file and do "java @file"
    val tempArgFile = File.makeTemp("fastscalac")
    val tokens = fscOptions ++ (sourceFiles map (_.getPath))
    tempArgFile writeAll encodeScalacArgsFile(tokens)

    val paths = List(Some(tempArgFile.toAbsolute.path), argfile).flatten map (_.toString)
    val res = execWithArgFiles(java, paths)

    if (failonerror && res != 0)
      buildError("Compilation failed because of an internal compiler error;"+
            " see the error output for details.")
  }
}
