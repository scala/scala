/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.sys
package process

import processInternal._
import ProcessBuilder._

/** Represents a process that is running or has finished running.
 *  It may be a compound process with several underlying native processes (such as 'a #&& b`).
 */
trait Process {
  /** Blocks until this process exits and returns the exit code.*/
  def exitValue(): Int
  /** Destroys this process. */
  def destroy(): Unit
}

/** Methods for constructing simple commands that can then be combined. */
object Process extends ProcessImpl with ProcessCreation { }

trait ProcessCreation {
  def apply(command: String): ProcessBuilder                         = apply(command, None)
  def apply(command: Seq[String]): ProcessBuilder                    = apply(command, None)
  def apply(command: String, arguments: Seq[String]): ProcessBuilder = apply(command +: arguments, None)

  /** create ProcessBuilder with working dir set to File and extra environment variables */
  def apply(command: String, cwd: File, extraEnv: (String, String)*): ProcessBuilder =
    apply(command, Some(cwd), extraEnv: _*)

  /** create ProcessBuilder with working dir set to File and extra environment variables */
  def apply(command: Seq[String], cwd: File, extraEnv: (String, String)*): ProcessBuilder =
    apply(command, Some(cwd), extraEnv: _*)

  /** create ProcessBuilder with working dir optionaly set to File and extra environment variables */
  def apply(command: String, cwd: Option[File], extraEnv: (String, String)*): ProcessBuilder = {
    apply(command.split("""\s+"""), cwd, extraEnv : _*)
    // not smart to use this on windows, because CommandParser uses \ to escape ".
    /*CommandParser.parse(command) match {
      case Left(errorMsg) => error(errorMsg)
      case Right((cmd, args)) => apply(cmd :: args, cwd, extraEnv : _*)
    }*/
  }

  /** create ProcessBuilder with working dir optionaly set to File and extra environment variables */
  def apply(command: Seq[String], cwd: Option[File], extraEnv: (String, String)*): ProcessBuilder = {
    val jpb = new JProcessBuilder(command.toArray: _*)
    cwd foreach (jpb directory _)
    extraEnv foreach { case (k, v) => jpb.environment.put(k, v) }
    apply(jpb)
  }

  def apply(builder: JProcessBuilder): ProcessBuilder = new Simple(builder)
  def apply(file: File): FileBuilder                  = new FileImpl(file)
  def apply(url: URL): URLBuilder                     = new URLImpl(url)
  def apply(command: scala.xml.Elem): ProcessBuilder  = apply(command.text.trim)
  def apply(value: Boolean): ProcessBuilder           = apply(value.toString, if (value) 0 else 1)

  def apply(name: String, exitValue: => Int): ProcessBuilder = new Dummy(name, exitValue)
  def applySeq[T](builders: Seq[T])(implicit convert: T => Source): Seq[Source] = builders.map(convert)

  def cat(file: Source, files: Source*): ProcessBuilder = cat(file +: files)
  def cat(files: Seq[Source]): ProcessBuilder = {
    require(files.nonEmpty)
    files map (_.cat) reduceLeft (_ #&& _)
  }
}

trait ProcessImplicits {
  import Process._

  implicit def buildersToProcess[T](builders: Seq[T])(implicit convert: T => Source): Seq[Source] = applySeq(builders)
  implicit def builderToProcess(builder: JProcessBuilder): ProcessBuilder = apply(builder)
  implicit def fileToProcess(file: File): FileBuilder                     = apply(file)
  implicit def urlToProcess(url: URL): URLBuilder                         = apply(url)
  implicit def xmlToProcess(command: scala.xml.Elem): ProcessBuilder      = apply(command)
  implicit def stringToProcess(command: String): ProcessBuilder           = apply(command)
  implicit def stringSeqToProcess(command: Seq[String]): ProcessBuilder   = apply(command)
}
