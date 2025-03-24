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

package scala
package sys
package process

import processInternal._
import ProcessBuilder._
import scala.language.implicitConversions


/** Represents a process that is running or has finished running.
 *  It may be a compound process with several underlying native processes (such as `a #&& b`).
 *
 *  This trait is often not used directly, though its companion object contains
 *  factories for [[scala.sys.process.ProcessBuilder]], the main component of this
 *  package.
 *
 *  It is used directly when calling the method `run` on a `ProcessBuilder`,
 *  which makes the process run in the background. The methods provided on `Process`
 *  make it possible for one to block until the process exits and get the exit value,
 *  or destroy the process altogether.
 *
 *  @see [[scala.sys.process.ProcessBuilder]]
 */
trait Process {
  /** Returns this process alive status */
  def isAlive(): Boolean
  /** Blocks until this process exits and returns the exit code.*/
  def exitValue(): Int
  /** Destroys this process. */
  def destroy(): Unit
}

/** Methods for constructing simple commands that can then be combined. */
object Process extends ProcessImpl with ProcessCreation { }

/** Factories for creating [[scala.sys.process.ProcessBuilder]]. They can be
 *  found on and used through [[scala.sys.process.Process]]'s companion object.
 */
trait ProcessCreation {
  /** Creates a [[scala.sys.process.ProcessBuilder]] from a `String`, including the
    * parameters.
    *
    * @example {{{ apply("cat file.txt") }}}
    */
  def apply(command: String): ProcessBuilder                         = apply(command, None)

  /** Creates a [[scala.sys.process.ProcessBuilder]] from a sequence of `String`,
    * where the head is the command and each element of the tail is a parameter.
    *
    * @example {{{ apply("cat" :: files) }}}
    */
  def apply(command: scala.collection.Seq[String]): ProcessBuilder   = apply(command, None)

  /** Creates a [[scala.sys.process.ProcessBuilder]] from a command represented by a `String`,
    * and a sequence of `String` representing the arguments.
    *
    * @example {{{ apply("cat", files) }}}
    */
  def apply(command: String, arguments: scala.collection.Seq[String]): ProcessBuilder = apply(command +: arguments, None)

  /** Creates a [[scala.sys.process.ProcessBuilder]] with working dir set to `File` and extra
    * environment variables.
    *
    * @example {{{ apply("java", new java.io.File("/opt/app"), "CLASSPATH" -> "library.jar") }}}
    */
  def apply(command: String, cwd: File, extraEnv: (String, String)*): ProcessBuilder =
    apply(command, Some(cwd), extraEnv: _*)

  /** Creates a [[scala.sys.process.ProcessBuilder]] with working dir set to `File` and extra
    * environment variables.
    *
    * @example {{{ apply("java" :: javaArgs, new java.io.File("/opt/app"), "CLASSPATH" -> "library.jar") }}}
    */
  def apply(command: scala.collection.Seq[String], cwd: File, extraEnv: (String, String)*): ProcessBuilder =
    apply(command, Some(cwd), extraEnv: _*)

  /** Creates a [[scala.sys.process.ProcessBuilder]] with working dir optionally set to
    * `File` and extra environment variables.
    *
    * @example {{{ apply("java", params.get("cwd"), "CLASSPATH" -> "library.jar") }}}
    */
  def apply(command: String, cwd: Option[File], extraEnv: (String, String)*): ProcessBuilder =
    apply(Parser.tokenize(command), cwd, extraEnv: _*)

  /** Creates a [[scala.sys.process.ProcessBuilder]] with working dir optionally set to
    * `File` and extra environment variables.
    *
    * @example {{{ apply("java" :: javaArgs, params.get("cwd"), "CLASSPATH" -> "library.jar") }}}
    */
  def apply(command: scala.collection.Seq[String], cwd: Option[File], extraEnv: (String, String)*): ProcessBuilder = {
    val jpb = new JProcessBuilder(command.toArray: _*)
    cwd foreach (jpb directory _)
    extraEnv foreach { case (k, v) => jpb.environment.put(k, v) }
    apply(jpb)
  }

  /** Creates a [[scala.sys.process.ProcessBuilder]] from a `java.lang.ProcessBuilder`.
    *
    * @example {{{
    * apply((new java.lang.ProcessBuilder("ls", "-l")) directory new java.io.File(System.getProperty("user.home")))
    * }}}
    */
  def apply(builder: JProcessBuilder): ProcessBuilder = new Simple(builder)

  /** Creates a [[scala.sys.process.ProcessBuilder]] from a `java.io.File`. This
    * `ProcessBuilder` can then be used as a `Source` or a `Sink`, so one can
    * pipe things from and to it.
    */
  def apply(file: File): FileBuilder                  = new FileImpl(file)

  /** Creates a [[scala.sys.process.ProcessBuilder]] from a `java.net.URL`. This
    * `ProcessBuilder` can then be used as a `Source`, so that one can pipe things
    * from it.
    */
  def apply(url: URL): URLBuilder                     = new URLImpl(url)

  /** Creates a [[scala.sys.process.ProcessBuilder]] from a `Boolean`. This can be
    * to force an exit value.
    */
  def apply(value: Boolean): ProcessBuilder           = apply(value.toString, if (value) 0 else 1)

  /** Creates a [[scala.sys.process.ProcessBuilder]] from a `String` name and a
    * `Boolean`. This can be used to force an exit value, with the name being
    * used for `toString`.
    */
  def apply(name: String, exitValue: => Int): ProcessBuilder = new Dummy(name, exitValue)

  /** Creates a sequence of [[scala.sys.process.ProcessBuilder.Source]] from a sequence of
    * something else for which there's an implicit conversion to `Source`.
    */
  def applySeq[T](builders: scala.collection.Seq[T])(implicit convert: T => Source): scala.collection.Seq[Source] = builders.map(convert)

  /** Creates a [[scala.sys.process.ProcessBuilder]] from one or more
    * [[scala.sys.process.ProcessBuilder.Source]], which can then be
    * piped to something else.
    *
    * This will concatenate the output of all sources. For example:
    *
    * {{{
    * import scala.sys.process._
    * import scala.sys.process.Process.cat
    * import java.net.URL
    * import java.io.File
    *
    * val spde = new URL("http://technically.us/spde.html")
    * val dispatch = new URL("https://dispatchhttp.org/Dispatch.html")
    * val build = new File("project/build.properties")
    * cat(spde, dispatch, build) #| "grep -i scala" !
    * }}}
    */
  def cat(file: Source, files: Source*): ProcessBuilder = cat(file +: files)

  /** Creates a [[scala.sys.process.ProcessBuilder]] from a non-empty sequence
    * of [[scala.sys.process.ProcessBuilder.Source]], which can then be
    * piped to something else.
    *
    * This will concatenate the output of all sources.
    */
  def cat(files: scala.collection.Seq[Source]): ProcessBuilder = {
    require(files.nonEmpty)
    files.map(_.cat).reduceLeft(_ #&& _)
  }
}

/** Provide implicit conversions for the factories offered by [[scala.sys.process.Process]]'s
  * companion object. These implicits can then be used to decrease the noise in a pipeline
  * of commands, making it look more shell-like. They are available through the package object
  * [[scala.sys.process]].
  */
trait ProcessImplicits {
  import Process._

  /** Return a sequence of [[scala.sys.process.ProcessBuilder.Source]] from a sequence
    * of values for which an implicit conversion to `Source` is available.
    */
  implicit def buildersToProcess[T](builders: scala.collection.Seq[T])(implicit convert: T => Source): scala.collection.Seq[Source] = applySeq(builders)

  /** Implicitly convert a `java.lang.ProcessBuilder` into a Scala one. */
  implicit def builderToProcess(builder: JProcessBuilder): ProcessBuilder = apply(builder)

  /** Implicitly convert a `java.io.File` into a
    * [[scala.sys.process.ProcessBuilder.FileBuilder]], which can be used as
    * either input or output of a process. For example:
    * {{{
    * import scala.sys.process._
    * "ls" #> new java.io.File("dirContents.txt") !
    * }}}
    */
  implicit def fileToProcess(file: File): FileBuilder                     = apply(file)

  /** Implicitly convert a `java.net.URL` into a
    * [[scala.sys.process.ProcessBuilder.URLBuilder]] , which can be used as
    * input to a process. For example:
    * {{{
    * import scala.sys.process._
    * Seq("xmllint", "--html", "-") #< new java.net.URL("https://www.scala-lang.org") #> new java.io.File("fixed.html") !
    * }}}
    */
  implicit def urlToProcess(url: URL): URLBuilder                         = apply(url)

  /** Implicitly convert a `String` into a [[scala.sys.process.ProcessBuilder]]. */
  implicit def stringToProcess(command: String): ProcessBuilder           = apply(command)

  /** Implicitly convert a sequence of `String` into a
    * [[scala.sys.process.ProcessBuilder]]. The first argument will be taken to
    * be the command to be executed, and the remaining will be its arguments.
    * When using this, arguments may contain spaces.
    */
  implicit def stringSeqToProcess(command: scala.collection.Seq[String]): ProcessBuilder = apply(command)
}
