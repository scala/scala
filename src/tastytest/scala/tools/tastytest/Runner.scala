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

package scala.tools.tastytest

import scala.reflect.internal.util.ScalaClassLoader
import scala.reflect.runtime.ReflectionUtils
import scala.util.Try

import java.nio.file.Paths
import java.io.{ OutputStream, ByteArrayOutputStream }
import java.{ lang => jl }
import jl.reflect.Modifier
import scala.util.control.NonFatal
import java.lang.reflect.Method

import Files._
import java.net.URL

class Runner private (classloader: ScalaClassLoader) {

  val Runner_run: Method = {
    val internal_Runner = Class.forName(Runner.name, true, classloader)
    val run = internal_Runner.getMethod("run", classOf[String], classOf[OutputStream], classOf[OutputStream])
    assert(Modifier.isStatic(run.getModifiers), s"${Runner.name}.run is not static")
    run
  }

  def runCaptured(name: String): Try[String] = {
    def kernel(out: OutputStream, err: OutputStream): Try[Unit] = Try {
      try classloader.asContext[Unit](Runner_run.invoke(null, name, out, err))
      catch {
        case NonFatal(ex) => throw ReflectionUtils.unwrapThrowable(ex)
      }
    }
    val outStream = new ByteArrayOutputStream(50)
    try {
      val result = kernel(outStream, outStream)
      outStream.flush()
      result.map(_ => outStream.toString)
    }
    finally outStream.close()
  }
}

object Runner extends Script.Command {

  private val name = "scala.tools.tastytest.internal.Runner"

  private def currentClasspath: Try[Seq[URL]] = splitClasspath(System.getProperty("java.class.path"))

  private def splitClasspath(classpath: String): Try[Seq[URL]] =
    Try(classpath.split(classpathSep).filter(_.nonEmpty).map(Paths.get(_).toUri.toURL).toIndexedSeq)

  def classloadFrom(classpath: String): Try[ScalaClassLoader] = for {
    classpaths  <- splitClasspath(classpath)
    current     <- currentClasspath
    classloader <- Try(ScalaClassLoader.fromURLs(current ++ classpaths))
  } yield classloader

  def run(classloader: ScalaClassLoader, name: String): Unit = {
    try {
      val objClass = Class.forName(name, true, classloader)
      val main     = objClass.getMethod("main", classOf[Array[String]])
      if (!Modifier.isStatic(main.getModifiers))
        throw new NoSuchMethodException(name + ".main is not static")
      classloader.asContext[Unit](main.invoke(null, Array.empty[String]))
    }
    catch {
      case NonFatal(ex) => throw ReflectionUtils.unwrapThrowable(ex)
    }
  }

  def capturingRunner(classloader: ScalaClassLoader): Try[Runner] = Try(new Runner(classloader))

  val commandName: String = "runDotty"
  val describe: String = s"$commandName <classpath: Paths> <classname: String>"

  def process(args: String*): Int = {
    if (args.length != 2) {
      println(red(s"please provide 2 arguments in sub-command: $describe"))
      return 1
    }
    val Seq(classpath, className) = args: @unchecked
    classloadFrom(classpath).map(run(_, className)).get
    0
  }

}
