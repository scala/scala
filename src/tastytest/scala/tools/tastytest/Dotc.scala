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

import scala.util.{Try, Success, Failure}
import scala.util.control.NonFatal

import scala.reflect.internal.util.ScalaClassLoader
import scala.reflect.runtime.ReflectionUtils
import java.lang.reflect.{Modifier, Method}

import ClasspathOps._
import java.io.OutputStream
import java.io.BufferedReader
import java.io.PrintWriter

object Dotc extends Script.Command {

  final case class ClassLoader private (val parent: ScalaClassLoader)

  def initClassloader(): Try[Dotc.ClassLoader] =
    Try(Dotc.ClassLoader(ScalaClassLoader.fromURLs(Classpaths.dottyCompiler.asURLs)))

  def processIn(op: Dotc.ClassLoader => Int): Int = {
    Dotc.initClassloader() match {
      case Success(cl) => op(cl)
      case Failure(err) =>
        println(red(s"could not initialise Scala 3 classpath: $err"))
        1
    }
  }

  def loadClass(name: String)(implicit cl: Dotc.ClassLoader) =
    Class.forName(name, true, cl.parent)

  def invokeStatic(method: Method, args: Seq[Any])(implicit cl: Dotc.ClassLoader) = {
    assert(Modifier.isStatic(method.getModifiers), s"$method is not static!")
    invoke(method, null, args)
  }

  def invokeStatic(
      className: String,
      methodName: String,
      args: Seq[(Class[_], Any)],
  )(implicit cl: Dotc.ClassLoader): Try[Object] = {
    val cls = loadClass(className)
    val (tpes, provided) = args.unzip
    val method = cls.getMethod(methodName, tpes:_*)
    Try {
      invokeStatic(method, provided)
    }
  }

  def invoke(method: Method, obj: AnyRef, args: Seq[Any])(implicit cl: Dotc.ClassLoader) = {
    inClassloader[AnyRef] {
      method.invoke(obj, args.toArray:_*)
    }
  }

  def inClassloader[T](op: => T)(implicit cl: Dotc.ClassLoader): T = {
    try cl.parent.asContext[T] {
      op
    }
    catch {
      case NonFatal(ex) => throw ReflectionUtils.unwrapThrowable(ex)
    }
  }

  def processMethod(className: String)(args: Seq[String])(implicit cl: Dotc.ClassLoader): Try[Boolean] =
    processMethodImpl(className)(args, None)

  private def makeConsoleReporter(stream: OutputStream)(implicit cl: Dotc.ClassLoader): Try[AnyRef] = Try {
    val consoleReporterCls = loadClass("dotty.tools.dotc.reporting.ConsoleReporter")
    val ctor = consoleReporterCls.getConstructor(
      /* reader: BufferedReader */classOf[BufferedReader],
      /* writer: PrintWriter */classOf[PrintWriter],
      /* echoer: PrintWriter */classOf[PrintWriter] // since 3.5.0-RC2
    )
    val pwriter = new PrintWriter(stream, true)
    inClassloader[AnyRef] {
      ctor.newInstance(/* reader = */Console.in, /* writer = */pwriter, /* echoer= */pwriter)
    }
  }

  private def processMethodImpl(className: String)(args: Seq[String], writer: Option[OutputStream])(implicit cl: Dotc.ClassLoader): Try[Boolean] = {
    val reporterCls = loadClass("dotty.tools.dotc.reporting.Reporter")
    val Reporter_hasErrors = reporterCls.getMethod("hasErrors")
    val processArgs: Try[Seq[(Class[_], Any)]] = {
      writer match {
        case Some(stream) =>
          val callbackCls = loadClass("dotty.tools.dotc.interfaces.CompilerCallback")
          for (myReporter <- makeConsoleReporter(stream)) yield
            Seq(classOf[Array[String]] -> args.toArray, reporterCls -> myReporter, callbackCls -> null)
        case _ =>
          Try(Seq(classOf[Array[String]] -> args.toArray))
      }
    }
    for {
      args <- processArgs
      reporter <- invokeStatic(className, "process", args)
    } yield {
      val hasErrors = invoke(Reporter_hasErrors, reporter, Seq.empty).asInstanceOf[Boolean]
      !hasErrors
    }
  }

  def mainMethod(className: String)(args: Seq[String])(implicit cl: Dotc.ClassLoader): Try[Unit] = {
    val mainArgs = Seq(classOf[Array[String]] -> args.toArray)
    for (_ <- invokeStatic(className, "main", mainArgs)) yield ()
  }

  def dotcVersion(implicit cl: Dotc.ClassLoader): String = {
    val compilerPropertiesClass = loadClass("dotty.tools.dotc.config.Properties")
    val Properties_simpleVersionString = compilerPropertiesClass.getMethod("simpleVersionString")
    invokeStatic(Properties_simpleVersionString, Seq.empty).asInstanceOf[String]
  }

  def dotc(out: String, classpath: String, additionalSettings: Seq[String], sources: String*)(implicit cl: Dotc.ClassLoader): Try[Boolean] =
    dotcImpl(None, out, classpath, additionalSettings, sources:_*)

  def dotc(writer: OutputStream, out: String, classpath: String, additionalSettings: Seq[String], sources: String*)(implicit cl: Dotc.ClassLoader): Try[Boolean] =
    dotcImpl(Some(writer), out, classpath, additionalSettings, sources:_*)

  def dotcImpl(writer: Option[OutputStream], out: String, classpath: String, additionalSettings: Seq[String], sources: String*)(implicit cl: Dotc.ClassLoader): Try[Boolean] = {
    if (sources.isEmpty) {
      Success(true)
    }
    else {
      val libraryDeps = Classpaths.dottyLibrary ++ Classpaths.scalaReflect
      val args = Seq(
        "-d", out,
        "-classpath", libraryDeps.mkString(classpath + Files.classpathSep, Files.classpathSep, ""),
        "-deprecation",
        "-Xfatal-warnings",
        "-color:never",
      ) ++ additionalSettings ++ sources
      if (TastyTest.verbose) {
        println(yellow(s"Invoking dotc (version $dotcVersion) with args: $args"))
      }
      processMethodImpl("dotty.tools.dotc.Main")(args, writer)
    }
  }

  val commandName: String = "dotc"
  val describe: String = s"$commandName <out: Directory> <src: File>"

  def process(args: String*): Int = {
    if (args.length < 2) {
      println(red(s"please provide at least two arguments in sub-command: $describe"))
      return 1
    }
    val Seq(out, src, additional @ _*) = args: @unchecked
    Dotc.processIn { implicit scala3classloader =>
      val success = dotc(out, out, additional, src).get
      if (success) 0 else 1
    }
  }

}
