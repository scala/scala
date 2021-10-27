package scala.tools.tastytest

import scala.util.{Try, Success, Failure}
import scala.util.control.NonFatal

import scala.reflect.internal.util.ScalaClassLoader
import scala.reflect.runtime.ReflectionUtils
import java.lang.reflect.{Modifier, Method}

import ClasspathOps._

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
      args: Seq[String]
  )(implicit cl: Dotc.ClassLoader): Try[Object] = {
    val cls = loadClass(className)
    val method = cls.getMethod(methodName, classOf[Array[String]])
    Try {
      invokeStatic(method, Seq(args.toArray))
    }
  }

  def invoke(method: Method, obj: AnyRef, args: Seq[Any])(implicit cl: Dotc.ClassLoader) = {
    try cl.parent.asContext[AnyRef] {
      method.invoke(obj, args.toArray:_*)
    }
    catch {
      case NonFatal(ex) => throw ReflectionUtils.unwrapThrowable(ex)
    }
  }

  private def dotcProcess(args: Seq[String])(implicit cl: Dotc.ClassLoader) = processMethod("dotty.tools.dotc.Main")(args)

  def processMethod(className: String)(args: Seq[String])(implicit cl: Dotc.ClassLoader): Try[Boolean] = {
    val reporterCls = loadClass("dotty.tools.dotc.reporting.Reporter")
    val Reporter_hasErrors = reporterCls.getMethod("hasErrors")
    for (reporter <- invokeStatic(className, "process", args)) yield {
      val hasErrors = invoke(Reporter_hasErrors, reporter, Seq.empty).asInstanceOf[Boolean]
      !hasErrors
    }
  }

  def mainMethod(className: String)(args: Seq[String])(implicit cl: Dotc.ClassLoader): Try[Unit] =
    for (_ <- invokeStatic(className, "main", args)) yield ()

  def dotcVersion(implicit cl: Dotc.ClassLoader): String = {
    val compilerPropertiesClass = loadClass("dotty.tools.dotc.config.Properties")
    val Properties_simpleVersionString = compilerPropertiesClass.getMethod("simpleVersionString")
    invokeStatic(Properties_simpleVersionString, Seq.empty).asInstanceOf[String]
  }

  def dotc(out: String, classpath: String, additionalSettings: Seq[String], sources: String*)(implicit cl: Dotc.ClassLoader): Try[Boolean] = {
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
      ) ++ additionalSettings ++ sources
      if (TastyTest.verbose) {
        println(yellow(s"Invoking dotc (version $dotcVersion) with args: $args"))
      }
      dotcProcess(args)
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
