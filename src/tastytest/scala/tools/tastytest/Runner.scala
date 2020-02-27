package scala.tools.tastytest

import scala.reflect.internal.util.ScalaClassLoader
import scala.reflect.runtime.ReflectionUtils
import scala.util.Try

import java.nio.file.Paths
import java.io.{ OutputStream, ByteArrayOutputStream }
import java.{ lang => jl, util => ju }
import jl.reflect.Modifier
import scala.util.control.NonFatal
import java.lang.reflect.Method
import java.net.URLClassLoader

import Files._
import java.net.URL

private class Runner(classloader: ScalaClassLoader) {

  val Runner_run: Method = {
    val internal_Runner = Class.forName(Runner.name, true, classloader)
    val run = internal_Runner.getMethod("run", classOf[String], classOf[OutputStream], classOf[OutputStream])
    assert(Modifier.isStatic(run.getModifiers), s"${Runner.name}.run is not static")
    run
  }

  def runRaw(name: String): Try[(String, String)] = {
    def kernel(out: OutputStream, err: OutputStream): Try[Unit] = Try {
      try classloader.asContext[Unit](Runner_run.invoke(null, name, out, err))
      catch {
        case NonFatal(ex) => throw ReflectionUtils.unwrapThrowable(ex)
      }
    }
    val outStream = new ByteArrayOutputStream(50)
    val errStream = new ByteArrayOutputStream(50)
    try {
      val result = kernel(outStream, errStream)
      outStream.flush()
      errStream.flush()
      result.map(_ => outStream.toString -> errStream.toString)
    }
    finally {
      outStream.close()
      errStream.close()
    }
  }
}

private object Runner {

  val name = "scala.tools.tastytest.internal.Runner"

  def getSystemClasspath: Array[URL] = ClassLoader.getSystemClassLoader().asInstanceOf[URLClassLoader].getURLs

  def classloadFrom(classpath: String): Try[Runner] = for {
    classpaths  <- Try(classpath.split(":").filter(_.nonEmpty).map(Paths.get(_).toUri.toURL))
    classloader <- Try(ScalaClassLoader.fromURLs(classpaths.toIndexedSeq ++ getSystemClasspath))
    runner      <- Try(new Runner(classloader))
  } yield runner

  def main(args: Array[String]): Unit = {
    val Array(classpath, className) = args
    (for {
      runner     <- classloadFrom(classpath)
      (out, err) <- runner.runRaw(className)
    } yield {
      if (err.nonEmpty) println(s"$className completed with\n  output: $out\n  error: $err")
      else println(s"$className completed with\n  output: $out")
    }).get
  }

}
