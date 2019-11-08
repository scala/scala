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

import Files._

private class Runner(classloader: ScalaClassLoader) {

  val Runner_run: Method = {
    val internal_Runner = Class.forName(Runner.name, true, classloader)
    val run = internal_Runner.getMethod("run", classOf[String], classOf[OutputStream], classOf[OutputStream])
    assert(Modifier.isStatic(run.getModifiers), s"${Runner.name}.run is not static")
    run
  }

  def run(name: String): Try[String] = {
    def kernel(out: OutputStream, err: OutputStream): Try[Unit] = Try {
      try classloader.asContext[Unit](Runner_run.invoke(null, name, out, err))
      catch {
        case NonFatal(ex) => throw ReflectionUtils.unwrapThrowable(ex)
      }
    }
    val byteArrayStream = new ByteArrayOutputStream(50)
    try {
      val result = kernel(byteArrayStream, byteArrayStream)
      byteArrayStream.flush()
      result.map(_ => byteArrayStream.toString)
    }
    finally {
      byteArrayStream.close()
    }
  }
}

private object Runner {

  val name = "scala.tools.tastytest.internal.Runner"

  private def internalRunnerPath: Try[java.net.URL] = Try {
    internal.Runner.getClass.getProtectionDomain.getCodeSource.getLocation
  }

  def classloadFrom(classpath: String): Try[Runner] = for {
    internalRunner <- internalRunnerPath
    classpaths     <- Try(classpath.split(":").filter(_.nonEmpty).map(Paths.get(_).toUri.toURL))
    classloader    <- Try(ScalaClassLoader.fromURLs(internalRunner +: classpaths.toIndexedSeq))
    runner         <- Try(new Runner(classloader))
  } yield runner
}
