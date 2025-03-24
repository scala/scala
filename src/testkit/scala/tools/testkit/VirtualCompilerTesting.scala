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

package scala.tools.testkit

import java.io.OutputStreamWriter
import java.net.URI
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Locale

import javax.tools._

import scala.jdk.CollectionConverters._
import scala.reflect.internal.util.AbstractFileClassLoader
import scala.reflect.io.{AbstractFile, VirtualDirectory}
import scala.tools.nsc.classpath.{AggregateClassPath, VirtualDirectoryClassPath}
import scala.tools.nsc.{Global, Settings}

/** Utilities for testing with javac/scalac without using the actual filesystem,
  * presumably because one doesn't wish to deal with platform idiosyncrasies.
  */
class VirtualCompiler {
  /** A java compiler instance that we can use. */
  lazy val javac = Option(ToolProvider.getSystemJavaCompiler)
                   .getOrElse(throw new UnsupportedOperationException("No java compiler found in current Java runtime"))

  /** The directory in which are placed classfiles. */
  lazy val output = new VirtualDirectory("out", maybeContainer = None)

  /** A javac file manager that places classfiles in `output`. */
  lazy val fileManager: JavaFileManager = {
    val dflt = javac.getStandardFileManager(null, Locale.ENGLISH, UTF_8)
    new VirtualFileManager(output, dflt)
  }

  /** A scala compiler. */
  lazy val scalac: Global = {
    val settings = new Settings()
    settings.usejavacp.value = true
    settings.outputDirs setSingleOutput output
    new Global(settings) {
      override lazy val platform = new super.GlobalPlatform() {
        override val classPath = AggregateClassPath(List(
          super.classPath,
          VirtualDirectoryClassPath(output),
        ))
      }
    }
  }

  def compileJava(sources: (String, String)*): Unit = {
    val sourcefiles = sources.map {
      case (filename, content) =>
        new InMemorySourcefile(new URI("vc:/" + filename), content)
    }
    val writer = new OutputStreamWriter(System.out)
    assert {
      javac
        .getTask(writer, fileManager, null, null, null, sourcefiles.asJava)
        .call()
    }
  }

  def compileScala(sources: (String, String)*): Unit = {
    val run = new scalac.Run()
    val units = sources.map {
      case (filename, content) => scalac.newCompilationUnit(content, filename)
    }
    run.compileUnits(units.toList, run.parserPhase)
  }

  def classloader: ClassLoader =
    new AbstractFileClassLoader(output, getClass.getClassLoader)
}

final class VirtualFileManager(dir: VirtualDirectory, del: StandardJavaFileManager)
    extends ForwardingJavaFileManager[StandardJavaFileManager](del) {
  import JavaFileManager.Location
  import JavaFileObject.Kind

  override def getJavaFileForOutput(
    loc: Location,
    clasz: String,
    kind: Kind,
    sibling: FileObject,
  ): JavaFileObject = {
    assert(loc == StandardLocation.CLASS_OUTPUT, loc)
    assert(kind == Kind.CLASS, kind)
    val (file, uri) = mkFile(clasz)
    new SimpleJavaFileObject(uri, Kind.CLASS) {
      override def openOutputStream() = file.output
    }
  }

  override def getJavaFileForInput(loc: Location, clasz: String, kind: Kind): JavaFileObject = {
    if (loc == StandardLocation.CLASS_PATH) {
      assert(kind == Kind.CLASS, kind)
      val (file, uri) = mkFile(clasz)
      new SimpleJavaFileObject(uri, Kind.CLASS) {
        override def openInputStream() = file.input
      }
    } else super.getJavaFileForInput(loc, clasz, kind)
  }

  private def mkFile(clasz: String): (AbstractFile, URI) = {
    val parts = clasz.split('.')
    val pkg = parts.init.foldLeft[AbstractFile](dir)(_ subdirectoryNamed _)
    val file = pkg.fileNamed(parts.last + ".class")
    val uri = new URI("vc:/" + parts.mkString("/") + ".class")
    (file, uri)
  }
}


final class InMemorySourcefile(uri: URI, contents: String)
    extends SimpleJavaFileObject(uri, JavaFileObject.Kind.SOURCE) {
  override def getCharContent(ignoreEncodingErrors: Boolean) = contents
}
