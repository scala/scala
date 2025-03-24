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

package scala.tools.partest

trait StubErrorMessageTest extends StoreReporterDirectTest {
  // Stub to feed to partest, unused
  def code = throw new Error("Use `userCode` instead of `code`.")

  val classpath = List(sys.props("partest.lib"), testOutput.path)
    .mkString(sys.props("path.separator"))

  def compileCode(codes: String*) = {
    val global = newCompiler("-cp", classpath, "-d", testOutput.path)
    val sourceFiles = newSources(codes: _*)
    withRun(global)(_ compileSources sourceFiles)
  }

  def removeClasses(inPackage: String, classNames: Seq[String]): Unit = {
    val pkg = new File(testOutput.path, inPackage)
    classNames.foreach { className =>
      val classFile = new File(pkg, s"$className.class")
      assert(classFile.exists)
      assert(classFile.delete())
    }
  }

  def removeFromClasspath(): Unit
  def codeA: String
  def codeB: String
  def userCode: String
  def extraUserCode: String = ""

  def show(): Unit = {
    compileCode(codeA)
    assert(filteredInfos.isEmpty, filteredInfos)

    compileCode(codeB)
    assert(filteredInfos.isEmpty, filteredInfos)
    removeFromClasspath()

    if (extraUserCode == "") compileCode(userCode)
    else compileCode(userCode, extraUserCode)
    import scala.reflect.internal.util.Position
    filteredInfos.foreach { report =>
      print(if (report.severity == storeReporter.ERROR) "error: " else "")
      println(Position.formatMessage(report.pos, report.msg, shortenFile = true))
    }
  }
}
