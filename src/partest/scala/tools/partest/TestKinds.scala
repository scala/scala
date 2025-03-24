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

package scala.tools
package partest

import nest.PathSettings

class TestKinds(pathSettings: PathSettings) {
  val standardKinds = ("pos neg run jvm res scalap specialized instrumented presentation" split "\\s+").toList

  def denotesTestFile(p: Path) = p.isFile && p.hasExtension("scala", "res", "xml")
  def denotesTestDir(p: Path) = kindOf(p) match {
    case "res"  => false
    case _      => p.isDirectory && p.extension == ""
  }

  // 1. neg/t7623.scala                   ->  test/files/neg/t7623.scala
  // 2. test/files/neg/t7623.check        ->  test/files/neg/t7623.scala
  // 3. test/files/jvm/future-spec.scala  ->  test/files/jvm/future-spec
  // 4. test/files/neg/t7623              ->  test/files/neg/t7623.scala
  val testIdentToTestPath = when(p => p.segments.length == 2)(pathSettings.srcDir.resolve(_))(_)
  val checkFileToTestFile = when(p => p.hasExtension("check"))(_.changeExtension("scala"))(_)
  val testFileToTestDir   = when(p => p.hasExtension("scala") && !p.isFile)(p => Path(p.path.stripSuffix(s".${p.extension}")))(_)
  val testDirToTestFile   = when(p => p.hasExtension("") && !p.isDirectory)(_.addExtension("scala"))(_)

  private def when(pred: Path => Boolean)(thenp: Path => Path)(p: Path) = if (pred(p)) thenp(p) else p

  def denotesTestPath(p: Path) = denotesTestDir(p) || denotesTestFile(p)

  def kindOf(p: Path) = p.toAbsolute.segments.takeRight(2).head

  def logOf(p: Path) = p.parent / s"${p.stripExtension}-${kindOf(p)}.log"

  // true if a test path matches the --grep expression.
  private[this] def pathMatchesExpr(path: Path, expr: String) = {
    // Matches the expression if any source file contains the expr,
    // or if the checkfile contains it, or if the filename contains
    // it (the last is case-insensitive.)
    def matches(p: Path) = (
         (p.path.toLowerCase contains expr.toLowerCase)
      || (p.fileContents contains expr)
    )
    def candidates = {
      (path changeExtension "check") +: {
        if (path.isFile) List(path)
        else path.toDirectory.deepList().filter(_.isJavaOrScala).toList
      }
    }

    (candidates exists matches)
  }

  def testsFor(kind: String): (List[Path], List[Path]) = {
    val (ti, others) = (pathSettings.srcDir / kind).toDirectory.list.partition(denotesTestPath)
    val ts = ti.toList
    val names = ts.toSet
    def warnable(p: Path) = ((p.hasExtension("flags") || p.hasExtension("check"))
      && List("scala", "res").forall(x => !names(p.changeExtension(x)))
      && !names(p.parent / p.stripExtension)
    )
    (ts, others.filter(warnable).toList)
  }
  def grepFor(expr: String): List[Path]  = standardTests filter (t => pathMatchesExpr(t, expr))
  def standardTests: List[Path]          = standardKinds flatMap (k => testsFor(k)._1)
  def failedTests: List[Path]            = standardTests filter (p => logOf(p).isFile)
}
