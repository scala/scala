package scala.tools
package partest

import nest.PathSettings.srcDir

object TestKinds {
  val standardKinds = ("pos neg run jvm res scalacheck scalap specialized instrumented presentation" split "\\s+").toList

  def denotesTestFile(p: Path) = p.isFile && p.hasExtension("scala", "res", "xml")
  def denotesTestDir(p: Path) = kindOf(p) match {
    case "res"  => false
    case _      => p.isDirectory && p.extension == ""
  }
  def denotesTestPath(p: Path) = denotesTestDir(p) || denotesTestFile(p)

  // TODO
  def isTestForPartest(p: Path) = (
       (p.name == "intentional-failure.scala")
    || (p.path contains "test-for-partest")
  )

  def kindOf(p: Path) = {
    p.toAbsolute.segments takeRight 2 head

    // (srcDir relativize p.toCanonical).segments match {
    //   case (".." :: "scaladoc" :: xs) => xs.head
    //   case xs => xs.head
    // }
  }
  def logOf(p: Path) = {
    p.parent / s"${p.stripExtension}-${kindOf(p)}.log"
    // p.parent / s"${p.stripExtension}.log"
  }

  // true if a test path matches the --grep expression.
  private def pathMatchesExpr(path: Path, expr: String) = {
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
        else path.toDirectory.deepList() filter (_.isJavaOrScala) toList
      }
    }

    (candidates exists matches)
  }

  def groupedTests(paths: List[Path]): List[(String, List[Path])] =
    (paths.distinct groupBy kindOf).toList sortBy (standardKinds indexOf _._1)

  /** Includes tests for testing partest. */
  private def allTestsForKind(kind: String): List[Path] =
    (srcDir / kind toDirectory).list.toList filter denotesTestPath

  def testsForPartest: List[Path]        = standardKinds flatMap allTestsForKind filter isTestForPartest
  def testsFor(kind: String): List[Path] = allTestsForKind(kind) filterNot isTestForPartest
  def grepFor(expr: String): List[Path]  = standardTests filter (t => pathMatchesExpr(t, expr))
  def standardTests: List[Path]          = standardKinds flatMap testsFor
  def failedTests: List[Path]            = standardTests filter (p => logOf(p).isFile)
}
