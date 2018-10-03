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
        else path.toDirectory.deepList() filter (_.isJavaOrScala) toList
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
