import sbt._
import sbt.complete._, Parser._, Parsers._

object PartestUtil {
  private case class TestFiles(srcPath: String, globalBase: File, testBase: File) {
    private val testCaseDir = new SimpleFileFilter(f => f.isDirectory && f.listFiles.nonEmpty && !(f.getParentFile / (f.name + ".res")).exists)
    private def testCaseFinder = (testBase / srcPath).*(AllPassFilter).*(GlobFilter("*.scala") | GlobFilter("*.java") | GlobFilter("*.res") || testCaseDir)
    private val basePaths = allTestCases.map(_._2.split('/').take(3).mkString("/") + "/").distinct

    def allTestCases = testCaseFinder.pair(relativeTo(globalBase))
    def basePathExamples = new FixedSetExamples(basePaths)
  }
  /** A parser for the custom `partest` command */
  def partestParser(globalBase: File, testBase: File): Parser[String] = {
    val knownUnaryOptions = List(
      "--pos", "--neg", "--run", "--jvm", "--res", "--ant", "--scalap", "--specialized",
      "--scalacheck", "--instrumented", "--presentation", "--failed", "--update-check",
      "--show-diff", "--verbose", "--terse", "--debug", "--version", "--self-test", "--help")
    val srcPathOption = "--srcpath"
    val grepOption = "--grep"

    // HACK: if we parse `--srpath scaladoc`, we overwrite this var. The parser for test file paths
    // then lazily creates the examples based on the current value.
    // TODO is there a cleaner way to do this with SBT's parser infrastructure?
    var srcPath = "files"
    var _testFiles: TestFiles = null
    def testFiles = {
      if (_testFiles == null || _testFiles.srcPath != srcPath) _testFiles = new TestFiles(srcPath, globalBase, testBase)
      _testFiles
    }
    val TestPathParser = token(NotSpace & not('-' ~> any.*, "File name cannot start with '-'."), TokenCompletions.fixed({
      (seen, level) =>
        val suggestions = testFiles.allTestCases.map(_._2).filter(_.startsWith(seen)).map(_.stripPrefix(seen))
        Completions.strict(suggestions.map(s => Completion.token(seen, s)).toSet)
    }))

    // allow `--grep "is unchecked" | --grep *t123*, in the spirit of ./bin/partest-ack
    // superset of the --grep built into partest itself.
    val Grep = {
      def expandGrep(x: String): Seq[String] = {
        val matchingFileContent = try {
          val Pattern = ("(?i)" + x).r
          testFiles.allTestCases.filter {
            case (testFile, testPath) =>
              val assocFiles = List(".check", ".flags").map(testFile.getParentFile / _)
              val sourceFiles = if (testFile.isFile) List(testFile) else testFile.**(AllPassFilter).get.toList
              val allFiles = testFile :: assocFiles ::: sourceFiles
              allFiles.exists { f => f.exists && f.isFile && Pattern.findFirstIn(IO.read(f)).isDefined }
          }
        } catch {
          case _: Throwable => Nil
        }
        val matchingFileName = try {
          val filter = GlobFilter("*" + x + "*")
          testFiles.allTestCases.filter(x => filter.accept(x._1.name))
        } catch {
          case t: Throwable => Nil
        }
        (matchingFileContent ++ matchingFileName).map(_._2).distinct.sorted
      }

      val completion = Completions.strict(Set("<filename glob>", "<regex> (for source, flags or checkfile contents)").map(s => Completion.displayOnly(s)))
      val tokenCompletion = TokenCompletions.fixed((seen, level) => completion)

      val globOrPattern = StringBasic.map(expandGrep).flatMap {
        case Seq() => failure("no tests match pattern / glob")
        case x => success(x.mkString(" "))
      }
      ((token(grepOption <~ Space)) ~> token(globOrPattern, tokenCompletion))
    }

    val SrcPath = ((token(srcPathOption) <~ Space) ~ token(StringBasic.examples(Set("files", "pending", "scaladoc")))) map {
      case opt ~ path =>
        srcPath = path
        opt + " " + path
    }
    // allow the user to additional abitrary arguments, in case our parser is incomplete.
    val WhatEver = token(NotSpace, _ => true).filter(x => !knownUnaryOptions.contains(x) && !Set(grepOption, srcPathOption).contains(x), x => x)
    val P = oneOf(knownUnaryOptions.map(x => token(x))) | SrcPath | TestPathParser | Grep //| WhatEver
    (Space ~> repsep(P, oneOrMore(Space))).map(_.mkString(" "))
  }
}
