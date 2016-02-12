import sbt._
import sbt.complete._, Parser._, Parsers._

object PartestUtil {
  private case class TestFiles(srcPath: String, globalBase: File, testBase: File) {
    private val testCaseDir = new SimpleFileFilter(f => f.isDirectory && f.listFiles.nonEmpty && !(f.getParentFile / (f.name + ".res")).exists)
    private val testCaseFilter = GlobFilter("*.scala") | GlobFilter("*.java") | GlobFilter("*.res") || testCaseDir
    private def testCaseFinder = (testBase / srcPath).*(AllPassFilter).*(testCaseFilter)
    private val basePaths = allTestCases.map(_._2.split('/').take(3).mkString("/") + "/").distinct

    def allTestCases = testCaseFinder.pair(relativeTo(globalBase))
    def basePathExamples = new FixedSetExamples(basePaths)
    private def equiv(f1: File, f2: File) = f1.getCanonicalFile == f2.getCanonicalFile
    def parentChain(f: File): Iterator[File] =
      if (f == null || !f.exists) Iterator()
      else Iterator(f) ++ (if (f.getParentFile == null) Nil else parentChain(f.getParentFile))
    def isParentOf(parent: File, f2: File, maxDepth: Int) =
      parentChain(f2).take(maxDepth).exists(p1 => equiv(p1, parent))
    def isTestCase(f: File) = {
      val grandParent = if (f != null && f.getParentFile != null) f.getParentFile.getParentFile else null
      grandParent != null && equiv(grandParent, testBase / srcPath) && testCaseFilter.accept(f)
    }
    def mayContainTestCase(f: File) = {
      isParentOf(testBase / srcPath, f, 2) || isParentOf(f, testBase / srcPath, Int.MaxValue)
    }
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
    val TestPathParser = ParserUtil.FileParser(
      new SimpleFileFilter(f => testFiles.isTestCase(f)),
      new SimpleFileFilter(f => testFiles.mayContainTestCase(f)), globalBase)

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
      token(grepOption <~ Space) ~> token(globOrPattern, tokenCompletion)
    }

    val SrcPath = ((token(srcPathOption) <~ Space) ~ token(StringBasic.examples(Set("files", "pending", "scaladoc")))) map {
      case opt ~ path =>
        srcPath = path
        opt + " " + path
    }
    val P = oneOf(knownUnaryOptions.map(x => token(x))) | SrcPath | TestPathParser | Grep
    (Space ~> repsep(P, oneOrMore(Space))).map(_.mkString(" ")).?.map(_.getOrElse("")) <~ OptSpace
  }
}
