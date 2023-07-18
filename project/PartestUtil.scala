package scala.build

import sbt._
import sbt.complete._, Parser._, Parsers._

import ParserUtil.Opt

object PartestUtil {
  private case class TestFiles(srcPath: String, globalBase: File, testBase: File) {
    val srcDir = testBase / srcPath // mirror of partest.nest.PathSettings#srcDir

    private val testCaseFile   = GlobFilter("*.scala") | GlobFilter("*.java") | GlobFilter("*.res")
    private val testCaseDir    = new SimpleFileFilter(f => f.isDirectory && f.listFiles().nonEmpty && !(f.getParentFile / (f.getName + ".res")).exists())
    private val testCaseFilter = testCaseFile || testCaseDir
    private val testCaseFinder = srcDir * AllPassFilter * testCaseFilter

    def allTestCases = testCaseFinder.pair(Path.relativeTo(globalBase))

    def parentChain(f: File): Iterator[File] =
      if (f == null || !f.exists()) Iterator.empty
      else Iterator.single(f) ++ Option(f.getParentFile).iterator.flatMap(parentChain)

    def isParentOf(parent: File, f2: File, maxDepth: Int) =
      parentChain(f2).take(maxDepth).exists(equivCanon(_, parent))

    def isTestCase(f: File)            = testCaseFilter.accept(f) && parentChain(f).slice(2, 3).exists(equivCanon(_, srcDir))
    def mayContainTestCase(f: File)    = isParentOf(srcDir, f, 2) || isParentOf(f, srcDir, Int.MaxValue)
    def equivCanon(f1: File, f2: File) = f1.getCanonicalFile == f2.getCanonicalFile
  }

  def testFilePaths(globalBase: File, testBase: File): Seq[File] =
    TestFiles("files", globalBase, testBase).allTestCases.map(_._1)

  /** A parser for the custom `partest` command */
  def partestParser(globalBase: File, testBase: File): Parser[String] = {
    val knownUnaryOptions = List(
      "--pos", "--neg", "--run", "--jvm", "--res", "--ant", "--scalap", "--specialized",
      "--instrumented", "--presentation", "--failed", "--update-check", "--no-exec",
      "--show-diff", "--show-log", "--verbose", "--terse", "--debug", "--realeasy", "--branch", "--version",
      "--help")
    val srcPathOption = "--srcpath"
    val compilerPathOption = "--compilerpath"
    val grepOption = "--grep"

    // HACK: if we parse `--srcpath scaladoc`, we overwrite this var. The parser for test file paths
    // then lazily creates the examples based on the current value.
    // TODO is there a cleaner way to do this with sbt's parser infrastructure?
    var testFiles = TestFiles("files", globalBase, testBase)
    def mkTestPathParser(base: File) = ParserUtil.FileParser(
      new SimpleFileFilter(f => testFiles.isTestCase(f)),
      new SimpleFileFilter(f => testFiles.mayContainTestCase(f)),
      base,
    )
    val TestPath     = mkTestPathParser(globalBase)
    val KindTestPath = mkTestPathParser(testFiles.srcDir)

    // allow `--grep "is unchecked" | --grep *t123*, in the spirit of ./bin/partest-ack
    // superset of the --grep built into partest itself.
    val Grep = {
      def expandGrep(x: String): Seq[String] = {
        val matchingFileContent = try {
          import scala.util.matching.Regex
          val re = raw"(?i)${Regex.quote(x)}".r
          testFiles.allTestCases.filter {
            case (testFile, testPath) =>
              def sibling(suffix: String) = {
                val name = testFile.name
                val prefix = name.lastIndexOf('.') match {
                  case -1 => name
                  case i  => name.substring(0, i)
                }
                val next = prefix + suffix
                testFile.getParentFile / next
              }
              val assocFiles = List(".check", ".flags").map(sibling)
              val sourceFiles = if (testFile.isFile) List(testFile) else testFile.**(AllPassFilter).get.toList
              val allFiles = testFile :: assocFiles ::: sourceFiles
              allFiles.exists(f => f.isFile && re.findFirstIn(IO.read(f)).isDefined)
          }
        } catch {
          case _: Throwable => Nil
        }
        val matchingFileName = try {
          val filter = GlobFilter("*" + x + "*")
          testFiles.allTestCases.filter(x => filter.accept(x._1.asFile.getPath))
        } catch {
          case _: Throwable => Nil
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

    val SrcPath = ((token(srcPathOption) <~ Space) ~ token(StringBasic.examples(Set("files", "scaladoc", "async")))) map {
      case opt ~ path =>
        testFiles = TestFiles(path, globalBase, testBase)
        opt + " " + path
    }

    val CompilerPath = ((token(compilerPathOption) <~ Space) ~ token(NotSpace)) map {
      case opt ~ path =>
        opt + " " + path
    }

    val ScalacOpts = (token("-Dpartest.scalac_opts=") ~ token(NotSpace)) map { case opt ~ v => opt + v }

    val P = oneOf(knownUnaryOptions.map(x => token(x))) | SrcPath | CompilerPath | TestPath | KindTestPath | Grep | ScalacOpts
    Opt((Space ~> repsep(P, oneOrMore(Space))).map(_.mkString(" ")))
  }
}
