import sbt._
import sbt.complete.Parser._
import sbt.complete.Parsers._
import sbt.complete._

object ParserUtil {
  def notStartingWith(parser: Parser[String], c: Char): Parser[String] = parser & not(c ~> any.*, "value cannot start with " + c + ".")
  def concat(p: Parser[(String, String)]): Parser[String] = {
    p.map(x => x._1 + x._2)
  }

  def EitherOr(a: Parser[String], b: Parser[String]): Parser[String] = {
    a.flatMap[String] {
      case "" => b
      case x: String =>
        concat(Space.string ~ b).map[String]((s: String) => x + s)
    }
  }
  def Opt(a: Parser[String]) = a.?.map(_.getOrElse(""))

  val StringBasicNotStartingWithDash = notStartingWith(StringBasic, '-')
  val IsDirectoryFilter = new SimpleFileFilter(_.isDirectory)
  val JarOrDirectoryParser = FileParser(GlobFilter("*.jar") || IsDirectoryFilter)
  def FileParser(filter: FileFilter, dirFilter: FileFilter = AllPassFilter, base: File = file(".")) = {
    def matching(prefix: String): List[String] = {
      val preFile = file(prefix)
      val cwd = base
      val parent = Option(preFile.getParentFile).getOrElse(cwd)
      if (preFile.exists) {
        if (preFile.isDirectory) {
          preFile.*(IsDirectoryFilter.&&(dirFilter) || filter).get.map(_.getPath).toList
        } else {
          List(preFile).filter(filter.accept).map(_.getPath)
        }
      }
      else if (parent != null) {
        def ensureSuffix(s: String, suffix: String) = if (s.endsWith(suffix)) s else s + suffix
        def pathOf(f: File): String = if (f.isDirectory && !filter.accept(f)) ensureSuffix(f.getPath, "/") else f.getPath
        parent.*(GlobFilter(preFile.name + "*") && ((IsDirectoryFilter && dirFilter) || filter)).get.map(x => pathOf(if (parent == cwd) x.relativeTo(cwd).get else x)).toList
      } else Nil
    }
    def displayPath = Completions.single(Completion.displayOnly("<path>"))
    token(StringBasic, TokenCompletions.fixed((seen, level) => if (seen.isEmpty) displayPath else matching(seen) match {
      case Nil => displayPath
      case x :: Nil =>
        if (filter.accept(file(x)))
          Completions.strict(Set(Completion.tokenDisplay(x.stripPrefix(seen), x)))
        else
          Completions.strict(Set(Completion.suggestion(x.stripPrefix(seen))))
      case xs =>
        Completions.strict(xs.map(x => Completion.tokenDisplay(x.stripPrefix(seen), x)).toSet)
    })).filter(!_.startsWith("-"), x => x)
  }
}