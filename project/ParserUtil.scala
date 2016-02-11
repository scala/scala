import sbt._
import sbt.complete.Parser._
import sbt.complete.Parsers._
import sbt.complete._

object ParserUtil {
  def notStartingWith(parser: Parser[String], c: Char): Parser[String] = parser & not(c ~> any.*, "value cannot start with " + c + ".")
  def concat(p: Parser[(String, String)]): Parser[String] = {
    p.map(x => x._1 + x._2)
  }

  def Opt(a: Parser[String]) = a.?.map(_.getOrElse(""))

  val StringBasicNotStartingWithDash = notStartingWith(StringBasic, '-')
  val IsDirectoryFilter = new SimpleFileFilter(_.isDirectory)
  val JarOrDirectoryParser = FileParser(GlobFilter("*.jar") || IsDirectoryFilter)
  def FileParser(fileFilter: FileFilter, dirFilter: FileFilter = AllPassFilter, base: File = file(".")) = {
    def matching(prefix: String): List[String] = {
      val preFile = file(prefix)
      val cwd = base
      val parent = Option(preFile.getParentFile).getOrElse(cwd)
      if (preFile.exists) {
        if (preFile.isDirectory) {
          preFile.*(IsDirectoryFilter.&&(dirFilter) || fileFilter).get.map(_.getPath).toList
        } else {
          List(preFile).filter(fileFilter.accept).map(_.getPath)
        }
      }
      else if (parent != null) {
        def ensureSuffix(s: String, suffix: String) = if (s.endsWith(suffix)) s else s + suffix
        def pathOf(f: File): String = {
          val f1 = if (preFile.getParentFile == null) f.relativeTo(cwd).getOrElse(f) else f
          if (f1.isDirectory && !fileFilter.accept(f1)) ensureSuffix(f1.getPath, "/") else f1.getPath
        }
        val childFilter = GlobFilter(preFile.name + "*") && ((IsDirectoryFilter && dirFilter) || fileFilter)
        val children = parent.*(childFilter).get
        children.map(pathOf).toList
      } else Nil
    }
    def displayPath = Completions.single(Completion.displayOnly("<path>"))
    token(StringBasic, TokenCompletions.fixed((seen, level) => if (seen.isEmpty) displayPath else matching(seen) match {
      case Nil => displayPath
      case x :: Nil =>
        if (fileFilter.accept(file(x)))
          Completions.strict(Set(Completion.tokenDisplay(x.stripPrefix(seen), x)))
        else
          Completions.strict(Set(Completion.suggestion(x.stripPrefix(seen))))
      case xs =>
        Completions.strict(xs.map(x => Completion.tokenDisplay(x.stripPrefix(seen), x)).toSet)
    })).filter(!_.startsWith("-"), x => x)
  }
}