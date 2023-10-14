package scala.build

import sbt._
import sbt.complete.Parser._
import sbt.complete.Parsers._
import sbt.complete._

object ParserUtil {
  def notStartingWith(parser: Parser[String], c: Char): Parser[String] = parser & not(c ~> any.*, s"value cannot start with $c.")
  def concat(p: Parser[(String, String)]): Parser[String] = p.map { case (a, b) => a + b }
  def Opt(a: Parser[String]) = a.?.map(_.getOrElse(""))

  val StringBasicNotStartingWithDash = notStartingWith(StringBasic, '-')
  val IsDirectoryFilter              = new SimpleFileFilter(_.isDirectory)
  val JarOrDirectoryParser           = FileParser(GlobFilter("*.jar") || IsDirectoryFilter)

  def FileParser(fileFilter: FileFilter, dirFilter: FileFilter = AllPassFilter, base: File = file(".")) = {
    val childFilter = IsDirectoryFilter && dirFilter || fileFilter
    def ensureSuffix(s: String, suffix: String) = if (s.endsWith(suffix)) s else s"$s$suffix"
    def matching(prefix: String): List[String] = {
      val prefixFile            = new File(prefix)
      val prefixIsAbsolute      = prefixFile.isAbsolute
      val preFile               = if (prefixIsAbsolute) prefixFile else new File(base, prefix)
      val basePrefix            = if (prefixIsAbsolute) "" else ensureSuffix(base.getPath, "/")
      def relativize(p: String) = p.stripPrefix(basePrefix)
      def pathOf(f: File)       = if (f.isDirectory && !fileFilter.accept(f)) ensureSuffix(f.getPath, "/") else f.getPath
      val finder = if (preFile.isDirectory) {
        preFile.glob(childFilter)
      } else if (preFile.exists()) {
        PathFinder(preFile).filter(fileFilter.accept)
      } else {
        preFile.getParentFile.glob(GlobFilter(s"${preFile.getName}*") && childFilter)
      }
      finder.get().toList.map(pathOf).map(relativize)
    }
    def displayPath = Completions.single(Completion.displayOnly("<path>"))
    token(StringBasic, TokenCompletions.fixed((prefix, _) => if (prefix.isEmpty) displayPath else matching(prefix) match {
      case Nil => displayPath
      case xs  => Completions.strict(xs.map(x => Completion.tokenDisplay(x.stripPrefix(prefix), x)).toSet)
    })).filter(!_.startsWith("-"), x => x)
  }
}
