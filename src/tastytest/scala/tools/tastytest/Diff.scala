package scala.tools.tastytest

import scala.jdk.CollectionConverters._

object Diff {
  def splitIntoLines(string: String): Seq[String] =
    string.trim.replace("\r\n", "\n").split("\n").toSeq

  def compareContents(check: String, output: String): String =
    compareContents(splitIntoLines(check), splitIntoLines(output))

  def compareContents(check: Seq[String], output: Seq[String]): String = {
    val diff = difflib.DiffUtils.diff(check.asJava, output.asJava)
    if (diff.getDeltas.isEmpty)
      ""
    else
      difflib.DiffUtils
        .generateUnifiedDiff(
          "output",
          "check",
          check.asJava,
          diff,
          1
        )
        .asScala
        .mkString("\n")
  }
}
